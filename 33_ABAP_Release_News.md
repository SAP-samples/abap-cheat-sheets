<a name="top"></a>

# ABAP Release News

- [ABAP Release News](#abap-release-news)
  - [ABAP for Cloud Development Documentation Release News](#abap-for-cloud-development-documentation-release-news)
  - [Standard ABAP Documentation Release News](#standard-abap-documentation-release-news)

This ABAP cheat sheet summarizes the release news from the ABAP Keyword Documentation for both versions of the ABAP language: ABAP for Cloud Development and Standard ABAP. It organizes the information by release and topic, such as ABAP SQL and ABAP CDS. This cheat sheet is intended to collect ABAP news, enabling quick browsing and research on new features and changes by release and topic.

> [!NOTE]  
>
> - ABAP releases are identified by numbers such as 7.58, 7.95, 7.96, 9.12, and so on. These numbers correspond to the technical release names of the *SAP_BASIS* software component. ABAP is not released independently; it is included with SAP products like the SAP BTP ABAP environment, SAP S/4HANA Cloud Public Edition and SAP S/4HANA, SAP S/4HANA Cloud Private Edition. Each of these products has its own release numbers that correspond to the ABAP releases. The releases of the underlying ABAP kernel are counted differently but can also be mapped to the ABAP releases. The numbers in brackets describe quarterly releases.
> - The content is presented in a simplified, plain text format. You can also check out the demo HTML file mentioned below.  
> - For a complete overview and links to related topics, and since **the content may not reflect the latest releases or news**, refer to the release news in the ABAP Keyword Documentation here:
[ABAP for Cloud Development](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abennews.htm) / [Standard ABAP (latest)](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews.htm)
> - The ABAP Keyword Documentation provides detailed feature summaries on these topics:
>   - [ABAP CDS](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abencds_language_elements.html)
>   - [RAP BDL (Behavior Definition Language)](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/ABENRAP_FEATURE_TABLE.html)

> [!TIP]
>
> - As an addition and [excursion](./README.md#%EF%B8%8F-disclaimer) to the content of this ABAP cheat sheet, the following HTML file serves as an *ABAP Release News Viewer* - a simplified HTML document that allows for filtering and searching ABAP release news.  
> - This HTML file includes excerpts of ABAP release news from the cheat sheet, formatted as JSON, which is used to construct a table.
> - To explore it, download the file to your local machine and open it in a browser.
> - 📄 [abap_release_news_viewer.html](./files/abap_release_news_viewer.html)
> - The news content is presented in plain text and is unformatted for simplicity.
> - The HTML file uses SAPUI5 and provides filtering and search options. This file is intended for [exploration, experimentation, and demonstration](./README.md#%EF%B8%8F-disclaimer). It does not represent best practices or role model approaches. For more information about SAPUI5, refer to the [UI5 Demo Kit](https://sapui5.hana.ondemand.com/sdk/#/).

<p align="right"><a href="#top">⬆️ back to top</a></p>

## ABAP for Cloud Development Documentation Release News

<details>
  <summary>🟢 Release 916 (2508)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="9">ABAP_CDS</td>
            <td>Stricter Rules for Association ON Conditions in CDS Custom Entities</td>
            <td>Stricter rules for the association ON condition for CDS custom entities have been implemented. For details, see CDS DDL - DEFINE CUSTOM ENTITY, association. The same rules also apply if other CDS entities define associations with a CDS custom entity as an association target. </td>
        </tr>
<tr>
            <td>New Element Annotations</td>
            <td>The following ABAP annotations are now supported as element annotations with the annotation scope #ELEMENT. AbapCatalog.typeSpec.conversionExit AbapCatalog.typeSpec.outputLength </td>
        </tr>
<tr>
            <td>CDS Table Entity, Table Buffering</td>
            <td>Table buffering can be defined for CDS table entities now: Table buffering can be enabled and disabled with the annotation @AbapCatalog.entityBuffer.definitionAllowed: true|false The buffering type can be specified with a CDS table entity buffer defined by DEFINE TABLE ENTITY BUFFER ON cds_table_entity ... Table buffering can be defined differently for the layers core, localization, industry, partner and customer. </td>
        </tr>
<tr>
            <td>Static External Entities</td>
            <td>With the first release of CDS external entities, only dynamic external entities were available. Now, static external entities are also available. They are defined using the addition PROVIDED BY logical_external_schema in the data definition. </td>
        </tr>
<tr>
            <td>CDS Static Caches</td>
            <td>Static caches are now available in ABAP CDS. These can lead to performance gains when aggregated data is cached. </td>
        </tr>
<tr>
            <td>RAP Change Documents</td>
            <td>RAP change documents are now available. They can be used to log changes of persisted RAP BO data in a change document object. </td>
        </tr>
<tr>
            <td>RAP Copy Action</td>
            <td>A new RAP hierarchy action is available that can be used in editable treeview scenarios: the RAP copy action. It is defined using the syntax addition copy action. </td>
        </tr>
<tr>
            <td>Dedicated Authorizations for Create-by-Association Operations</td>
            <td>RAP authorization dependent entities use the authorization control that is defined on the related authorization master entity. For RAP create-by-association operations on an authorization-dependent entity, the system applies the authorization check for updates of the authorization master entity by default. For create by association on CDS to-child associations, you can now deviate from this default and specify separate authorizations for create-by-association operations. </td>
        </tr>
<tr>
            <td>RAP Collaborative Draft</td>
            <td>A new RAP draft handling with collaborative capabilities is now available. The RAP collaborative draft allows multiple users to work concurrently on the same draft instance. It is defined using the syntax with collaborative draft. </td>
        </tr>
<tr>
            <td rowspan="7">ABAP_SQL</td>
            <td>Non-numeric Results in Aggregate Functions</td>
            <td>The result of the aggregate functions MIN and MAX can now be non-numeric for window expressions and hierarchy aggregate navigators. </td>
        </tr>
<tr>
            <td>Aggregate Functions in Hierarchy Aggregate Navigators</td>
            <td>The aggregate function AVG can now be used for the hierarchy aggregate navigator HIERARCHY_DESCENDANTS_AGGREGATE if a JOIN condition is not defined and for the hierarchy aggregate navigator HIERARCHY_ANCESTORS_AGGREGATE. </td>
        </tr>
<tr>
            <td>New Spatial Functions</td>
            <td>The following spatial functions are now available: ST_3DLENGTH ST_ASMVTGEOM ST_BUFFER ST_CLOSESTPOINTOFAPPROACH ST_FRECHETDISTANCE ST_INTERIORRINGN ST_ISVALIDTRAJECTORY ST_MAKEPOLYGON ST_NEW_POINT ST_NEW_POINTM ST_NEW_POINTZ ST_NEW_POINTZM ST_REMOVEPOINT ST_REVERSE ST_TOUCHES ST_WITHIN </td>
        </tr>
<tr>
            <td>DISTINCT in OVER Clause</td>
            <td>The addition DISTINCT is now allowed for the aggregate function COUNT in a window expression. </td>
        </tr>
<tr>
            <td>Enhancement of LPAD and RPAD Functions</td>
            <td>The length len of the LPAD and RPAD functions can now be a host variable or an SQL expression. Additionally, the length of len can be less than 1 and greater than 1333. Both functions can produce the result type SSTRING with an undefined length. </td>
        </tr>
<tr>
            <td>New Aggregate Spatial Functions</td>
            <td>The following aggregate spatial functions are now available: ST_ALPHASHAPEAGGR ST_ALPHASHAPEAREAAGGR ST_ASESRIJSON_MULTICOLUMN ST_ASGEOJSON_MULTICOLUMN ST_ASMVT ST_ASSVGAGGR ST_COLLECTAGGR ST_CONCAVEHULLAGGR ST_CONVEXHULLAGGR ST_ENVELOPEAGGR ST_INTERSECTIONAGGR ST_UNIONAGGR </td>
        </tr>
<tr>
            <td>New SQL Functions GREATEST and LEAST</td>
            <td>The SQL functions GREATEST and LEAST are now available. These functions determine the largest and smallest value of a set of arguments. </td>
        </tr>
<tr>
            <td rowspan="1">EML</td>
            <td>Retry Feature for RAP Local Event Executions</td>
            <td>An automatic retry feature is now available to ensure RAP local events execute in case of temporary issues like locks on RAP BO instances. The framework will attempt to retry up to three times. You can design your implementation for the framework to automatically retry event execution after a specified time. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Arithmetic Functions for Statistics and Probability Theory in ABAP</td>
            <td>The following arithmetic functions are now available: ERF, ERFC, and ERF_INV for calculations with floating point numbers and decimal floating points numbers implement the error function, the complementary error function and its inverses. GAMMA and LOG_GAMMA implement the gamma and log gamma functions for calculations with floating point numbers and decimal floating points numbers. GAMMA_LOWER implements the lower incomplete gamma function and its inverse for calculations with floating point numbers and decimal floating points numbers. </td>
        </tr>
<tr>
            <td rowspan="2">SYSTEM_CLASSES</td>
            <td>System Classes for Probability Distribution</td>
            <td>Two new system classes for probability distribution are available: CL_ABAP_PROB_DISTRIBUTION defines probability distributions in ABAP for probabilities of a floating point data type. CL_ABAP_PROB_DISTRIBUTION_DF34 defines probability distributions in ABAP for probabilities of the built-in data type decfloat34. Both classes allow the generation of random numbers and calculation of quantile, probabilities, and other aspects. The first release includes the following distributions: normal, log-normal, gamma, exponential, and uniform int/int8 distributions. Discrete and continuous distributions are supported. </td>
        </tr>
<tr>
            <td>Mathematical Constants</td>
            <td>The following constants for minimum and maximum values are now available as part of class CL_ABAP_MATH: Minimal positive and not subnormal value for type decfloat34 and decfloat16 Maximal value for type decfloat34 and decfloat16 Smallest floating point number &gt; 0 Difference between smallest floating point number &gt; 1 and 1 In addition, the following common mathematical constants are available in class CL_ABAP_MATH: Euler's constant e pi pi squared pi / 2 pi / 4 Square root of 2, 3, and pi Decimal logarithm (log10) and binary logarithm (log2) of e Decimal logarithm of the numbers 2 and 10 These constants are available as floating point numbers (f) and as decfloat34 numbers. </td>
        </tr>
<tr>
            <td rowspan="1">TRANSFORMATIONS</td>
            <td>New Addition JSON for CALL TRANSFORMATION</td>
            <td>The new additions SOURCE JSON and RESULT JSON are now available. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 915 (2505)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>CDS Delegated Buffers</td>
            <td>CDS delegated buffers can now be defined for CDS view entities using the syntax DEFINE VIEW ENTITY BUFFER ON ... cds_entity TYPE DELEGATED. Delegated buffers delegate the buffer usage of a CDS view entity to the buffer of its base object, which can improve performance. </td>
        </tr>
<tr>
            <td>RAP Type Mapping, Constants</td>
            <td>It is now possible to specify a fixed value on the left side of a RAP type mapping using the syntax constant. This can be useful, for example, in scenarios where different entities are mapped to the same underlying RAP persistent table. </td>
        </tr>
<tr>
            <td rowspan="6">ABAP_SQL</td>
            <td>Level-based Hierarchies</td>
            <td>A new variant of the hierarchy generator HIERARCHY uses the addition LEVELS in order to generate level-based hierarchies. These allow access to hierarchical data in data sources that are defined by level columns. In contrast, the former variant of HIERARCHY uses the addition CHILD TO PARENT ASSOCIATION in order to generate parent-child-based hierarchies from parent-child relationships. </td>
        </tr>
<tr>
            <td>Enhancement of STRING_AGG Function</td>
            <td>The optional separator sep of the STRING_AGG function can now be a host variable. </td>
        </tr>
<tr>
            <td>Enhancement of LTRIM and RTRIM Functions</td>
            <td>The character char of the LTRIM and RTRIM functions can now be a host variable or an SQL expression. Additionally, the length of char can be other than 1. </td>
        </tr>
<tr>
            <td>Aggregate Functions Processed by the ABAP SQL In-Memory Engine</td>
            <td>The aggregate functions MIN, MAX, and SUM can be processed by the ABAP SQL in-memory engine in the SELECT list now. They no longer bypass table buffering and do not cause the transport of an internal table accessed by FROM @itab to the database. </td>
        </tr>
<tr>
            <td>Spatial Functions</td>
            <td>SAP HANA constructors, methods, and functions for accessing and manipulating spatial data are now available as functions in ABAP SQL. </td>
        </tr>
<tr>
            <td>HIERARCHY_COMPOSITE_ID</td>
            <td>The function HIERARCHY_COMPOSITE_ID can be used to generate composite node identifiers free of name clashes. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Factorial and Binomial Functions</td>
            <td>The built-in function factorial computes the factorial of positive integer values. The binomial function computes the binomial coefficient. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 914 (2502)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="7">ABAP_CDS</td>
            <td>Writable CDS View Entities</td>
            <td>CDS view entities can now be defined as writable using the syntax WRITABLE. Some restrictions apply, which are listed in the respective chapter. </td>
        </tr>
<tr>
            <td>CDS Table Entities</td>
            <td>A new kind of CDS entity is available: the CDS table entity. A CDS table entity defines a database table in ABAP CDS using the statement DEFINE TABLE ENTITY. CDS table entities are the ABAP Cloud successor of DDIC database tables. </td>
        </tr>
<tr>
            <td>CDS Service Definitions Can Now Expose AMDP Procedure Implementations</td>
            <td>The new keyword EXPOSE METHOD can now be used in CDS service definitions for exposing AMDP procedure implementations for consumption in a business service. </td>
        </tr>
<tr>
            <td>Writable CDS External Entities</td>
            <td>CDS external entities can now be defined as writable using the syntax WRITABLE. Writable external entities allow modifying data on external database systems using the ABAP SQL INSERT, UPDATE, MODIFY, and DELETE operations. An ABAP SQL connection object is required to establish a service connection and enable the logical external schema for write access. </td>
        </tr>
<tr>
            <td>Event-Driven Side Effects</td>
            <td>You can define a business event to trigger a side effect in your RAP business object using the syntax for side effects. Whenever a defined event is raised by the application server, the event-driven side effect is triggered and the defined targets are reloaded. </td>
        </tr>
<tr>
            <td>Non-Standard Operations for Associations</td>
            <td>You can now define non-standard operations for associations. These are link action, unlink action, and inverse function. </td>
        </tr>
<tr>
            <td>Editable Treeview</td>
            <td>The editable treeview scenario is now available. You can use the syntax instance hierarchy to make a treeview editable. The RAP reorder action can optionally be specified to move a hierarchy node to a dedicated position among its siblings. It is defined using the syntax addition [managed] reorder action. </td>
        </tr>
<tr>
            <td rowspan="3">ABAP_SQL</td>
            <td>Connection Objects</td>
            <td>An ABAP SQL connection object implementing the interface IF_ABAP_SQL_CONNECTION represents a database connection that can be used behind the CONNECTION addition of ABAP SQL statements. Connection objects enable using service connections in ABAP Cloud. </td>
        </tr>
<tr>
            <td>Declaring a Client Column for Client-independent Data Sources</td>
            <td>The addition DECLARE CLIENT in the FROM clause of an ABAP SQL query declares a column of a client-independent data source as a client column and the data source as client-dependent during the current access. Implicit client handling takes place. </td>
        </tr>
<tr>
            <td>Addition OPTIONS for DML Statements</td>
            <td>The addition OPTIONS can now also be used for DML statements to introduce ABAP-specific additions. It can be used as OPTIONS for the DML statement itself. It can be used for subqueries of DML statements. </td>
        </tr>
<tr>
            <td rowspan="2">AMDP</td>
            <td>Client Safety of AMDP Methods</td>
            <td>In client-safe AMDP methods, also client-dependent or client-independent DDIC database tables can be accessed. Client isolation is achieved by filtering a generated intermediate database view for the client that is contained in the HANA session variable CDS_CLIENT. In client-safe AMDP methods, only objects of the ABAP database schema can be accessed that can be controlled by the USING list. </td>
        </tr>
<tr>
            <td>FOR SQL SERVICE</td>
            <td>The new addition FOR SQL SERVICE to the statement CLASS-METHODS specifies that an AMDP procedure implementation is intended to be exposed for external SQL access using an SQL service. </td>
        </tr>
<tr>
            <td rowspan="3">EML</td>
            <td>Runtime Type Service Method for BDEF Derived Types</td>
            <td>Runtime Type Services now offer a method to dynamically and type-safely create BDEF derived types. See the topic here. </td>
        </tr>
<tr>
            <td>Using Custom Secondary Table Keys in BDEF Derived Types</td>
            <td>It is possible to create your own secondary table keys for BDEF derived types, and use them in ABAP statements. Find more information here. </td>
        </tr>
<tr>
            <td>New Contract Checks</td>
            <td>The following contract checks have been added: Disallowed entries in the failed response parameter Entries with wrong fail causes in the failed response parameter Entries with non-initial %state_area components in the reported response in various contexts Violations result in the runtime error BEHAVIOR_BAD_HANDLER_RESPONSE. </td>
        </tr>
<tr>
            <td rowspan="1">SXML</td>
            <td>Header Options for sXML Library</td>
            <td>Header options for the XML declaration are now available in the sXML Library: CO_OPT_VAL_NO (no XML header) This is the default option. CO_OPT_VAL_WITHOUT_ENCODING (version) CO_OPT_VAL_FULL (version and encoding) </td>
        </tr>
<tr>
            <td rowspan="2">TRANSFORMATIONS</td>
            <td>New Domain for Mapping from ABAP to XML</td>
            <td>The following new special domain has been introduced, which overrides the default mapping of elementary ABAP types to asXML. XSDGEO for spatial data </td>
        </tr>
<tr>
            <td>New Format for format in the Attribute option of tt:value</td>
            <td>The following new format can be specified in parentheses after format in the option attribute of the ST statement tt:value: geometry, geography, and geo=SRID to define a spatial reference system for the conversion of the EWKB representation to GeoJSON and vice versa. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 913 (2411)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>Client Safety of CDS Table Functions</td>
            <td>The annotation ClientHandling.clientSafe declares a CDS table function explicitly as client-safe and ensures the corresponding checks. </td>
        </tr>
<tr>
            <td>authorization master ( none )</td>
            <td>The authorization control type none can be specified for authorization master to implicitly mark operations in authorization-dependent entities with authorization:none. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>Binary Floating Point Types in Arithmetic Expressions</td>
            <td>Operands of type FLTP or f can now be used in a decimal floating point expression and integer types can be used in a binary floating expression. </td>
        </tr>
<tr>
            <td rowspan="2">ITAB</td>
            <td>READ TABLE ... WHERE ...</td>
            <td>The addition WHERE can now be used with the READ TABLE statement. It reads the first line of an internal table that fulfills the WHERE condition. In contrast to the addition WITH KEY, arbitrary logical expressions can be specified instead of equality conditions only. The syntax offers an easier to use alternative to the following frequently used pattern but does not offer any performance gain. LOOP AT itab ... [USING KEY ...] WHERE log_exp|(cond_syntax).   EXIT. ENDLOOP. The same rules are valid as for using a WHERE condition in other internal table statements ( LOOP AT, MODIFY, DELETE) and must be taken into account. In particular, it is also possible to specify a dynamic WHERE condition. </td>
        </tr>
<tr>
            <td>IS [NOT] INITIAL in WHERE Condition of FILTER</td>
            <td>In the basic form of the FILTER operator, the predicate IS [NOT] INITIAL is now possible. </td>
        </tr>
<tr>
            <td rowspan="1">RFC</td>
            <td>New Synchronous RFC</td>
            <td>A synchronous RFC call specifying a remote session is now possible using the syntax CALL FUNCTION ... IN REMOTE SESSION .... </td>
        </tr>
<tr>
            <td rowspan="1">TRANSFORMATIONS</td>
            <td>Regular Expressions for XSLT</td>
            <td>Regular expression functions can now be used in XSLT. The following functions are available: matches() replace() tokenize() </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 912 (2408)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="9">ABAP_CDS</td>
            <td>CDS Propagated Buffer</td>
            <td>CDS buffer propagation can be defined for CDS view entities and CDS transactional queries now: Buffer propagation can be enabled and disabled with the annotation AbapCatalog.entityBuffer.propagationAllowed: true|false The propagated fields can be specified in a CDS propagated buffer defined by PROPAGATE VIEW ENTITY BUFFER ON cds_entity ... </td>
        </tr>
<tr>
            <td>CDS Aspects</td>
            <td>CDS aspects can now be used to define reusable elements of CDS entities using the syntax DEFINE ASPECT. The syntax BIND ASPECT binds the aspect to a CDS view entity, and with the syntax INCLUDE, individual components of the CDS aspect can be included in the select list of the CDS view entity and CDS projection views. </td>
        </tr>
<tr>
            <td>CDS External Entities</td>
            <td>A new kind of CDS entity is available: the CDS external entity. CDS external entities can be used to access data from external database systems within the AS ABAP. They are defined using the statement DEFINE EXTERNAL ENTITY. The properties of the connection between an external entity and the external system are configured using a CDS logical external schema. </td>
        </tr>
<tr>
            <td>Client Parameter in SQL Functions</td>
            <td>For the following built-in functions, the session variable $session.client must be passed to the optional formal parameter client in ABAP for Cloud Development: UNIT_CONVERSION CURRENCY_CONVERSION ABAP_SYSTEM_TIMEZONE ABAP_USER_TIMEZONE TSTMP_TO_DATS TSTMP_TO_TIMS TSTMP_TO_DST DATS_TIMS_TO_TSTMP The same holds for usage of these functions in CDS DDIC-based views. </td>
        </tr>
<tr>
            <td>Session Variables and Enumerated Constants on the Right Side of Literal Conditions</td>
            <td>Literal conditions in CDS roles now support session variables and enumerated constants on the right side of the logical expression. For details, see the topic CDS DCL - DEFINE ROLE, literal_condition. </td>
        </tr>
<tr>
            <td>ABP Auxiliary Class</td>
            <td>The new statement auxiliary class can be used to specify one or more ABP auxiliary classes that allow the outsourcing of functionality from an ABAP behavior pool. The primary purpose of an ABP auxiliary class is to allow developers to concurrently implement RAP behavior. </td>
        </tr>
<tr>
            <td>Abstract BDEF, with hierarchy like entity</td>
            <td>The new addition with hierarchy like entity can now be used in the RAP behavior definition header of an abstract behavior definition. It imposes stricter syntax checks to ensure compatibility between the hierarchical BDEF derived type of the abstract BDEF in question and the structured type of the underlying CDS entity. </td>
        </tr>
<tr>
            <td>BDEF Alternative Keys for Managed and Unmanaged Behavior Definitions</td>
            <td>BDEF pure keys and BDEF secondary keys are now available. They define the signature of a RAP key function. </td>
        </tr>
<tr>
            <td>Explicit Save Order</td>
            <td>You can explicitly define the save order using save after. </td>
        </tr>
<tr>
            <td rowspan="1">DDIC</td>
            <td>Empty Key of Table Types</td>
            <td>Empty table keys that are available in ABAP language since 7.40, SP02, can now also be defined for DDIC table types for standard tables. </td>
        </tr>
<tr>
            <td rowspan="3">EML</td>
            <td>FORWARDING PRIVILEGED Addition</td>
            <td>Using the FORWARDING PRIVILEGED addition in READ, MODIFY, and GET PERMISSIONS statements, you can pass on the privileged context in subsequent ABAP EML requests. </td>
        </tr>
<tr>
            <td>ABAP EML Read Operations Not Allowed in Late Save Phase</td>
            <td>ABAP EML read operations are not allowed in the RAP late save phase. </td>
        </tr>
<tr>
            <td>ABP Auxiliary Class</td>
            <td>ABP auxiliary classes, which enable extended access rights to a referenced RAP BO, are possible now. Find more information here. </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Harmonization of Table Expressions and READ TABLE Statement</td>
            <td>The variants for specifying a table line in table expressions are now mapped exactly to the respective variants of the READ TABLE statement. The same rules apply and the same syntax warnings can occur. For that, the internal behavior of the variants [KEY keyname [COMPONENTS]] ... was adjusted to match the behavior of the respective READ variants and a new variant TABLE KEY keyname [COMPONENTS] ... was introduced for READ ... WITH TABLE KEY .... Before, the syntax of the existing variants was not stringently mapped. Despite this adjustment, the behavior of existing programs is not changed. The adjustment offers new possibilities, such as specifying only a part of a table key or specifying additional search fields with a table key. Some new syntax warnings regarding performance might show up and can be either corrected or hidden by a pragma. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 796 (2405)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="8">ABAP_CDS</td>
            <td>Extended Cast Matrix</td>
            <td>In CDS view entities and in CDS DDIC-based views (obsolete), casting from the data type CURR into the data types DECFLOAT16 and DECFLOAT34 is now possible. </td>
        </tr>
<tr>
            <td>New Analytical Scalar Functions</td>
            <td>The following analytical scalar functions are available as of ABAP release 7.96: LN LOG EXPONENTIAL POWER For a complete list, see the topic ABAP CDS - Analytical Scalar Functions. </td>
        </tr>
<tr>
            <td>CDS Enumerated Elements in CDS Conditions</td>
            <td>CDS enumerated elements are now also supported as left-hand operands in logical expressions. For more details, see the topic CDS DDL - Using CDS Enumerated Types. </td>
        </tr>
<tr>
            <td>Support of Data Types in Logical Expressions with IS INITIAL</td>
            <td>In logical expressions with the operator INITIAL, the left-hand operand can now also have one of the following data types: RAW, DECFLOAT16, DECFLOAT34, DATN, TIMN, UTCL. This applies to all CDS entities except for CDS DDIC-based view. </td>
        </tr>
<tr>
            <td>RAP Business Events for Child Entities</td>
            <td>RAP business events can now also be defined for CDS child entities. Up to now, a RAP business event could only be defined for the root node of a RAP business object. </td>
        </tr>
<tr>
            <td>Subentities as Authorization Master</td>
            <td>You can now define RAP BO entities as RAP authorization master entity that are not defined as root entity. </td>
        </tr>
<tr>
            <td>Result Multiplicity in Factory Actions</td>
            <td>RAP factory actions now allow result multiplicity. This means that the cardinality can have one of the following values: [0..*] [1..*] A factory action can be used to create none, one, or any number of entity instances. </td>
        </tr>
<tr>
            <td>RAP Key Functions</td>
            <td>RAP key functions are now available. A RAP key function is defined with reference to a BDEF alternative key and it returns the entity instances with matching values. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_OBJECTS</td>
            <td>Visibility Sections</td>
            <td>For technical reasons a global class that can be part of an inheritance tree that is a subclass a non-final class that is not a subclass but can be a superclass must contain the statements PROTECTED SECTION and PRIVATE SECTION also for empty protected and private sections. Before Release 7.96, a syntax warning occurred in case of missing protected and private sections only for non-final classes. Now the warning occurs also for final subclasses. </td>
        </tr>
<tr>
            <td rowspan="7">ABAP_SQL</td>
            <td>Dynamic SELECT</td>
            <td>The new dynamic forms of the statements WITH enable fully dynamic SELECT statements, where all clauses except the INTO clause and the ABAP-specific additions can be specified in one dynamic token. </td>
        </tr>
<tr>
            <td>ORDER BY n</td>
            <td>In the column list of the ORDER BY clause, a literal or a host constant of an integer type is handled as the column position in the result set of the query. This feature is available since the introduction of SQL expressions in the ORDER BY clause with release 7.89 but was undocumented and behaved partly undefined. With release 7.95 the behavior is defined and documented. </td>
        </tr>
<tr>
            <td>OPTIONS</td>
            <td>A new addition OPTIONS can and should be used in front of the ABAP-specific additions PRIVILEGED ACCESS, BYPASSING BUFFER at the end of the statement SELECT. </td>
        </tr>
<tr>
            <td>PROVIDED BY</td>
            <td>A new addition PROVIDED BY to the data source of the FROM clause specifies a logical external schema in order to map a CDS external entity to an external system. </td>
        </tr>
<tr>
            <td>Syntax Warning for Replacement Objects</td>
            <td>Each ABAP SQL access to a DDIC database table or a DDIC database view with a replacement object produces a syntax check warning that can be suppressed by the pragma ##open_sql_redirect_dml[...]. The pragma is used to highlight such accesses in the code and indicates the special rules that must be observed for them. Before release 7.96, only write accesses led to warnings. </td>
        </tr>
<tr>
            <td>CDS Scalar Functions</td>
            <td>CDS scalar functions can be used as SQL expressions at all operand positions where expressions are allowed. </td>
        </tr>
<tr>
            <td>Client Parameter in SQL Functions</td>
            <td>In the following built-in functions, the optional formal parameter client must not be used in language version ABAP for Cloud Development any more: UNIT_CONVERSION CURRENCY_CONVERSION ABAP_SYSTEM_TIMEZONE ABAP_USER_TIMEZONE TSTMP_TO_DATS TSTMP_TO_TIMS TSTMP_TO_DST DATS_TIMS_TO_TSTMP Only the current client as the default value of the parameter must be used in these functions. Passing an explicit client leads to a syntax warning and later to a syntax error. </td>
        </tr>
<tr>
            <td rowspan="2">AMDP</td>
            <td>New AMDP Option CDS SESSION CLIENT DEPENDENT</td>
            <td>The new addition CDS SESSION CLIENT DEPENDENT can be used after AMDP OPTIONS to set the HANA session variable CDS_CLIENT to its default value and to declare the AMDP method explicitly as client-dependent and to make it client-safe. The addition can be also used as an option after FOR DDL. </td>
        </tr>
<tr>
            <td>Client Safety of AMDP Methods</td>
            <td>In order to support the concept of client isolation, AMDP methods can be made client-safe. This is mandatory for ABAP for Cloud Development. </td>
        </tr>
<tr>
            <td rowspan="1">EML</td>
            <td>Commits in the Context of RAP Projections</td>
            <td>Regarding commits in the context of RAP projections behavior definitions (or RAP interface behavior definitions): RAP BO instances that fail to be committed in the RAP save sequence for the base RAP BO are returned to the RAP BO consumer. The messages in the failed RAP response parameter are automatically mapped to projections. </td>
        </tr>
<tr>
            <td rowspan="3">EXPRESSIONS</td>
            <td>String Expressions in VALUE Operator</td>
            <td>String expressions can now directly be assigned to incompatible components without using CONV in assignments to structure components: VALUE ...( comp = strg_expr ) vs. VALUE ...( comp = CONV #( strg_expr ) ). </td>
        </tr>
<tr>
            <td>FOR GROUPS ... OF Supported for NEW</td>
            <td>The variant FOR GROUPS ... OF of the iteration expression FOR is now also supported for the constructor operator NEW. </td>
        </tr>
<tr>
            <td>Syntax Warning for Multiple Assignments of Components</td>
            <td>In the constructor operators NEW and VALUE for structures, it is not allowed to assign multiple values to a component. This leads to a syntax error, if the component is directly named. It leads now also to a syntax warning in cases, where component groups are involved. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 795 (2402)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="1">ABAP_CDS</td>
            <td>BDEF Friends</td>
            <td>The new optional addition with friends can be used to specify one or more BDEF extensions as BDEF friends. A BDEF friend has unrestricted access to all components of the original BDEF. For example, a BDEF friend can call internal RAP BO operations and read or modify RAP BO entities IN LOCAL MODE. BDEF friends can be used for modularization purposes. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_DOCU</td>
            <td>Documentation Language</td>
            <td>From release 7.95 on, the ABAP Keyword Documentation is delivered and supported in English only. </td>
        </tr>
<tr>
            <td>CDS Annotation Documentation</td>
            <td>From release 7.95 on, properties and documentation for CDS annotations delivered as SAP annotations are shown under ABAP CDS - SAP Annotation Documentation. If available, the KTD of an annotation definition is embedded there and/or links to help portal documentation are shown. The documentation of the ABAP annotations that formerly appeared at their point of use in the ABAP Keyword Documentation was migrated to KTDs and is shown under ABAP Annotations now. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_SQL</td>
            <td>Caching of Hierarchies</td>
            <td>The new addition CACHE for SELECT, FROM HIERARCHY allows the caching policy to be defined for hierarchies. </td>
        </tr>
<tr>
            <td>Database Hints for the ABAP SQL In-Memory Engine</td>
            <td>Behind the addition %_HINTS, the database system ABAP can now be specified for listing database hints for the ABAP SQL in-memory engine. These database hints are documented under ABAP SQL In-Memory Engine, Database Hints. In the current release, there are not yet any hints available for the ABAP SQL in-memory engine. </td>
        </tr>
<tr>
            <td rowspan="1">DDIC</td>
            <td>Removal of Extensibility Annotations</td>
            <td>The following extensibility annotations are no longer available for dictionary objects and they are no longer required for C0 release of a DDIC object: @AbapCatalog.enhancement.quotaShareCustomer @AbapCatalog.enhancement.quotaSharePartner </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 794 (2311)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_CDS</td>
            <td>Extended Cast Matrix</td>
            <td>In CDS view entities and in CDS DDIC-based views (obsolete), casting from the data type CURR into the data types DECFLOAT16 and DECFLOAT34 is now possible. </td>
        </tr>
<tr>
            <td>Abstract BDEF, mandatory:execute for Associations</td>
            <td>The optional addition mandatory:execute is now available for associations in abstract BDEFs. It declares the association in question as mandatory, that is, whenever the abstract BDEF is used as an input parameter for a RAP action or a RAP function, the %control flag must be set for this association. </td>
        </tr>
<tr>
            <td>RAP Default Values Function</td>
            <td>A RAP default values function can be used to default values for fields or input parameters for RAP BO operations on the user interface. The syntax for defining a default values function is default function. </td>
        </tr>
<tr>
            <td rowspan="3">EML</td>
            <td>RAP Handler Methods for RAP Default Values Functions</td>
            <td>RAP default values functions require an implementation in an ABAP behavior pool in a specific way. See the topic Handler Methods for RAP Default Values Functions. </td>
        </tr>
<tr>
            <td>C1 Release of Class CL_ABAP_BEHAVIOR_SAVER_FAILED</td>
            <td>The RAP saver class CL_ABAP_BEHAVIOR_SAVER_FAILED has been released with the C1 release contract for the ABAP language version ABAP for Cloud Development. See the blog post Using BAPIs in RAP for further details. </td>
        </tr>
<tr>
            <td>Unit Tests and Contract Check Violations</td>
            <td>ABAP contract checks violations are made visible in unit tests, i.e. the unit tests will fail if a contract check violation occurs. </td>
        </tr>
<tr>
            <td rowspan="1">MISC</td>
            <td>Enhancement to CL_ABAP_EXPIMP_UTILITIES</td>
            <td>... </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 793 (2308)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="11">ABAP_CDS</td>
            <td>CDS Scalar Functions</td>
            <td>The new CDS object is available: the CDS scalar function. It is defined using the CDS DDL statement DEFINE SCALAR FUNCTION. As of this release, scalar functions are available for two different runtimes: The HANA SQL runtime. SQL-based scalar functions can be created internally at SAP as CDS system functions, or externally by customers and partners as CDS user-defined functions. The ABAP Analytical Engine. Analytical scalar functions can be created internally at SAP only. They are delivered to customers as CDS system functions. </td>
        </tr>
<tr>
            <td>Extension of the CAST Matrix</td>
            <td>The matrix of types that can be converted to each other using CAST has been enhanced for CDS DDIC-based views (obsolete) and for CDS view entities. Data types DECFLOAT16 and DECFLOAT34 can now be cast into data type CURR. See topic CDS DDL - DDIC-Based View, cast_expr and topic CDS DDL - CDS View Entity, cast_expr </td>
        </tr>
<tr>
            <td>Release of CDS Enumerated Types</td>
            <td>CDS enumerated types define enumerated types natively in ABAP CDS. The syntax statement for defining a CDS enumerated type is DEFINE TYPE ENUM. CDS enumerated types can be used as follows: In ABAP CDS for typing and casting, as operands in expressions, and in comparisons. In ABAP for typing after the TYPES statement. In ABAP SQL as elementary operands and in cast expressions. </td>
        </tr>
<tr>
            <td>CDS Transactional Interface, WHERE Clause</td>
            <td>A WHERE condition is now available also for CDS transactional interfaces to restrict the result set that is returned when the transactional interface is accessed. </td>
        </tr>
<tr>
            <td>$projection References on Path Expressions</td>
            <td>A reuse expression $projection.Field can now also be used to reuse fields selected via a path expression. </td>
        </tr>
<tr>
            <td>Interface BDEF, use event</td>
            <td>In RAP interface behavior definitions, RAP business events can be reused using the syntax use event. </td>
        </tr>
<tr>
            <td>RAP Derived Events</td>
            <td>RAP derived events allow the reuse of existing RAP business events with a custom payload. RAP derived events are available in managed and unmanaged RAP BOs and in base BDEF extensions. They are defined using the syntax managed event. </td>
        </tr>
<tr>
            <td>Draft Action Activate Optimized</td>
            <td>The optional addition optimized is now available for the draft action Activate. SAP recommends always using this addition, since it speeds up the activation of draft instances. </td>
        </tr>
<tr>
            <td>with managed instance filter</td>
            <td>The optional addition with managed instance filter is available for projection BDEFs and interface BDEFs. If specified, the WHERE condition of the underlying CDS transactional query or CDS transactional interface is evaluated when the BDEF is accessed with ABAP EML or OData requests from Web clients. </td>
        </tr>
<tr>
            <td>Redefined Parameters, Projection or Interface BDEF</td>
            <td>The input or output parameter of a RAP action, RAP function, or a RAP business event that is reused in a projection BDEF or an interface BDEF can optionally be replaced with its own projection-specific structure using the syntax additions deep parameter or deep result. </td>
        </tr>
<tr>
            <td>Draft Action AdditionalSave</td>
            <td>A new draft action is available, draft action AdditionalSave. This draft action allows users to define a custom saving strategy for draft instances. It is intended in particular for draft actions with a user-defined implementation, defined using the addition with additional implementation. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_DOCU</td>
            <td>ADT Documentation Layout</td>
            <td>From Release 7.93 on, the ABAP Keyword Documentation presents a new layout in ABAP development tools for Eclipse (ADT) where a toggle between Standard ABAP and ABAP Cloud is also available. The new layout includes: New header bar with links to the mail feedback and the web version Highlighted key blocks for hints, code blocks, and others Tables with zebra stripes </td>
        </tr>
<tr>
            <td rowspan="1">AMDP</td>
            <td>New AMDP Option CLIENT INDEPENDENT</td>
            <td>The new addition CLIENT INDEPENDENT can be used after AMDP OPTIONS to declare an AMDP method as client-independent. When used, only client-independent database objects can be accessed and specified after the addition USING. </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Improved Optimization of the WHERE Condition</td>
            <td>The improved optimization of the WHERE condition introduced in release 7.91 was extended by respecting any binary comparison operators and also BETWEEN for sorted keys. </td>
        </tr>
<tr>
            <td rowspan="4">SYSTEM_CLASSES</td>
            <td>System Classes for Numbers</td>
            <td>Two new system classes for numeric calculations are available: CL_ABAP_BIGINT contains methods for calculations with any size of integer in ABAP. CL_ABAP_RATIONAL contains methods for calculating with rational numbers without any precision loss. </td>
        </tr>
<tr>
            <td>New Class CL_ABAP_DIFF</td>
            <td>The new class CL_ABAP_DIFF compares the content of internal tables and returns information about any differences found. </td>
        </tr>
<tr>
            <td>New Methods in CL_ABAP_TSTMP</td>
            <td>The new methods MOVE_TRUNC, MOVE_TO_SHORT_TRUNC, ADD_TO_SHORT_TRUNC and SUBTRACTSECS_TO_SHORT_TRUNC of system class CL_ABAP_TSTMP round the fractional seconds of long UTC time stamps down while the existing methods MOVE, MOVE_TO_SHORT, ADD_TO_SHORT and SUBTRACTSECS_TO_SHORT round commercially. Since the latter might be unexpected, the new methods can be used now in order to preserve the integer part of a long time stamp. </td>
        </tr>
<tr>
            <td>New Method in CL_ABAP_BEHV_AUX</td>
            <td>Using the new method GET_CURRENT_PHASE, you get information about the current RAP transactional phase. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 792 (2305)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">ABAP_CDS</td>
            <td>Annotation Environment.sql.passValue, Scope Enhancement</td>
            <td>The ABAP annotation Environment.sql.passValue is now also available for CDS parameters in CDS view entities, CDS projection views, and CDS hierarchies. The annotation scope has been enhanced. </td>
        </tr>
<tr>
            <td>New Annotation Environment.sql.passValueForClient</td>
            <td>A new ABAP annotation is available in CDS view entities, CDS projection views, and CDS hierarchies: Environment.sql.passValueForClient. It works in a similar way to the annotation Environment.sql.passValue, but for client fields. It specifies whether a placeholder ? or a literal value is passed to the database in an ABAP SQL condition when the client field is compared with a host variable. </td>
        </tr>
<tr>
            <td>Further Operand Positions for Typed Literals</td>
            <td>Typed literals can now be used in more operand positions in ABAP CDS. They can now be used in the WHERE condition of CDS projection views and in the ON condition of CDS associations. </td>
        </tr>
<tr>
            <td>Authorization Control on Action Level</td>
            <td>Two new RAP BO operation additions are available for actions and determine actions: authorization:global authorization:instance Both of these additions are specified on action-level and they replace the authorization control that is specified in the authorization master entity. In managed, unmanaged, and projection BDEFs, these additions are optional. In base BDEF extensions, authorization control on action-level is mandatory. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_DOCU</td>
            <td>External Mailbox for Documentation</td>
            <td>From release 7.92 on, the ABAP Keyword Documentation display offers a function to send a feedback mail to f1_help@sap.com in non-SAP development systems and in the SAP Help Portal. Up to now, the possibility for a feedback mail existed only in SAP's own development systems. The function can be switched on or off in the configuration of the ABAP Keyword Documentation, see SAP Note 3051036. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>Access to Multiple Internal Tables</td>
            <td>It is now possible to process multiple internal tables accessed with FROM @itab within one ABAP SQL statement with the ABAP SQL in-memory engine. Currently, this is restricted to joins of internal tables where no database tables are involved. </td>
        </tr>
<tr>
            <td rowspan="5">EML</td>
            <td>RAISE ENTITY EVENT</td>
            <td>You can use RAISE ENTITY EVENT statements to raise a RAP business event. </td>
        </tr>
<tr>
            <td>METHODS, FOR ENTITY EVENT</td>
            <td>The METHODS, FOR ENTITY EVENT addition supports RAP event handler method definitions. </td>
        </tr>
<tr>
            <td>CLASS, FOR EVENTS OF</td>
            <td>The FOR EVENTS OF addition supports the creation of RAP event handler classes. </td>
        </tr>
<tr>
            <td>CL_ABAP_BEHAVIOR_EVENT_HANDLER</td>
            <td>Local classes that inherit from CL_ABAP_BEHAVIOR_EVENT_HANDLER can be implemented in the CCIMP include of a RAP event handler class to locally consume RAP business events. </td>
        </tr>
<tr>
            <td>CL_ABAP_TX</td>
            <td>The class CL_ABAP_TX is used to explicitly set RAP transactional phases. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 791 (2302)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="9">ABAP_CDS</td>
            <td>CDS Analytical Projection View, Projected Entity</td>
            <td>CDS analytical projection views can now also have analytical dimension views as projected entity. </td>
        </tr>
<tr>
            <td>Release of CDS Simple Types</td>
            <td>CDS simple types define elementary data types natively in ABAP CDS. A CDS simple type can be enriched with metadata using CDS annotations. The syntax statement for defining a CDS simple type is DEFINE TYPE. </td>
        </tr>
<tr>
            <td>New Cardinality Syntax for Associations and Joins</td>
            <td>A new syntax for specifying the cardinality of CDS associations, CDS joins, and of filter conditions of CDS path expressions is now available: {MANY | ONE | {EXACT ONE}} TO {MANY | ONE | {EXACT ONE}} This syntax allows a source and a target cardinality to be specified, while the previously available numeric syntax only allowed the target cardinality to be specified. The new cardinality syntax can be used to improve query performance. It is available in CDS view entities, CDS projection views, CDS custom entities, and CDS abstract entities. </td>
        </tr>
<tr>
            <td>New Type of Access Rule for CDS Projection Views</td>
            <td>A new type of access rule is available for CDS projection views of type CDS transactional query: the projection_rule defined with the statement GRANT SELECT ON ... AS PROJECTION ON ... FALLBACK ASSOCIATION .... </td>
        </tr>
<tr>
            <td>New DCL Functions</td>
            <td>Two new DCL functions are available: SWITCH_RUNTIME_STATE TOGGLE_RUNTIME_STATE Both functions retrieve the runtime state of a switch in the Switch Framework. </td>
        </tr>
<tr>
            <td>RAP Side Effects</td>
            <td>RAP side effects define interdependencies among RAP BO properties that trigger a reload of the affected properties on the user interface. Side effects are translated into annotations in the OData metadata of a RAP service. The syntax for defining RAP side effects is side effects. </td>
        </tr>
<tr>
            <td>Abstract BDEF Extensions</td>
            <td>The new statement extension for abstract of the RAP BDL makes it possible to extend abstract behavior definitions with abstract BDEF extensions. Abstract BDEFs are mainly used as parameters for RAP actions, RAP functions, and RAP business events. An abstract BDEF extension allows you to extend these parameters. </td>
        </tr>
<tr>
            <td>BDEF Extension Layering</td>
            <td>It is now possible to extend a BDEF extension with further BDEF extensions. This is referred to as extension layering. As a prerequisite, the BDEF extension in question must be explicitly enabled for extensibility. The rules are described in the topic RAP - Extensibility Enabling for BDEF Extensions. Technically, the BDEF extension that extends another extension is a regular extension and the respective rules apply. </td>
        </tr>
<tr>
            <td>Authorization Context for Disable, New Options</td>
            <td>There are further options available for the authorization context for disable after the addition for disable: save:early: Skips the authorization checks defined in the respective authorization context in the early save phases finalize and check_before_save. save:late: Skips the authorization checks defined in the respective authorization context in the late save phases adjust_numbers, save, and save_modified. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_SQL</td>
            <td>New Cardinality Syntax for Joins</td>
            <td>A new syntax for specifying the cardinality of joins is available: {MANY | ONE | {EXACT ONE}} TO {MANY | ONE | {EXACT ONE}} This syntax allows specifying a source and a target cardinality. The new cardinality syntax can be used to improve query performance. It can also be used in SQL path expressions and CTE associations. </td>
        </tr>
<tr>
            <td>New Addition PRIVILEGED ACCESS</td>
            <td>With the new addition PRIVILEGED ACCESS, CDS access control can be disabled for a complete SELECT statement. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>DEFAULT as New Addition to the CORRESPONDING Operator</td>
            <td>The addition DEFAULT allows the assignment of values for a target component based on an expression. </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Improved Optimization of the WHERE Condition</td>
            <td>Up to now, an optimization of the WHERE condition by using key access took place only under very limited circumstances where the key value pairs had to be compared explicitly for equality and combined by AND. Now, the compiler analyzes the WHERE condition thoroughly and tries itself to extract the key/value pairs that are necessary for the key access. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 790 (2211)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">ABAP_CDS</td>
            <td>@AbapCatalog.preserveKey is Obsolete</td>
            <td>The CDS annotation @AbapCatalog.preserveKey is obsolete and has partly a different behavior now. The annotation was used before Release 7.90 to define the key fields of the CDS-managed DDIC view of an obsolete CDS DDIC-based view. The annotation can be kept in existing data definitions, but it does not have an effect any more and leads to a syntax check warning. The key fields of the CDS-managed DDIC view are always defined by the addition key in the SELECT list. The change in behavior between 7.89 and 7.90 is incompatible. The key fields of the CDS-managed DDIC view of an obsolete CDS DDIC-based view are defined differently now for views where the annotation was not specified or where it was specified with the value false. The value false was used to define the key fields of the CDS-managed DDIC view as for a DDIC database views in ABAP Dictionary, regardless of the addition KEY. It was also the standard value, when the annotation was not specified. Now, the behavior is always as for the value true. </td>
        </tr>
<tr>
            <td>Interface BDEF, New Field Characteristics</td>
            <td>In RAP interface behavior definitions, the following RAP field characteristics are now available: readonly mandatory:create readonly:update </td>
        </tr>
<tr>
            <td>Managed and Unmanaged RAP BO, New Field Characteristic</td>
            <td>A new RAP field characteristic is available for managed and unmanaged RAP BOs: notrigger[:warn]. Fields with this attribute must not be used in a trigger condition of a RAP validation or a RAP determination. </td>
        </tr>
<tr>
            <td>Static Default Factory Actions</td>
            <td>The syntax addition default is available for static factory actions in managed, unmanaged, and projection BDEFs. Exactly one static factory action per RAP BO entity can be defined as default static factory action. The addition default is evaluated by consuming frameworks, such as OData. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>ABAP SQL Expressions on the Right Side</td>
            <td>ABAP SQL expressions can be defined on the right side of a condition enclosed in parentheses. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>Returning a Value with RETURN</td>
            <td>In functional methods, the statement RETURN can be used to assign the result of an expression expr to the return value when terminating the method. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>Absolute Type Names for Line Types</td>
            <td>The new specification \LINE allows the line type of an internal table to be specified in an absolute type name. \LINE can be followed by -comp to specify the type of a component of the line type. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 789 (2208)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="20">ABAP_CDS</td>
            <td>CDS Custom Entity Extensions</td>
            <td>The new statement EXTEND CUSTOM ENTITY of the DDL of ABAP CDS makes it possible to add new elements to existing CDS custom entities by using CDS custom entity extensions. </td>
        </tr>
<tr>
            <td>CDS View Entity, LEFT and RIGHT</td>
            <td>In CDS view entities, the functions LEFT and RIGHT have been enhanced. They now accept literals, fields, parameters, expressions, and built-in functions as argument for the parameter len. </td>
        </tr>
<tr>
            <td>CDS View Entity, CASE ELSE NULL</td>
            <td>In CDS view entities, the addition ELSE NULL is available in simple and complex case distinctions. It defines the null value as return value of the ELSE branch. </td>
        </tr>
<tr>
            <td>CDS View Entity, Table Buffering</td>
            <td>CDS view entity buffering was enhanced: View on view buffering is now supported, under the precondition that the CDS view entity used as data source meets certain requirements. </td>
        </tr>
<tr>
            <td>CDS DDIC-Based Views Are Obsolete</td>
            <td>CDS DDIC-based views (obsolete), defined using the statement DEFINE VIEW, are obsolete. When creating new data models, CDS view entities, defined using DEFINE VIEW ENTITY, should be used instead. </td>
        </tr>
<tr>
            <td>ABAP Program That Lists Usages of CDS-Managed DDIC Views</td>
            <td>Using CDS-managed DDIC views has been declared obsolete. The following ABAP program lists all repository objects that use CDS-managed DDIC views: RUT_WHERE_USE_SQLVIEW. </td>
        </tr>
<tr>
            <td>DEFINE ROLE Statement Has New Addition SWITCHABLE</td>
            <td>The new addition SWITCHABLE is now available for the DEFINE ROLE statement. Using this addition, the content of the role reacts to status changes in the Switch Framework with respect to the package of the access control. </td>
        </tr>
<tr>
            <td>Define Authorization Context</td>
            <td>It is now possible to define authorization contexts in a RAP behavior definition using the keyword define authorization context. There are different ways to activate an authorization context. When activated, all authorization objects listed in the respective context are always successful, that means, the respective authorization checks are skipped. </td>
        </tr>
<tr>
            <td>With Privileged Mode Disabling</td>
            <td>The new syntax with privileged mode disabling supersedes the deprecated version with privileged mode. The new syntax version disables an authorization contexts when accessing the RAP BO in question with a privileged EML call. </td>
        </tr>
<tr>
            <td>Variants to Define a RAP Own Authorization Context</td>
            <td>New variants are available for defining a RAP own authorization context: define own authorization context by privileged mode; define own authorization context by privileged mode and { ... } </td>
        </tr>
<tr>
            <td>BDEF Extensions for RAP BOs</td>
            <td>BDEF extensions for RAP BOs can be defined using the statement extension. </td>
        </tr>
<tr>
            <td>BDEF Extensions for RAP Projection BOs</td>
            <td>BDEF projection extensions for RAP projection business objects can be defined using the statement extension for projection. </td>
        </tr>
<tr>
            <td>New Version of BDEF Strict Mode</td>
            <td>A new version of BDEF strict mode is available: Strict mode version 2, specified using strict(2). It applies even more syntax checks than the first version. </td>
        </tr>
<tr>
            <td>Draft Query Views for Draft-Enabled RAP BOs</td>
            <td>The new syntax addition query is available to define a draft query view for a draft-enabled RAP BO. This addition is optional. Only in the context of RAP extensibility is it a mandatory prerequisite for the C0 release of the CDS behavior definition in question. </td>
        </tr>
<tr>
            <td>result selective for Deep Result Types</td>
            <td>The addition result selective is now also possible for deep result types. It can be specified for an output parameter of a RAP action or RAP function to return only parts of the result structure. </td>
        </tr>
<tr>
            <td>RAP Business Events</td>
            <td>Using RAP business events, the ABAP RESTful Application Programming Model now offers native support for event-driven architecture. RAP business events are defined with the language element event. A flat or deep output parameter can optionally be specified. </td>
        </tr>
<tr>
            <td>RAP Save Actions with Phase Specification</td>
            <td>RAP save actions can now specify one or both of the RAP saver methods finalize or adjustnumbers in brackets to indicate the RAP saver method during which the action can be executed. </td>
        </tr>
<tr>
            <td>Repeatable RAP Actions and Functions</td>
            <td>RAP actions and RAP functions can be defined as repeatable. This syntax addition explicitly allows multiple executions of the same action or function on the same RAP BO entity instance within the same ABAP EML or OData request. The BDEF derived type of a repeatable action or function has an extra %cid component, in contrast to the BDEF derived type of non-repeatable actions or function. </td>
        </tr>
<tr>
            <td>CDS Service Definition Extension</td>
            <td>It is now possible to define service definition extensions in CDS SDL using the statement EXTEND SERVICE. </td>
        </tr>
<tr>
            <td>CDS Service Definition, Provider Contract</td>
            <td>The new statement PROVIDER CONTRACTS is now available for CDS service definitions. A provider contract defines the type of service binding that is to be used for a service definition. The effect is that stricter syntax checks are applied. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_SQL</td>
            <td>Support for ORDER BY in the ABAP SQL In-Memory Engine</td>
            <td>The ABAP SQL in-memory engine fully supports the ORDER BY clause now: ORDER BY no longer bypasses table buffering. Before, it bypassed table buffering in cases where single columns were specified as sort keys and these columns were not a left-aligned subset of the primary key in the correct order or if the addition DESCENDING was specified for a column. ORDER BY can now be used with SELECT FROM @itab without transporting the internal table to the database. This allows looping over an internal table in a sequence defined by ORDER BY and the sorting of internal tables by expressions. </td>
        </tr>
<tr>
            <td>ORDER BY n</td>
            <td>In the column list of the ORDER BY clause, a literal or a host constant of an integer type is handled as the column position in the result set of the query. This feature is undocumented and behaves partly undefined. With release 7.96 the behavior is defined and documented. </td>
        </tr>
<tr>
            <td rowspan="2">ASSIGNMENTS</td>
            <td>New Dynamic Component Specification in ASSIGN</td>
            <td>Components of structures can be assigned to field symbols with the new syntax struc-(comp) that largely replaces the variant ASSIGN COMPONENT OF. </td>
        </tr>
<tr>
            <td>New Addition ELSE UNASSIGN</td>
            <td>The new addition ELSE UNASSIGN can be specified for the following variants of the statement ASSIGN: dynamic assignments assignments of dynamic components dynamic access assignment of a table expression It can be also specified with the addition ASSIGNING of the following internal table statements: READ TABLE LOOP AT itab INSERT MODIFY All these statements set sy-subrc. If an assignment is not successful, sy-subrc is set to the value 4 or sometimes 8. If the addition ELSE UNASSIGN is specified, the state of the field symbol is set to unassigned in that case. The field symbol is assigned only if the assignment is successful. If ELSE UNASSIGN is not specified, the field symbol keeps its previous state, which was the only behavior up to now. Using ELSE UNASSIGN introduces the same behavior as for the static variant to the above variants. In another way around, one can say that the static variant uses ELSE UNASSIGN implicitly. </td>
        </tr>
<tr>
            <td rowspan="1">EML</td>
            <td>Entity Manipulation Language</td>
            <td>RAISE ENTITY EVENT It is now possible to raise a RAP entity event using this statement. READ and MODIFY The FIELDS ( ... ) WITH addition for READ and MODIFY supports the setting of %control regarding components in deep input parameters in the context of deep action and function parameters. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Inline Declarations</td>
            <td>The new declaration operator FINAL declares an immutable variable that cannot be assigned another value at other write positions of the same context. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 788 (2205)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="6">ABAP_CDS</td>
            <td>CDS View Entity, SUBSTRING Function</td>
            <td>In CDS view entities, the SUBSTRING function has been enhanced. It now accepts not only literals, but also fields, parameters, expressions, and built-in functions as arguments for the parameters pos and len. </td>
        </tr>
<tr>
            <td>IF ... THEN ... ELSE ... Control Structure Available in Access Conditions</td>
            <td>Control structures IF ... THEN ... ELSE ... can now be used as part of an access condition after the DEFINE ROLE statement. </td>
        </tr>
<tr>
            <td>CDS Interface Behavior Definitions</td>
            <td>A new implementation type is available: the RAP interface behavior definition. Such interface BDEFs are based on CDS transactional interfaces and define the behavior of a RAP BO interface. The overall purpose of a RAP BO interface is to project a business object for stable consumption. </td>
        </tr>
<tr>
            <td>Managed RAP BO, New Field Characteristic</td>
            <td>In managed RAP business objects, a new field characteristic is available: field(suppress). It removes the field in question from the BDEF derived types and from all RAP APIs. </td>
        </tr>
<tr>
            <td>Managed RAP BO, with full data</td>
            <td>In managed RAP business objects, the optional addition with full data can be used in combination with one of the RAP saving options to ensure that full instance data is handed over to the save_modified method of the RAP saver class in the ABAP behavior pool. </td>
        </tr>
<tr>
            <td>Abstract BDEF, New Field Characteristic</td>
            <td>In RAP abstract behavior definitions, a new RAP field characteristic is available: mandatory:execute. It declares the field in question as mandatory. Whenever the hierarchical type is used as input parameter for a RAP action or a RAP function, a value must be supplied. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_SQL</td>
            <td>String Functions Processed by the ABAP SQL In-Memory Engine</td>
            <td>The string functions LEFT, LOWER, RIGHT, UPPER, and SUBSTRING can be processed by the ABAP SQL in-memory engine now. They no longer bypass table buffering and do not cause the transport of an internal table accessed by FROM @itab to the database. While LEFT, LOWER, RIGHT, and UPPER were not supported at all, SUBSTRING was supported under certain conditions. </td>
        </tr>
<tr>
            <td>Table Buffering for Decimal Floating Point Calculations</td>
            <td>Calculations for the built-in types DECFLOAT16 and DF16_DEC no longer bypass table buffering. The calculation in the table buffer is done with the respective decimal floating point arithmetic. The following differences to ABAP and to the database must be considered: In ABAP, the calculation type for decfloat16 is decfloat34. On the database, the built-in type DF16_DEC is handled as a packed number. Both can lead to different results. </td>
        </tr>
<tr>
            <td rowspan="1">MISC</td>
            <td>Import of Type n into Type c</td>
            <td>The statement IMPORT now allows assignments of data of type n to data objects of type c if they have the same length. This change might lead to incompatible behavior if handling of the former exception leads to different results than the new behavior. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 787 (2202)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="5">ABAP_CDS</td>
            <td>Comparisons with CDS Amount Fields and CDS Quantity Fields</td>
            <td>Special handling for CDS amount fields and CDS quantity fields for comparisons in CDS view entities has been implemented. </td>
        </tr>
<tr>
            <td>CDS View Entity, Extended Cast Matrix</td>
            <td>New casting options have been added in CDS view entities. Casting from data type SSTRING into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data type CHAR into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data type DATS into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data type TIMS into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data types DECFLOAT16 and DECFLOAT34 into data type CHAR is now possible. </td>
        </tr>
<tr>
            <td>CDS View Entity, Table Buffering</td>
            <td>Table buffering can be defined for CDS view entities now: Table buffering can be enabled and disabled with the annotation @AbapCatalog.entityBuffer.definitionAllowed: true|false The buffering type can be specified with a CDS entity buffer defined by DEFINE VIW ENTITY BUFFER ON cds_view_entity ... Table buffering can be defined differently for the layers core, localization, industry, partner and customer. </td>
        </tr>
<tr>
            <td>CONTEXT_NODE_VALUES, New Parameter datatype</td>
            <td>The access control context function CONTEXT_NODE_VALUES now has a further optional keyword parameter: datatype. This parameter specifies a DDIC data element that is used to represent the context node values at runtime before they are used in the comparison operations. </td>
        </tr>
<tr>
            <td>Unmanaged RAP BO, New Field Characteristic</td>
            <td>In unmanaged RAP business objects, a new field characteristic is available: field(suppress). It removes the field in question from the BDEF derived types and from all RAP APIs. </td>
        </tr>
<tr>
            <td rowspan="1">EML</td>
            <td>Entity Manipulation Language</td>
            <td>RAISE ENTITY EVENT Raises a RAP business event. </td>
        </tr>
<tr>
            <td rowspan="3">ITAB</td>
            <td>New Addition STEP for Defining Step Size and Processing Order</td>
            <td>The new addition STEP defines the step size and the order for processing an internal table. For the statements LOOP and FOR, STEP can be used to control the step size and the processing order. For the statements APPEND, DELETE, INSERT, VALUE, and NEW, STEP can only be used to define the step size. It is not possible to change the processing order with STEP for these statements. </td>
        </tr>
<tr>
            <td>Exception when Mixing Index Access with Hash Key Access</td>
            <td>Access to a table index when accessing an internal table using a hash key (accessing a hashed table using its primary key or accessing any internal table using a hashed secondary key) is not allowed. When a hashed key is specified dynamically behind USING KEY in statement LOOP AT or expression FOR ... IN, usage of FROM and TO must result in an exception. This was not the case before release 7.87. From release 7.87 on, the runtime error ITAB_ILLEGAL_INDEX_OP occurs in such a situation. Before release 7.87, the behavior was undefined. This change is slightly incompatible. </td>
        </tr>
<tr>
            <td>Correction for FROM Addition</td>
            <td>When a negative value is specified for FROM in statement LOOP AT or expression FOR ... IN, it is set to 1 implicitly. Before release 7.87, this was not the case in the following situation: The internal table is accessed using a sorted key. The internal table contains more than 10 table lines. A WHERE condition is specified that can be optimized. The loop was not processed at all. From release 7.87 on, the loop is processed as documented. This change is slightly incompatible. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 786 (2111)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="9">ABAP_CDS</td>
            <td>CDS View Entity, Extended Cast Matrix</td>
            <td>In CDS view entities, casting from data type FLTP into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. </td>
        </tr>
<tr>
            <td>CDS View Entity, Application Session Variables</td>
            <td>In CDS view entities, two new application session variables are available: bs_system_id bs_zone_id </td>
        </tr>
<tr>
            <td>CDS Analytical Projection Views</td>
            <td>CDS analytical projection views for modelling analytical queries are available. A CDS analytical projection view is defined using DEFINE TRANSIENT VIEW ENTITY AS PROJECTION ON. The value for the provider contract must be set to ANALYTICAL_QUERY. </td>
        </tr>
<tr>
            <td>CDS Transactional Interface</td>
            <td>A new type of CDS projection view is available: the CDS transactional interface. CDS transactional interfaces serve as stable public interface layer in a CDS data model. They are typically used in the context of the ABAP RESTful Application Programming Model to provide the basis for a RAP BO interface. A CDS transactional interface view is defined using DEFINE VIEW ENTITY AS PROJECTION ON. The value for the provider contract must be set to TRANSACTIONAL_INTERFACE. </td>
        </tr>
<tr>
            <td>Support of Input Parameters of Type abap.string</td>
            <td>Data type abap.string is now supported for input parameters in the parameter list of a CDS view entity and in the parameter list of a CDS table function. Data type abap.string is now supported when binding actual parameters to the input parameters of a CDS view entity, if a CDS table function is used as data source after FROM. </td>
        </tr>
<tr>
            <td>CDS Access Control Context</td>
            <td>An access control context is a hierarchical data structure with nodes that can have other nodes and values as child nodes. It provides access roles with specific information about the type of data selection that is being performed. Two functions are available to query the access control context: CONTEXT_NODE_EXISTS CONTEXT_NODE_VALUES Note: The access control context and the relating functions are available only internally at SAP. </td>
        </tr>
<tr>
            <td>RAP Late Numbering for Managed and for Draft-Enabled RAP BOs</td>
            <td>RAP late numbering is now also available for managed RAP BOs, managed draft-enabled RAP BOs, and for unmanaged draft-enabled RAP BOs. </td>
        </tr>
<tr>
            <td>Instance-Bound Factory Actions for Managed RAP BOs</td>
            <td>Instance-bound factory actions are now also available for managed RAP BOs. </td>
        </tr>
<tr>
            <td>BDEF Privileged Mode for RAP Projection BOs</td>
            <td>A new syntax variant of with privileged mode is now available for RAP projection BOs. </td>
        </tr>
<tr>
            <td rowspan="1">MISC</td>
            <td>Dynamic Component</td>
            <td>The following syntax can be used for the structure component selector - to access components dynamically now: ... struct-(comp) </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 785 (2108)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="16">ABAP_CDS</td>
            <td>CDS Abstract Entity, To-Parent Association</td>
            <td>In CDS abstract entities, it is now possible to define to-parent associations without ON condition. The ON condition can be left out if the association target is also a CDS abstract entity. </td>
        </tr>
<tr>
            <td>CDS View Entity, New Set Operators</td>
            <td>In CDS view entities, two new set operators are available: EXCEPT INTERSECT </td>
        </tr>
<tr>
            <td>CDS View Entity, Extended Expression Matrix</td>
            <td>In CDS view entities, the expression matrix has been extended: In a searched case expression, arithmetic expressions and case expressions are supported as operands. </td>
        </tr>
<tr>
            <td>Migration Tool for CDS Views</td>
            <td>The following documented ABAP program is now available for migrating CDS DDIC-based views to CDS view entities: RUTDDLSV2MIGRATION. </td>
        </tr>
<tr>
            <td>New AbapCatalog Annotations</td>
            <td>The following new AbapCatalog.extensibility annotations have been released: AbapCatalog.extensibility.allowNewdataSources AbapCatalog.extensibility.dataSources AbapCatalog.extensibility.elementSuffix AbapCatalog.extensibility.extensible AbapCatalog.extensibility.quota.maximumBytes AbapCatalog.extensibility.quota.maximumFields </td>
        </tr>
<tr>
            <td>Using CDS-Managed DDIC Views is Obsolete</td>
            <td>For each CDS DDIC-based view (obsolete), a CDS-managed DDIC view (obsolete) is created in ABAP Dictionary upon activation. Using this CDS-managed DDIC view (obsolete), for example for typing or for accessing the CDS entity in question, is obsolete from now on. The reason is the release of a new type of CDS entity, the CDS view entity, which will replace CDS DDIC-based views (obsolete) in the future. CDS view entities do not have a CDS-managed DDIC view (obsolete) attached and in case of a migration from CDS DDIC-based view (obsolete) to CDS view entity, each usage of a CDS-managed DDIC view (obsolete) leads to a syntax error. </td>
        </tr>
<tr>
            <td>Graceful Behavior in Access Condition Inheritance for Missing Dictionary Elements</td>
            <td>From this release on, authors of access controls can mark a subset of the CDS elements used in their access conditions as optional, so that CDS entities which inherit their access conditions are not affected by a Day-1-impact any more. GRANT SELECT ON cds_entity WITH OPTIONAL ELEMENTS (      element1 DEFAULT (TRUE|FALSE), ...) WHERE ...  </td>
        </tr>
<tr>
            <td>Unified Syntax for Authorization Objects, Authorization Fields and SACF Scenario Names</td>
            <td>At all locations of access controls, references to authorization objects, authorization fields, and SACF scenario names can be written in identifier syntax when they comply with it and in string syntax with single apostrophes as an alternative. Before: ( ... ) = ASPECT PFCG_AUTH( S_OBJ IN SCENARIO 'THESCENARIO' ) Now allowed: ( ... ) = ASPECT PFCG_AUTH( S_OBJ IN SCENARIO TheScenario )  </td>
        </tr>
<tr>
            <td>Additional Filtering of User-Defined Aspect Usage</td>
            <td>When using user-defined aspects, these can now denominate an arbitrary set of their elements (path expressions are supported) as filter element. These filter elements can by referenced when using the user-defined aspect in an access condition. DEFINE ASPECT ... AS SELECT FROM ...   WITH USER ELEMENT ...   WITH FILTER ELEMENTS ( element1, element2 AS alias2 )   {       ...   } WHERE ( ... ) = ASPECT ... FILTER BY ( element1 = 'X' OR                                        alias2 IS NOT NULL )  </td>
        </tr>
<tr>
            <td>Condition Replacement for Role-Based Inheritance</td>
            <td>The REPLACING section formerly only available to entity-based inheritance INHERITING CONDITIONS FROM ENTITY cds_entity is now also available for role-based inheritance INHERIT role FOR GRANT SELECT ON cds_entity  </td>
        </tr>
<tr>
            <td>Generic Element Replacement for Condition Inheritance</td>
            <td>The REPLACING section of condition inheritance now supports a generic replacement step to replace an arbitrary field or association of the inheritance source with an arbitrary field or association of the inheritance target. WHERE INHERITING CONDITIONS FROM ENTITY Source REPLACING {   ELEMENT Element1OfSource WITH Element1OfTarget,   ELEMENT Assoc1OfSource WITH Assoc1[r = 4].Assoc2OfTarget,   ELEMENT Assoc2(p : $parameters.p1)[ q = 1].Field WITH MyShortField }  </td>
        </tr>
<tr>
            <td>Enabling/Disabling of Access Conditions Based on the State of SACF</td>
            <td>Respecting settings in the switchable authorization framework (SACF) was already possible for dedicated PFCG conditions: ASPECT PFCG_AUTH ( S_OBJECT IN SCENARIO ... ) Now, those settings can be used to enable or disable entire condition sets: GRANT SELECT ON cds_entity   WHERE     SACF_CHECK_IN_USE (NAME =&gt; NEW_AUTH_SWITCH ) IS INITIAL       AND     ( element ) = ASPECT PFCG_AUTH( OLD_AUTH, F )   OR     SACF_CHECK_IN_USE( NAME =&gt; NEW_AUTH_SWITCH ) IS NOT INITIAL       AND     ( element ) = ASPECT PFCG_AUTH( NEW_AUTH, F );  </td>
        </tr>
<tr>
            <td>Access Conditions for CDS Hierarchies Based on Elements of the Hierarchy Directory</td>
            <td>For CDS hierarchies, access control was restricted to the use of conditions not resulting in database filtering. Now elements located in the declared hierarchy directory DIRECTORY ... FILTER BY can be used to formulate such conditions. </td>
        </tr>
<tr>
            <td>New DCL Function</td>
            <td>The following new DCL function is available: OPTIONAL_ELEMENT_EXISTS. </td>
        </tr>
<tr>
            <td>RAP BO Operation Addition authorization:update</td>
            <td>The new RAP BO operation addition authorization:update is available for managed and unmanaged BOs. It delegates the authorization control for an operation to the authorization control that is specified for the update operation. </td>
        </tr>
<tr>
            <td>Abstract BDEF, with control</td>
            <td>The optional addition with control is available for abstract BDEFs. It adds a %control structure to the corresponding derived type structure. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_OBJECTS</td>
            <td>Dynamic Target</td>
            <td>The following syntax can be used for the object component selector -&gt; to access components and attributes dynamically now: ... { dref-&gt;(comp_name) }   | { cref-&gt;(attr_name) }   | { iref-&gt;(attr_name) } ... Before, this was possible for dynamic components and dynamic access in the statement ASSIGN only. </td>
        </tr>
<tr>
            <td rowspan="1">ABP</td>
            <td>ABAP Behavior Pools (ABP)</td>
            <td>ABAP Behavior Pools (ABP) are now available as part of the ABAP Keyword Documentation. The following topics are covered: CLASS, FOR BEHAVIOR OF RAP Handler Class METHODS, FOR RAP Saver Class finalize check_before_save adjust_numbers save cleanup cleanup_finalize save_modified </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>New Parameter for CL_ABAP_CORRESPONDING=&gt;EXECUTE</td>
            <td>The method EXECUTE of the system class CL_ABAP_CORRESPONDING has a new parameter KEEPING_LINES. It has the same effect as the additions KEEPING TARGET LINES in MOVE-CORRESPONDING or BASE in CORRESPONDING. </td>
        </tr>
<tr>
            <td rowspan="1">DDIC</td>
            <td>C0 Developer Extensibility for DDIC Objects</td>
            <td>The following new extensibility annotations are available for dictionary objects. They are required for C0 release of a DDIC object, which is a prerequisite for developer extensibility. @AbapCatalog.enhancement.fieldSuffix @AbapCatalog.enhancement.quotaMaximumFields @AbapCatalog.enhancement.quotaMaximumBytes @AbapCatalog.enhancement.quotaShareCustomer @AbapCatalog.enhancement.quotaSharePartner </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Access to Generically Typed Internal Tables</td>
            <td>Earlier, in statements for accessing internal tables, the internal table had to be known statically. The operand had to be typed at least with any table. Now, this restriction is partly removed. In the statements INSERT, APPEND, COLLECT, MODIFY, DELETE, READ, LOOP, and SORT, operands can be field symbols and formal parameters that are typed fully generically with TYPE data or TYPE any. Such operands can be used as if typed with any table. If an index access is involved, operands are still required that are typed at least with TYPE index_table. Hint This is not yet possible in expressions as FOR expressions or table expressions. Example The following was not possible in older releases. DATA itab TYPE TABLE OF scarr. FIELD-SYMBOLS &lt;itab&gt; TYPE ANY. ASSIGN itab TO &lt;itab&gt;. LOOP AT &lt;itab&gt; ASSIGNING FIELD-SYMBOL(&lt;fs&gt;).   ... ENDLOOP. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 784 (2105)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="9">ABAP_CDS</td>
            <td>CDS View Entity, Reusing Expressions</td>
            <td>In CDS view entities, it is now possible to reuse expressions defined in the SELECT list in other operand positions of the same CDS view entity by using the syntax $projection.reuse_exp. </td>
        </tr>
<tr>
            <td>CDS View Entity, Extended Expression Matrix</td>
            <td>In CDS view entities, the expression matrix has been extended: In the WHERE clause, arithmetic expressions and case expressions are supported as operands. In the HAVING clause, arithmetic expressions and case expressions are supported as operands. </td>
        </tr>
<tr>
            <td>CDS Abstract Entity Extensions</td>
            <td>The new statement EXTEND ABSTRACT ENTITY of the DDL of ABAP CDS makes it possible to add new elements to existing CDS abstract entities by using CDS abstract entity extensions. </td>
        </tr>
<tr>
            <td>CDS View Entity, Handling of Amounts and Quantities</td>
            <td>In CDS view entities, special handling for CDS amount fields and CDS quantity fields has been implemented. It differs from the handling of amount fields and quantity fields in DDIC, for example, more data types are possible and the currency key/unit key reference is considered in expressions. Moreover, a new type is available: the CDS calculated quantity. A CDS calculated quantity is the result type of a calculation using amount and/or quantity fields. </td>
        </tr>
<tr>
            <td>BDEF Strict Mode</td>
            <td>BDEF strict mode is now available. It can be enabled using the syntax addition strict and it applies additional syntax checks to RAP behavior definitions. </td>
        </tr>
<tr>
            <td>Projection BDEF, New Actions and Functions</td>
            <td>It is now possible to define and implement new actions and functions in a projection BDEF. </td>
        </tr>
<tr>
            <td>Projection BDEF, Authorization Concept</td>
            <td>It is now possible to define an authorization concept in a projection BDEF that controls access to the newly defined actions and functions in a projection BDEF. </td>
        </tr>
<tr>
            <td>Projection BDEF, Augmented Fields</td>
            <td>Field characteristics can be specified for augmented fields in a projection BDEF. </td>
        </tr>
<tr>
            <td>Projection BDEF, New Field Characteristic</td>
            <td>In projection BDEFs, a new field characteristic is available: field(suppress). It removes the field in question from the BDEF derived types and all RAP APIs. </td>
        </tr>
<tr>
            <td rowspan="4">ABAP_SQL</td>
            <td>Addition for the String Function REPLACE_REGEXPR</td>
            <td>The new parameter start can now be used in the function REPLACE_REGEXPR. Additionally, the parameter occurrence can now include expressions. </td>
        </tr>
<tr>
            <td>New String Function SUBSTRING_REGEXPR</td>
            <td>ABAP SQL now supports the new string function SUBSTRING_REGEXPR which supports regular expressions. </td>
        </tr>
<tr>
            <td>Byte Fields as Null Indicators</td>
            <td>The new addition INDICATORS ... NULL BITFIELD of the INTO clause of a SELECT statement allows a byte field type component to be specified as a null indicator. The single bits of the byte field serve for indicating null values in the result set of the query. For this purpose, condensed indicator structures can be declared with the addition AS BITFIELD of the TYPES statement. </td>
        </tr>
<tr>
            <td>Position of Null Indicators</td>
            <td>If CORRESPONDING FIELDS is used in the INTO clause of a SELECT statement, a null indicator defined by INDICATORS can be positioned anywhere in the target area. Otherwise, it must be the last component. </td>
        </tr>
<tr>
            <td rowspan="1">BYTE</td>
            <td>Writable Expression in SET BIT</td>
            <td>For the operand byte_string of statement SET BIT, a writable expression can be specified now. </td>
        </tr>
<tr>
            <td rowspan="1">EML</td>
            <td>Entity Manipulation Language</td>
            <td>ABAP EML keywords are now available as part of the ABAP Keyword Documentation. The following keywords and topics are covered among others: MODIFY ... MODIFY ENTITY MODIFY ENTITIES MODIFY ENTITIES OPERATIONS READ ... READ ENTITY READ ENTITIES READ ENTITIES OPERATIONS COMMIT ENTITIES ... COMMIT ENTITIES COMMIT ENTITIES RESPONSE OF COMMIT ENTITIES RESPONSES OF COMMIT ENTITIES BEGIN, END including CONVERT KEY OF ROLLBACK ENTITIES GET PERMISSIONS ... GET PERMISSIONS GET PERMISSIONS OF GET PERMISSIONS OPERATIONS SET LOCKS ... SET LOCKS ENTITY SET LOCKS OF SET LOCKS (dynamic form) Type Mapping for RAP SET NAMES SET FLAGS RAP-specific variants of the CORRESPONDING operator </td>
        </tr>
<tr>
            <td rowspan="1">REFERENCES</td>
            <td>Dereferencing Data References</td>
            <td>The dereferencing operator-&gt;* can be used for generically typed data reference variables in almost all operand positions now. Before, that was possible in the ASSIGN statement only. </td>
        </tr>
<tr>
            <td rowspan="2">TYPES</td>
            <td>Condensed Indicator Structures</td>
            <td>The new addition AS BITFIELD behind addition INDICATORS of the statement TYPES allows a condensed indicator structure to be defined as a byte field type component of a given structured type. An condensed indicator structure can be used as a null indicator in ABAP SQL statements. </td>
        </tr>
<tr>
            <td>BDEF Derived Types</td>
            <td>BDEF derived types are now available as part of the ABAP keyword documentation. The following keywords and topics are covered: TYPE TABLE FOR TYPE STRUCTURE FOR TYPE RESPONSE FOR TYPE REQUEST FOR Components of BDEF Derived Types Declaration of Variables with BDEF Derived Types </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 783 (2102)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="8">ABAP_CDS</td>
            <td>CDS View Entity, UNION Clause</td>
            <td>UNION clauses are now supported in CDS view entities. There are a few differences to UNION clauses in CDS DDIC-based views. The most important difference is that branches of union clauses can be nested within each other in CDS view entities. </td>
        </tr>
<tr>
            <td>CDS View Entity, DISTINCT</td>
            <td>The addition DISTINCT is now available for SELECT statements in CDS view entities. </td>
        </tr>
<tr>
            <td>CDS View Entity, New Conversion Functions</td>
            <td>Two new conversion functions are available in CDS view entities: GET_NUMERIC_VALUE CURR_TO_DECFLOAT_AMOUNT </td>
        </tr>
<tr>
            <td>CDS View Entity, Typed Literals</td>
            <td>Typed literals are now available for CDS view entities. Typed literals allow an explicit type declaration and they are available for many built-in ABAP Dictionary data types. </td>
        </tr>
<tr>
            <td>CDS Projection View, PROVIDER CONTRACT</td>
            <td>It is now possible to specify a provider contract for CDS projection views using the keyword PROVIDER CONTRACT. The provider contract specifies in which scenario a CDS projection view is used, and the scenario in turn determines in which runtime the view is executed and which features are available. In this release, there is only one provider contract option available: TRANSACTIONAL_QUERY. </td>
        </tr>
<tr>
            <td>Unmanaged Early Numbering in Managed BOs</td>
            <td>The entity behavior characteristic early numbering can be used to define unmanaged early numbering for all primary key fields of a managed RAP BO. </td>
        </tr>
<tr>
            <td>Implementing Cleanup in Managed BOs</td>
            <td>The new addition and cleanup is available for additional and unmanaged save in managed RAP BOs. It allows the application developer to implement the cleanup method. </td>
        </tr>
<tr>
            <td>CDS Abstract Behavior Definitions</td>
            <td>A new implementation type is available: the RAP abstract behavior definition. Such abstract BDEFs mainly serve as typing mechanism for deep action or function parameters. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_DOCU</td>
            <td>Configuration of the Documentation</td>
            <td>From release 7.83 on, important settings of the ABAP Keyword Documentation can be configured explicitly. Before, the documentation was configured implicitly from system settings. The configuration of the ABAP Keyword Documentation is saved in customizing table ABDOCCONFIG, that is maintained with transaction code ABAP_DOCU_CONFIG (based on executable program ABAP_DOCU_CONFIG) or with the configuration entity ABAP_DOCU_CONFIG_ENTITY. All repository objects related to the configuration are documented. The following properties can be set: CP If set to X, the ABAP Keyword Documentation uses terms appropriate for the SAP BTP ABAP environment (aka Steampunk) instead of Application Server ABAP in some footers and copyright texts. Recommendation: Set to X in systems of the SAP BTP ABAP environment. Set to initial in all other systems. ICF Enables or disables the Web Version of the ABAP Keyword Documentation based on ICF nodes /sap/public/bc/abap/docu and /sap/bc/abap/docu. The following settings can be applied: If set to X, the Web Version is enabled. The execution of example programs from the Web Version is disabled. If set to E, the Web Version is enabled. The execution of example programs from the Web Version is also enabled. Recommendation: Set to E in SAP development systems. Set to E in customer development systems. Set to X in SAP S/4HANA. Set to initial in SAP BTP ABAP environment and SAP S/4HANA Cloud. BATCH If set to X, the infrastructure of the ABAP Keyword Documentation is allowed to start batch jobs that prepare buffers that are needed for search in and display of the documentation. Recommendation: Set to X in SAP S/4HANA. Set to initial in SAP BTP ABAP environment. MAILBOX If set to X, the ABAP Keyword Documentation display offers a function to send a feedback mail to abap.docu@exchange.sap.corp. This mailbox is only available to SAP employees. Therefore, the setting makes sense for SAP's own development systems only. Recommendation: Set to X in SAP's own development systems. Set to initial in all other systems. VERSION Determines the ABAP language version for which the ABAP Keyword Documentation is displayed. Allowed values are defined in domain ABAPVRS. This setting can be overridden when calling the documentation using the respective APIs: CL_ABAP_DOCU for the SAP GUI version CL_ABAP_DOCU_EXTERNAL for the ADT or Web version. Calls to the documentation from ABAP Workbench and ADT set the language version depending on the object currently being edited. Calls to the documentation via the ABAPDOCU and ABAPHELP transactions use the version from ABDOCCONFIG by default. An initial value for VERSION in ABDOCCONFIG displays the documentation for Standard ABAP (X). Recommendation: Set to X in SAP S/4HANA Set to 5 in SAP BTP ABAP environment and SAP S/4HANA Cloud Program ABAP_DOCU_CONFIG allows a choice to be made between different sets of parameters: Parameter set typical for SAP development systems Parameter set typical for customer development systems Parameter set typical for SAP S/4HANA systems Parameter set typical for SAP S/4HANA Cloud ABAP environment Parameter set typical for SAP BTP ABAP environment Current parameter set The passed parameters are used as default values for a dialog window and can be overridden there. If customizing table ABDOCCONFIG is initial in a customer system, the first call of the ABAP Keyword documentation supplies it with values recommended for an SAP S/4HANA System. In SAP's own systems, values for a SAP development system are supplied. If the table is partly filled, default values are used for the missing rows. These values are initial for all properties except VERSION. If the language version is not passed by the caller and cannot be found in ABDOCCONFIG, the value X is used and a warning is shown in the documentation display. </td>
        </tr>
<tr>
            <td rowspan="6">ABAP_SQL</td>
            <td>New String Function</td>
            <td>ABAP SQL now supports the new string function INITCAP. </td>
        </tr>
<tr>
            <td>New Date and Time Functions</td>
            <td>The following new generic date and time functions are available: SQL Function Date Time Time Stamp IS_VALID x x x EXTRACT_YEAR x - x EXTRACT_MONTH x - x EXTRACT_DAY x - x EXTRACT_HOUR - x x EXTRACT_MINUTE - x x EXTRACT_SECOND - x x DAYNAME x - x MONTHNAME x - x WEEKDAY x - x DAYS_BETWEEN x - x ADD_DAYS x - x ADD_MONTHS x - x </td>
        </tr>
<tr>
            <td>New Casts</td>
            <td>The following new casts are available: Source type Numeric target type Character-like target type Date/time field as target type CHAR, SSTRING, DATS, TIMS INT1, INT2, INT4, INT8, DEC, CURR, QUAN, DECFLOAT16, DECFLOAT34, FLTP - - FLTP INT1, INT2, INT4, INT8, DEC, CURR, QUAN, DECFLOAT16, DECFLOAT34 CHAR, SSTRING - DF16_DEC, DF34_DEC FLTP - - DATN - - DATS TIMN - - TIMS </td>
        </tr>
<tr>
            <td>New Set Operators</td>
            <td>ABAP SQL now supports the new set operators INTERSECT and EXCEPT. </td>
        </tr>
<tr>
            <td>New Function for Unit Conversion</td>
            <td>ABAP SQL now supports the new function UNIT_CONVERSION for unit conversions. </td>
        </tr>
<tr>
            <td>New Expression Null</td>
            <td>ABAP SQL now supports the new expression NULL. </td>
        </tr>
<tr>
            <td rowspan="2">ASSIGNMENTS</td>
            <td>New Additions for MOVE-CORRESPONDING</td>
            <td>It is now possible to specify MOVE-CORRESPONDING statements with the following additions in the context of nested tables in deep structures. Both ensure that nested tables of the deep target structures are not deleted and new lines of nested tables in deep source structures are added: KEEPING TARGET LINES EXPANDING NESTED TABLES KEEPING TARGET LINES </td>
        </tr>
<tr>
            <td>New Additions for the Component Operator CORRESPONDING</td>
            <td>It is now possible to specify statements with the component operator CORRESPONDING with the following additions in the context of nested tables in deep structures. Both ensure that the nested tables of deep target structures are not deleted and new lines of nested tables in deep source structures are added: APPENDING BASE DEEP APPENDING BASE </td>
        </tr>
<tr>
            <td rowspan="1">DDIC</td>
            <td>Extension for Load Unit</td>
            <td>In ABAP Dictionary, the load unit has been extended. There are now the following settings available: Column Preferred Page Preferred Column Enforced Page Enforced </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 782 (2011)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="9">ABAP_CDS</td>
            <td>CDS Hierarchy, Caching Policy</td>
            <td>A hierarchy definition DEFINE HIERARCHY can now use the new addition CACHE ON|OFF|FORCE to specify the caching policy for a generated hierarchy. </td>
        </tr>
<tr>
            <td>CDS Projection View, REDEFINE ASSOCIATION</td>
            <td>In CDS projection views, it is now possible to redefine the CDS associations from the projected entity in the header part. This is done using the keyword REDEFINE ASSOCIATION. Redefinition can include a new filter, alias name, and redirection to a new association target, which must also be a CDS projection view, thus moving the complete data model to the projection layer. </td>
        </tr>
<tr>
            <td>ABAP Program for Migration Analysis</td>
            <td>The following documented ABAP program is now available for evaluating whether a migration from a CDS DDIC-based view (obsolete) to a CDS view entity is possible: RUTDDLS_MIGRATION_CANDIDATES. </td>
        </tr>
<tr>
            <td>Nested Determinations on Modify</td>
            <td>It is now possible to trigger a determination on modify by another determination on modify. </td>
        </tr>
<tr>
            <td>Always Flag in Determine Actions</td>
            <td>The new addition always can be used for determinations and validations assigned to a determine action. When the determine action is called, determinations and validations with this flag are executed regardless of their trigger conditions. </td>
        </tr>
<tr>
            <td>Global Feature Control</td>
            <td>The new RAP BO operation addition features:global can be used to define global feature control for RAP BO operations. </td>
        </tr>
<tr>
            <td>Global Authorization Control</td>
            <td>Global authorization control is available. It can be defined using authorization master (global). </td>
        </tr>
<tr>
            <td>Non-Locking Actions</td>
            <td>The new RAP BO operation addition lock:none can be used to suppress the locking mechanism for an action. </td>
        </tr>
<tr>
            <td>Projection BDEF, Operation Augment</td>
            <td>For projections BDEFs, the operation augment is available. Augmentation enhances incoming requests with a custom implementation, for example with default values. </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Alias Names for Secondary Keys</td>
            <td>Alias names can now be declared for secondary keys of internal tables by using the addition ALIAS of TYPES and DATA. This can be helpful when changing existing secondary keys without invalidating users. </td>
        </tr>
<tr>
            <td rowspan="2">STRINGS</td>
            <td>XPath and XSD Regular Expressions</td>
            <td>Besides the existing support of PCRE regular expressions and POSIX regular expressions (obsolete) ABAP supports now also XPath regular expressions and XSD regular expressions. Internally, those are transformed to PCRE regular expressions and processed by the PCRE2 Library. Both kinds of regular expressions can be used by the new (factory) methods CREATE_XPATH2 and CREATE_XSD of the system classes CL_ABAP_REGEX and CL_ABAP_MATCHER. XPath regular expressions can be used by the new parameter xpath in some built-in functions. </td>
        </tr>
<tr>
            <td>Callouts in PCRE Regular Expressions</td>
            <td>The class CL_ABAP_MATCHER supports callouts in PCRE syntax now. The method SET_CALLOUT can be used to register a handler class that implements the interface IF_ABAP_MATCHER_CALLOUT. The special characters (?C...) of a PCRE regular expression then call the interface method CALLOUT when the method MATCH is executed. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 781 (2008)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="7">ABAP_CDS</td>
            <td>New Session Variables for User Time Zone and User Date</td>
            <td>Two new session variables are available in ABAP CDS: $session.user_timezone, which returns the time zone defined in the user master record for the local user time. $session.user_date, which returns the local date of a user. </td>
        </tr>
<tr>
            <td>New String Function for Regular Expressions</td>
            <td>ABAP CDS now supports the new string function REPLACE_REGEXPR that allows regular expressions to be replaced. </td>
        </tr>
<tr>
            <td>Quantifier Operators</td>
            <td>With the new addition ALL, access conditions can express that access shall only be granted when from a set-valued association all values satisfy the condition. WHERE ALL toItem.State = 'A' To accompany this use case, the BYPASS WHEN operator has been extended to literal conditions also. A dedicated operator EXISTS can be used when different access conditions using the same set-valued association shall not be coupled in their fields by means of a common join expression.  </td>
        </tr>
<tr>
            <td>Draft Support for RAP BOs</td>
            <td>The new statement with draft can be used to enable the draft concept for a RAP BO. </td>
        </tr>
<tr>
            <td>Determine Actions</td>
            <td>Determine actions are a new type of action defined using determine action. With a determine action, determinations and validations can be executed on request. </td>
        </tr>
<tr>
            <td>Precheck for Modify Operations</td>
            <td>The new RAP BO operation addition precheck can be used to prevent illegal changes from reaching the application buffer by prechecking modify operations. </td>
        </tr>
<tr>
            <td>New Field Characteristics</td>
            <td>RAP BDL now supports the following new field characteristics: mandatory:create readonly:update </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_SQL</td>
            <td>New String Functions</td>
            <td>ABAP SQL now supports the new string functions REPLACE_REGEXPR, LIKE_REGEXPR, and OCCURRENCES_REGEXPR, which support regular expressions. </td>
        </tr>
<tr>
            <td>Addition to the UPDATE FROM Clause</td>
            <td>Set indicators can now be used as additions after the UPDATE FROM clause to indicate columns for updating. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Calculation Assignments in Constructor Operator REDUCE</td>
            <td>In the assignments behind the addition NEXT of the constructor operator REDUCE. The calculation assignment operators +=, +=, *=, /= or &amp;&amp;= can be used now and the respective rules apply. </td>
        </tr>
<tr>
            <td rowspan="1">STRINGS</td>
            <td>Exception CX_SY_STRING_SIZE_TOO_LARGE in Transformations</td>
            <td>The exception of exception class CX_SY_STRING_SIZE_TOO_LARGE that occurs, when an operation with a string exceeds its maximum size can now also be handled for the statement CALL TRANSFORMATION if some conditions are met. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>Indicator Structures</td>
            <td>The new addition INDICATORS of the statement TYPES allows an indicator structure to be defined as a substructure of a given structured type. An indicator structure can be used as a ABAP SQL indicator in ABAP SQL read and write statements. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 780 (2005)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_CDS</td>
            <td>Defining Associations in CDS Projection Views</td>
            <td>It is now possible to define new associations with external data sources in CDS projection views. </td>
        </tr>
<tr>
            <td>CDS View Entities</td>
            <td>A new kind of CDS view is available: the CDS view entity. CDS view entities represent an improved version of CDS DDIC-based views (obsolete) (DEFINE VIEW). They serve the same purpose and have the same structure as CDS DDIC-based views (obsolete), but offer many advantages. CDS view entities are planned to replace CDS DDIC-based views (obsolete) in the future. A CDS view entity is defined with the statement DEFINE VIEW ENTITY. </td>
        </tr>
<tr>
            <td>Unmanaged Lock in Managed RAP BOs</td>
            <td>The new statement lock master unmanaged can be used if the application developer wants to implement an own locking mechanism in a managed RAP BO. An own locking mechanism can be used instead of the RAP locking mechanism provided by the RAP framework. </td>
        </tr>
<tr>
            <td rowspan="4">ABAP_SQL</td>
            <td>Further Data Types Allowed in Elementary SQL Expressions</td>
            <td>Elementary SQL expressions can now also have the dictionary data types STRING and RAWSTRING. </td>
        </tr>
<tr>
            <td>New Type Conversion Function to_blob</td>
            <td>ABAP SQL now supports the new type conversion function to_blob. </td>
        </tr>
<tr>
            <td>Hierarchy Load Options</td>
            <td>The hierarchy generator HIERARCHY can now use the new addition LOAD BULK|INCREMENTAL|load_option to specify the load policy for a generated hierarchy. </td>
        </tr>
<tr>
            <td>Typed Literals</td>
            <td>Typed literals for many ABAP Dictionary types are now available in ABAP SQL. </td>
        </tr>
<tr>
            <td rowspan="1">DDIC</td>
            <td>Load Unit</td>
            <td>In ABAP Dictionary, a new technical setting for database tables is available: the load unit. It specifies how the data of the table is loaded into the main memory of the SAP HANA database. It can be used to reduce the memory consumption in the HANA database server. </td>
        </tr>
<tr>
            <td rowspan="4">STRINGS</td>
            <td>Support of Perl Compatible Regular Expressions</td>
            <td>Besides the existing support for POSIX regular expressions (now obsolete), ABAP supports now also PCRE regular expressions that are processed by the PCRE2 Library implemented in the ABAP Kernel. PCRE regular expressions can be used in the same way as POSIX regular expressions. The following distinctions exist: The new addition PCRE can be used instead of REGEX in the statements FIND and REPLACE. The new parameter pcre can be used instead of regex in built-in functions. The new (factory) methods of the system classes CL_ABAP_REGEX and CL_ABAP_MATCHER. PCRE regular expressions are more powerful and have better performance than POSIX regular expressions. For more information, see Regular Expressions. </td>
        </tr>
<tr>
            <td>Verbatim Replacements</td>
            <td>The new addition VERBATIM of the REPLACE statement causes all characters of the replacement string to be taken literally. If this addition is used, special characters for regular expression replacement patterns have no special meaning. </td>
        </tr>
<tr>
            <td>New Catchable Exception CX_SY_STRING_SIZE_TOO_LARGE</td>
            <td>The exception that occurs when an operation with a string exceeds its maximum size is now connected to the exception class CX_SY_STRING_SIZE_TOO_LARGE and can be handled. Previously, it always resulted in runtime error STRING_SIZE_TOO_LARGE. </td>
        </tr>
<tr>
            <td>Formatting Option CURRENCY for Decimal Floating Point Numbers</td>
            <td>The formatting option CURRENCY in string templates can now also be applied to decimal floating point numbers. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 779 (2002)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>Managed Internal Numbering for Managed RAP BOs</td>
            <td>The new statement numbering:managed can be used to automatically generate values for primary key fields with a UUID. Available for managed RAP BOs. </td>
        </tr>
<tr>
            <td>New Options for Output Parameters</td>
            <td>For actions and functions, the output parameter can now be an entity or a structure. The addition selective can be used for an output parameter to return only parts of the result structure. </td>
        </tr>
<tr>
            <td rowspan="5">ABAP_SQL</td>
            <td>New Window Function NTILE</td>
            <td>ABAP SQL now supports the following new window function in window expressions: NTILE </td>
        </tr>
<tr>
            <td>SELECT, INTO target Modification</td>
            <td>When using SELECT, INTO target, host variables can now be declared inline even when the FROM clause is dynamic, as long as all fields of the SELECT list are known statically. Previously, the structure of the result set, including SELECT list, FROM clause, and any indicators needed to be static. </td>
        </tr>
<tr>
            <td>New Type Conversion Function to_clob</td>
            <td>ABAP SQL now supports the new type conversion function to_clob. </td>
        </tr>
<tr>
            <td>New Currency Conversion Function currency conversion</td>
            <td>ABAP SQL now supports the new currency conversion function currency_conversion. </td>
        </tr>
<tr>
            <td>Streaming and Locators Can Now Be Used on SQL Expressions</td>
            <td>Streaming and locators can now be used in combination with SQL expressions such as TO_CLOB, TO_BLOB, and AS_GEO_JSON. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 778 (1911)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="10">ABAP_CDS</td>
            <td>CDS View Entity Extensions</td>
            <td>The new statement EXTEND VIEW ENTITY of the DDL of ABAP CDS makes it possible to add new view fields to existing CDS views entities and CDS projection views by using CDS view entity extensions. </td>
        </tr>
<tr>
            <td>Check with element IS [NOT] INITIAL in a Literal Condition</td>
            <td>In a literal condition, IS [NOT] INITIAL can now be used to check whether the value of the left side matches (does not match) the initial value of the ABAP data type that matches the element. </td>
        </tr>
<tr>
            <td>Addition bypass when</td>
            <td>The addition BYPASS WHEN can now be used to specify a bypass condition for an element. If the condition is met, the element in question is not used for authorization filtering. </td>
        </tr>
<tr>
            <td>Check on the User Name in User Conditions</td>
            <td>When the user name is checked in user conditions, the following can now be checked instead of the user name: The value of the alias. The number of the business partner assigned to the user. </td>
        </tr>
<tr>
            <td>Data Types</td>
            <td>The operand that can be specified on the left side of a condition of an access rule of a CDS role can now have the built-in ABAP Dictionary data type RAW. </td>
        </tr>
<tr>
            <td>Aspect Bypass Conditions</td>
            <td>IS [NOT] INITIAL and IS INITIAL OR NULL are now possible as aspect bypass conditions. </td>
        </tr>
<tr>
            <td>Type Mapping</td>
            <td>The new statement mapping for can be used to map field names from legacy code to field names from the current data model. Available for unmanaged and managed RAP BOs. </td>
        </tr>
<tr>
            <td>Additional Save in Managed BOs</td>
            <td>The new statement with additional save can be used to enhance the default save sequence in a managed RAP BO. </td>
        </tr>
<tr>
            <td>Unmanaged Save in Managed BOs</td>
            <td>The new statement with unmanaged save can be used to implement an own saving strategy in a managed RAP BO. </td>
        </tr>
<tr>
            <td>Implementation Grouping</td>
            <td>The new statement group can be used to divide the implementation-relevant parts of a BO's business logic into several groups for behavior implementation. </td>
        </tr>
<tr>
            <td rowspan="10">ABAP_SQL</td>
            <td>New Aggregate Function ALLOW_PRECISION_LOSS</td>
            <td>ABAP SQL now supports the following new aggregate function in combination with the aggregate expression SUM: ALLOW_PRECISION_LOSS </td>
        </tr>
<tr>
            <td>Optional Window Frame Specification within a Window Function</td>
            <td>The optional window frame specification allows a subset of rows within a window, called a frame, to be defined. Frames are determined with respect to the current row, which enables the frame to move within a window. </td>
        </tr>
<tr>
            <td>New Window Functions FIRST_VALUE and LAST_VALUE</td>
            <td>ABAP SQL now supports the following new window functions in window expressions: FIRST_VALUE and LAST_VALUE. </td>
        </tr>
<tr>
            <td>New Date/Time Conversion Functions</td>
            <td>ABAP SQL now supports the following new date/time conversion functions: TSTMPL_TO_UTCL and TSTMPL_FROM_UTCL DATS_TO_DATN and DATS_FROM_DATN TIMS_TO_TIMN and TIMS_FROM_TIMN </td>
        </tr>
<tr>
            <td>New Time Stamp Functions</td>
            <td>ABAP SQL now supports the following new time stamp functions: UTCL_CURRENT, UTCL_ADD_SECONDS, and UTCL_SECONDS_BETWEEN. </td>
        </tr>
<tr>
            <td>New Date Functions</td>
            <td>ABAP SQL now supports the following new date functions: DATN_DAYS_BETWEEN, DATN_ADD_DAYS, and DATN_ADD_MONTHS. </td>
        </tr>
<tr>
            <td>New Additions After the ORDER BY Clause</td>
            <td>ABAP SQL now supports the following additions after the ORDER BY clause: NULLS FIRST and NULLS LAST. </td>
        </tr>
<tr>
            <td>New Aggregate Functions</td>
            <td>ABAP SQL now supports the following new aggregate functions: MEDIAN, STDDEV, VAR, CORR, and CORR_SPEARMAN. </td>
        </tr>
<tr>
            <td>New Geometry Conversion Function as_geo_json</td>
            <td>ABAP SQL now supports the following new geometry conversion function: as_geo_json </td>
        </tr>
<tr>
            <td>SQL Conditions Revised</td>
            <td>The following SQL conditions were revised: The operator IN can now be used with a subquery that returns value tuples. SQL functions and cast expressions can now be used as operands on the right side of comparison operators. </td>
        </tr>
<tr>
            <td rowspan="1">EXCEPTIONS</td>
            <td>Declaration of CX_NO_CHECK Exceptions</td>
            <td>Exceptions of category CX_NO_CHECK are always declared implicitly in interfaces of procedures and can always be propagated. Now it is also possible to declare exceptions of category CX_NO_CHECK with RAISING in procedure interfaces, for example for methods. This allows it to document the possible occurrence of such exceptions and to change the category of existing exceptions to CX_NO_CHECK without leading to syntax errors in procedure interfaces. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 777 (1908)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="16">ABAP_CDS</td>
            <td>New Date Functions</td>
            <td>ABAP CDS now supports the following new date functions: DATN_DAYS_BETWEEN DATN_ADD_DAYS DATN_ADD_MONTHS </td>
        </tr>
<tr>
            <td>New Time Stamp Functions</td>
            <td>ABAP CDS now supports the following new time stamp functions: UTCL_CURRENT UTCL_ADD_SECONDS UTCL_SECONDS_BETWEEN </td>
        </tr>
<tr>
            <td>New Date/Time Conversion Functions</td>
            <td>ABAP CDS now supports the following new date/time conversion functions: TSTMPL_TO_UTCL and TSTMPL_FROM_UTCL DATS_TO_DATN and DATS_FROM_DATN TIMS_TO_TIMN and TIMS_FROM_TIMN </td>
        </tr>
<tr>
            <td>Addition COMBINATION MODE OR|AND of the Statement GRANT SELECT ON</td>
            <td>The optional addition COMBINATION MODE can now be used to define how the access conditions of multiple CDS access rules for the same CDS entity are combined. </td>
        </tr>
<tr>
            <td>Addition REDEFINITION of the Statement GRANT SELECT ON</td>
            <td>The addition REDEFINITION is used to indicate that the specified CDS access rule is the only existing access rule and any other access rules are ignored. </td>
        </tr>
<tr>
            <td>Addition IN SCENARIO of the Statement GRANT SELECT ON</td>
            <td>The optional addition IN SCENARIO can be used to apply the switchable authorization check to an authorization object. </td>
        </tr>
<tr>
            <td>Generic Aspect Condition of the Statement DEFINE ROLE</td>
            <td>An aspect condition can now be used to specify the generic aspect defined in an aspect definition in the right side introduced by ASPECT. </td>
        </tr>
<tr>
            <td>New Variant INHERITING CONDITIONS FROM SUPER</td>
            <td>There is now another variant of the inheritance condition in an access rule of the statement DEFINE ROLE, which applies the access conditions from SUPER. </td>
        </tr>
<tr>
            <td>REPLACING Operator ELEMENT ... WITH</td>
            <td>Any number of ELEMENT operators can be used to transform conditions that use fields of the inherited entity to a different field of the inheriting entity. </td>
        </tr>
<tr>
            <td>Definition of a Generic Aspect</td>
            <td>A generic aspect can now be defined as part of a CDS access policy itself defined using DEFINE ACCESSPOLICY. </td>
        </tr>
<tr>
            <td>Access Control for ABAP CDS Hierarchies</td>
            <td>ABAP CDS hierarchies can now be protected using access control with some restrictions. </td>
        </tr>
<tr>
            <td>DCL Restrictions for CDS Transactional Queries</td>
            <td>Transactional queries apply the access control of the underlying CDS entity with restrictions. </td>
        </tr>
<tr>
            <td>VOID</td>
            <td>The new addition VOID can be specified in an access condition. It defines that the access condition in question is ignored. </td>
        </tr>
<tr>
            <td>CONSTRAINT ID</td>
            <td>The new addition CONSTRAINT ID can be specified in a PFCG mapping as part of a CDS access policy. It defines further restrictions for authorization fields of authorization objects in a CDS access policy. </td>
        </tr>
<tr>
            <td>Managed RAP BO</td>
            <td>The new statement managed can be used to create managed RAP BOs within the framework of the ABAP RESTful Application Programming Model. This scenario is intended for greenfield developments that are developed from scratch. Standard operations and services are provided by the RAP framework. </td>
        </tr>
<tr>
            <td>Business Object Projection</td>
            <td>The new statement projection can be used to create projections of a business object. This projects and aliases a subset of a business object for a specific business service within the framework of the ABAP RESTful Application Programming Model. </td>
        </tr>
<tr>
            <td rowspan="8">ABAP_SQL</td>
            <td>Extensions of the INTO Clause</td>
            <td>The INTO clause has been extended as follows: The new addition NEW can be used to implicitly create anonymous data objects as target areas. The addition NEW now also makes inline declarations using @DATA(...) possible when using dynamic tokens. The new addition INDICATORS can be used to specify a null indicator. </td>
        </tr>
<tr>
            <td>Extension of the CAST Matrix</td>
            <td>The matrix of types that can be converted to each other using a CAST expression was revised. In particular, the new data types in ABAP Dictionary are respected. The new types DECFLOAT16 and DECFLOAT34 can also be specified after the addition AS of the aggregate function AVG. </td>
        </tr>
<tr>
            <td>Extension of Arithmetic Expressions</td>
            <td>In arithmetic expressions, decimal floating point expressions with operators of types DECFLOAT16 or DECFLOAT34 are possible now. </td>
        </tr>
<tr>
            <td>New Built-In Functions</td>
            <td>ABAP SQL Now supports the following new built-in functions: UUID </td>
        </tr>
<tr>
            <td>New Window Functions</td>
            <td>ABAP SQL now supports the following new window functions in window expressions: LEAD and LAG. </td>
        </tr>
<tr>
            <td>SQL Conditions</td>
            <td>The SQL conditions were revised as follows: Unlike in all other relational expressions in ABAP SQL, the relational expression IS [NOT] NULL can now be used to check LOBs and geodata types. A new variant of the operator IN can be used to compare multiple operands with a list of value tuples. </td>
        </tr>
<tr>
            <td>Hierarchy Navigators</td>
            <td>The hierarchy aggregate navigators were revised as follows: New WITH additions in the hierarchy navigator HIERARCHY_DESCENDANTS_AGGREGATE make it possible to evaluate the aggregate functions specified using MEASURES for further row sets in the hierarchy. The new hierarchy navigator HIERARCHY_ANCESTORS_AGGREGATE makes it possible to calculate aggregate functions for ancestor nodes and also supports the new aggregate function PRODUCT. </td>
        </tr>
<tr>
            <td>Syntax Check for Literals and Host Constants</td>
            <td>The fact that conversions of host variables in read positions need to be lossless is checked for untyped literals and host constants in the strict syntax check modes from ABAP release 7.62 and ABAP release 7.63 and hence can produce syntax errors. </td>
        </tr>
<tr>
            <td rowspan="2">DDIC</td>
            <td>New Built-In Data Types</td>
            <td>The following new built-in data types were introduced in ABAP Dictionary: Decimal floating point numbers DECFLOAT16 DECFLOAT34 Date fields, time fields, and time stamp fields DATN TIMN UTCLONG Geodata types GEOM_EWKB These data types are currently only supported by SAP HANA databases. These types can be mapped to HANA-specific data types but not to other vendor-specific data types. ABAP-managed database objects where these types are used can only be created on SAP HANA databases. </td>
        </tr>
<tr>
            <td>Dynamic Cached Views</td>
            <td>Dynamic cached views are a kind of HANA tuning object defined using the statement DEFINE DYNAMIC CACHE. A dynamic cache is a DDIC integration of an SAP HANA Dynamic Result Cache. </td>
        </tr>
<tr>
            <td rowspan="1">MESSAGES</td>
            <td>Implicit Message Type in IF_T100_DYN_MSG</td>
            <td>If the object reference variable oref in the variant MESSAGE oref of the statement MESSAGE points to an object that implements the system interface IF_T100_DYN_MSG, the addition TYPE can be omitted and the message type from the interface attribute MSGTY of the object is used implicitly. Until now, however, the statement MESSAGE oref could only have the further additions RAISING and DISPLAY LIKE if TYPE was specified explicitly. These additions are now also possible if TYPE is not specified. </td>
        </tr>
<tr>
            <td rowspan="1">STRINGS</td>
            <td>Decimal Places in Time Stamps</td>
            <td>In embedded expressions of string templates, the formatting option DECIMALS can now be combined with TIMESTAMP and TIME ZONE to define the number of decimal places in UTC time stamps in packed numbers. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>New Built-In ABAP Type utclong</td>
            <td>The new built-in time stamp type utclong makes it possible to declare time stamp fields for UTC time stamps according to the POSIX standard. The new data type is taken into account in all relevant positions. Important points are: The associated data type in ABAP Dictionary was introduced with the name UTCLONG. The value range of the new data type are time stamps between 0001-01-01T00:00:00.0000000 and 9999-12-31T23:59:59.9999999 plus a special initial value. In assignments and comparisons, the data type utclong can only be converted to the types c and string, and vice versa. In assignments, any attempt to perform another conversion raises an exception of the new class CX_SY_CONVERSION_NOT_SUPPORTED. Time stamp fields are formatted specially in string templates. There is a mapping rule for asXML. Time stamp fields must meet an alignment requirement. Their memory address must be divisible by eight. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 776 (1905)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_CDS</td>
            <td>Annotation for Database Hints</td>
            <td>The framework-specific annotation @Consumption.dbHints replaces the ABAP annotation @AbapCatalog.dbHints and makes it obsolete. The ABAP annotation is evaluated by frameworks such as SADL and not by the ABAP runtime framework. </td>
        </tr>
<tr>
            <td>Annotations for Releasing Elements</td>
            <td>The new element annotations @API.element.releaseState @API.element.successor can be used to override releases of the individual elements and successors can be specified for forbidden elements. </td>
        </tr>
<tr>
            <td>CDS Projection Views</td>
            <td>A new type of CDS entity is available: the CDS projection view. A CDS projection view is a direct projection of the underlying CDS view and exposes only a subset of elements of the projected entity. A CDS projection view is defined using DEFINE VIEW ENTITY AS PROJECTION ON in a CDS data definition. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 775 (1902)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">ABAP_CDS</td>
            <td>Temporal Hierarchies</td>
            <td>The new addition PERIOD of the statement DEFINE HIERARCHY can now be used to create temporal SQL hierarchies in which the hierarchy nodes are limited by time intervals. </td>
        </tr>
<tr>
            <td>CDS Custom Entities</td>
            <td>A new type of CDS entity is available: the CDS custom entity. CDS custom entities are used in the RAP framework to implement ABAP queries in CDS. </td>
        </tr>
<tr>
            <td>Root Nodes and Compositions</td>
            <td>The new addition ROOT is available for CDS entities to mark an entity as a CDS root entity. In addition, CDS associations can be declared as COMPOSITION or TO PARENT. In this way, a CDS composition tree can be modeled for use in the ABAP RESTful Application Programming Model. </td>
        </tr>
<tr>
            <td>CDS Service Definitions</td>
            <td>The new statement DEFINE SERVICE is available for defining CDS service definitions. </td>
        </tr>
<tr>
            <td rowspan="4">ABAP_SQL</td>
            <td>Window Expressions</td>
            <td>Window expressions defined using OVER can now be used in the SELECT list of a query. Window expressions define windows as a subset of the result set and apply window functions to them. </td>
        </tr>
<tr>
            <td>New Aggregate Function STRING_AGG</td>
            <td>The new aggregate function STRING_AGG can be used to chain character-like results of the rows of the result set of a query or of the current group as a string. </td>
        </tr>
<tr>
            <td>Addition DISTINCT Optional in Aggregate Function COUNT</td>
            <td>The aggregate function COUNT( sql_exp ) can now be used without the addition DISTINCT. In this case, it counts all rows in which the value of the argument is not the null value. </td>
        </tr>
<tr>
            <td>Hierarchy Navigators</td>
            <td>The new hierarchy navigator HIERARCHY_DESCENDANTS_AGGREGATE can be used to calculate aggregate functions for descendant nodes. </td>
        </tr>
<tr>
            <td rowspan="1">AMDP</td>
            <td>Specifying CDS Entities After USING</td>
            <td>In the implementation of the AMDP method, the name of the CDS entity can now be specified after the USING addition for CDS views, CDS table functions, and CDS hierarchies. The name of the CDS-managed DDIC view (obsolete) of a CDS view or of the AMDP function of a CDS table function can still be specified, but is best replaced by specifying CDS entities. If a CDS entity is specified, all other database objects of CDS entities must be also be specified using this entity. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 774 (1811)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>Hierarchy Load Options</td>
            <td>The hierarchy generator DEFINE HIERARCHY can now use the new addition LOAD BULK|INCREMENTAL|load_option to specify the load policy for a generated hierarchy. </td>
        </tr>
<tr>
            <td>Handling of Annotation Values</td>
            <td>A change in the handling of annotation values has been introduced with the following consequences: Annotations that require an enumeration symbol as annotation value no longer accept string values. They only accept enumeration symbols. Example: Until release 7.74, the following was accepted: @AccessControl.authorizationCheck: '#CHECK'. From release 7.74, this is no longer accepted. The quotation marks must be removed. If # is the first or the only character of EndUserText.label or EndUserText.quickInfo, then it is not removed from the unescaped value any more. These changes are slightly incompatible. </td>
        </tr>
<tr>
            <td rowspan="3">ABAP_SQL</td>
            <td>Definition of Associations</td>
            <td>When associations of a common table expression are exposed using the addition WITH ASSOCIATIONS, new CTE associations can be defined by specifying JOIN TO ONE|MANY. These CTE associations can be used in the subsequent queries of the current WITH statement, either in path expressions or as hierarchy associations in the hierarchy generator HIERARCHY. </td>
        </tr>
<tr>
            <td>Temporal SQL Hierarchies</td>
            <td>The hierarchy generator HIERARCHY can now use the new addition PERIOD FROM TO VALID FROM TO to create temporal SQL hierarchies in which the hierarchy nodes are limited by time intervals. </td>
        </tr>
<tr>
            <td>Aggregate Expressions in SQL Expressions</td>
            <td>From ABAP release 7.74, aggregate expressions can be specified as operands of SQL expressions. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>Calculation Assignments</td>
            <td>A calculation assignment is an assignment with an addition assignment operator (+=) subtraction assignment operator (-=) multiplication assignment operator (*=) division assignment operator (/=) concatenation assignment operator (&amp;&amp;=). A calculation takes place when the assignment is made. These new operators make the statements ADD, SUBTRACT, MULTIPLY, and DIVIDE obsolete. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 773 (1808)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_CDS</td>
            <td>CDS Hierarchies</td>
            <td>The new statement DEFINE HIERARCHY can be used to create CDS hierarchies that are accessed as ABAP SQL hierarchies in ABAP SQL read statements. </td>
        </tr>
<tr>
            <td>New Condition IS INITIAL</td>
            <td>The new condition IS INITIAL can be used to check the initial value of operands. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The following enhancements have been implemented in CDS access control: New Boolean predicates TRUE and FALSE can now be used as part of an access condition. The following is now possible in an inheritance condition defined using INHERITING: The new additions DEFAULT TRUE and DEFAULT FALSE can be specified. ROOT WITH can be used to insert CDS elements in front of SQL path expressions. The syntax of DEFINE PFCG_MAPPING was modified to make it possible to define the PFCG mapping using a freely selectable CDS entity mapping_entity. </td>
        </tr>
<tr>
            <td rowspan="4">ABAP_SQL</td>
            <td>Access to Hierarchy Data</td>
            <td>In queries, both hierarchies and hierarchy node navigators can be specified as data sources. </td>
        </tr>
<tr>
            <td>Subquery as Data Source of MODIFY</td>
            <td>In the ABAP SQL write statement MODIFY, a parenthesized subquery SELECT subquery_clauses can now be specified as a data source after FROM. The rows of the result set of the subquery are modified or inserted in the target table directly on the database. No data transport is required between the database and AS ABAP. </td>
        </tr>
<tr>
            <td>GROUP BY Addition GROUPING SETS</td>
            <td>In a SELECT statement, the GROUP BY addition GROUPING SETS can now be used. The addition GROUPING SETS makes it possible to group multiple grouping sets under one GROUP BY clause. This is the same as specifying UNION ALL with different GROUP BY clauses. The addition GROUPING SETS has an advantage over a UNION clause grouping because the SELECT clause only needs to be specified once. </td>
        </tr>
<tr>
            <td>Aggregate Function GROUPING</td>
            <td>The aggregate function GROUPING can now be used in a SELECT statement. The grouping function GROUPING can be used to verify whether a specific column is part of the aggregation. The grouping function can be used only if the GROUP BY clause contains the addition GROUPING SETS. </td>
        </tr>
<tr>
            <td rowspan="2">AMDP</td>
            <td>AMDP Scalar Functions</td>
            <td>In the implementation of AMDP scalar functions, it is now possible to specify the database-specific option DETERMINISTIC after OPTIONS. This buffers the result of the function for the duration of a query. </td>
        </tr>
<tr>
            <td>Option CDS SESSION CLIENT Mandatory</td>
            <td>The option CDS SESSION CLIENT is now mandatory when an AMDP method accesses the CDS-managed DDIC view (obsolete) of a CDS view whose client handling is determined by the annotation @ClientHandling.algorithm: #SESSION_VARIABLE. If this option is not specified in this case, a syntax error occurs. The option sets the session variable of the database that can be addressed under the name $session.client in the CDS DDL of the ABAP CDS to a particular value when the method is called from ABAP. An exception of the class CX_AMDP_CDS_CLIENT_MISMATCH can now no longer be raised. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>Convertibility of Structures</td>
            <td>The conversion rules for flat structures are based on their fragment views, where each alignment gap is considered as a fragment. Alignment gaps arise from the alignment requirements of the component's data types. For character-like components, the alignment requirement depends on the character representation: For character representation UCS-2 used by the ABAP programming language, the memory address of character-like data objects must be divisible by 2. For other character representations there are other alignment requirements. For example, the non-Unicode character representation ASCII has no alignment requirement while the Unicode character representation UTF-32 requires a divisibility by 4. Meanwhile, only Unicode systems are supported. The system code page is UTF-16 and its subset UCS-2 is supported in the ABAP language. Any character is represented by 2 bytes and the alignment requirement is always a divisibility by 2. Because of this, the conversion rules for flat structures can be less strict than before and the new determination of possible alignment gaps was introduced in ABAP release 7.73. Example For example, the following assignment between two flat structures was not possible before ABAP release 7.73 but is possible now. TYPES:   BEGIN OF incl1,     num TYPE i,     c2  TYPE c LENGTH 2,   END OF incl1. DATA:   BEGIN OF struc1,     c1 TYPE c LENGTH 2.     INCLUDE TYPE incl1. DATA:     c3 TYPE c LENGTH 2,   END OF struc1. DATA:   BEGIN OF struc2,     c1  TYPE c LENGTH 2,     num TYPE i,     c2  TYPE c LENGTH 2,     c3  TYPE c LENGTH 2,   END OF struc2. struc1 = struc2. In the included substructure incl1, the character component has the same alignment as the integer component and there is no alignment gap. As long as non-Unicode Systems were supported, however, an alignment gap had to be assumed behind the included structure in order to make the program executable in Unicode systems as well as in non-Unicode Systems. </td>
        </tr>
<tr>
            <td rowspan="1">DATABASE</td>
            <td>Only SAP HANA Database as Standard Database</td>
            <td>As of this release, only an SAP HANA database is supported as the standard database of an AS ABAP. For this reason, Open SQL was renamed to ABAP SQL (see ABAP SQL in ABAP Release 7.73). </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Stricter Syntax Check in COLLECT</td>
            <td>Before the statement COLLECT can be executed for an internal table, all components that are not part of the primary table key must have a numeric data type. Until now, if certain structured components broke this rule, a syntax check warning and a program runtime error occurred. Now a syntax error occurs here too. </td>
        </tr>
<tr>
            <td rowspan="2">TYPES</td>
            <td>Checks on STRUCTURE Typing</td>
            <td>In obsolete STRUCTURE typing of formal parameters and field symbols, one assigned data object must be at least as long as the structure in question. Typing checks now respect the alignment gaps at the end of the data object and the structure. </td>
        </tr>
<tr>
            <td>Checks on Literals as Actual Parameters</td>
            <td>The value of a literal passed to a procedure must not be modified in the procedure. In certain cases this was, however, possible, namely when literals as actual parameters were passed to differently typed CHANGING parameters of subroutines. This is now prevented by stricter checks and always produces a runtime error. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 772 (1805)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>Literals</td>
            <td>A numeric literal whose value is outside the value range of INT4 but within the value range of INT8 is now handled like a field of type INT8 everywhere, including the definition of CDS associations. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The following enhancements have been implemented in CDS access control: New inheritance conditions can be used as access conditions in CDS roles. Some inheritance conditions apply conditions from existing CDS roles and some inheritance conditions apply access rules defined for other CDS entities. In a PFCG condition, a PFCG mapping can be mapped to an element list. This mapping assigns the CDS elements to the authorization fields of an authorization object. A PFCG mapping is defined in a CDS access policy using DEFINE PFCG_MAPPING. </td>
        </tr>
<tr>
            <td rowspan="5">ABAP_SQL</td>
            <td>Date/Time Functions</td>
            <td>ABAP SQL now supports the following new time zone functions: ABAP_SYSTEM_TIMEZONE ABAP_USER_TIMEZONE </td>
        </tr>
<tr>
            <td>Relational Expression IS INITIAL</td>
            <td>The relational expression IS [NOT] INITIAL can now be used in a condition sql_cond to compare operands with their type-dependent initial value. </td>
        </tr>
<tr>
            <td>Exposing CDS Associations of Common Table Expressions</td>
            <td>When CDS views are accessed within a common table expression, the addition WITH ASSOCIATIONS of the statement WITH can now be used to expose CDS associations of these views for use in path expressions of the current WITH statement. The addition REDIRECTED TO can also be used to replace the association target of the exposed CDS association with a previous CTE or the current CTE. </td>
        </tr>
<tr>
            <td>Numeric Literals in the SELECT List</td>
            <td>Until now, only those numeric literals whose value matched the value range of the type INT4 could be specified as elementary SQL expressions in the SELECT list of a query. Now numeric literals of up to 31 digits can be specified, which are interpreted as numbers of the type DEC if the value range of INT4 is exceeded. </td>
        </tr>
<tr>
            <td>Enhanced Cast Matrix</td>
            <td>A cast expression can now be used to convert the data types INT1, INT2, INT4, and INT8 to DEC. </td>
        </tr>
<tr>
            <td rowspan="2">AMDP</td>
            <td>AMDP Scalar Functions</td>
            <td>AMDP scalar functions are now supported alongside AMDP table functions. The AMDP function implementation of an AMDP scalar function has an elementary return value and can be used in ABAP like a regular function method. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>When a replacement parameter declared using DEFAULT is specified for an elementary input parameter of an AMDP method, the following (previously undocumented) restrictions were lifted: Constants declared using the addition VALUE IS INITIAL can now be specified for the data types d, t, and x. This previously produced a syntax error. Constants declared using the addition VALUE '00010101' can now be specified for the data type d, or the literal '00010101' can be specified directly. Both previously produced a syntax error. Constants declared by specifying numeric literals after VALUE and whose lengths are not precisely 8 or 6 can now be specified for the data types d and t. This previously produced a syntax error. Until now, values of literals with lengths of precisely 8 or 6 were handled like a string. This incorrect behavior was also modified and the numeric value is handled as the number of days since 01.01.001 or as the number of seconds since 00:00:00. This modification is incompatible if an AMDP procedure or function with an input parameter of this type is called from other database procedures or functions without an actual parameter being assigned to the parameter in question. In AMDP methods, the addition DEFAULT now behaves in the same way as in regular methods. It still cannot be specified for the data types string, xstring, decfloat16, and decfloat34, however, and no literals can be specified that cannot be converted into the data type of the input parameter. </td>
        </tr>
<tr>
            <td rowspan="3">EXCEPTIONS</td>
            <td>Raising Runtime Errors</td>
            <td>The new statement RAISE SHORTDUMP raises runtime errors linked with an exception object. This means more information can now be passed to the short dump than was previously possible in an exit message. </td>
        </tr>
<tr>
            <td>Last Message in a Chain of Exception Objects</td>
            <td>The new method GET_LATEST_T100_EXCEPTION in the class CL_MESSAGE_HELPER is used to return the last object in a chain of exception objects that has an exception text defined by a message. Here, the chain is created using the attribute PREVIOUS. </td>
        </tr>
<tr>
            <td>Setting the Attribute IS_RESUMABLE</td>
            <td>After an exception is raised using the statement RAISE RESUMABLE EXCEPTION and caught using the statement CATCH BEFORE UNWIND, the attribute IS_RESUMABLE is set for all previous exception objects referenced in the attribute PREVIOUS and not just for the current exception object. Up until the first resumable exception in the chain IS_RESUMABLE is set to the value of abap_true and is set to the value of abap_false otherwise. </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Predicates in WHERE Conditions</td>
            <td>In WHERE conditions of the statements LOOP AT itab, DELETE itab, and MODIFY itab, and in table iterations with FOR, IS [NOT] INSTANCE OF can now be specified alongside the predicate expressions IS [NOT] INITIAL and IS [NOT] BOUND. </td>
        </tr>
<tr>
            <td rowspan="1">IXML_LIBRARY</td>
            <td>iXML Library for ABAP Cloud</td>
            <td>The iXML Library now provides a wrapper API to handle XML documents in ABAP for Cloud Development. To access the iXML Library, use the syntax cl_ixml_core()=&gt;create. </td>
        </tr>
<tr>
            <td rowspan="1">TRANSFORMATIONS</td>
            <td>Resumable Exceptions in Deserializations with ST</td>
            <td>The new transformation option OPTIONS exceptions = 'resumable' can be used to make exceptions of the class CX_ST_DESERIALIZATION_ERROR into resumable exceptions. In handling with CATCH BEFORE UNWIND, the new attribute RESULT_REF_FOR_RESUME of the exception object of the class CX_ST_DESERIALIZATION_ERROR points to the target field where the exception was raised. The canceled transformation can be resumed using RESUME. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 771 (1802)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="5">ABAP_SQL</td>
            <td>Null Values in the Table Buffer</td>
            <td>The table buffer now supports real null values. In table buffering, null values are no longer transformed to type-dependent initial values. When the buffer is accessed, the same results are produced as when the database is accessed directly. The corresponding restrictions now no longer apply. The following are some of the aspects affected: Relational expressions with operands that contain null values. The result of a comparison of this type is now also unknown when the comparison is made in the buffer (except in the expression IS [NOT] NULL). If used, IS [NOT] NULL no longer bypasses table buffering. Accesses to buffered CDS views. When the buffer is accessed, the same results are now produced as when the database is accessed directly. Null values are often produced by outer joins or in expressions such as case distinction expressions. The restriction specifying that only those CDS views are buffered whose elements do not contain null values no longer applies. </td>
        </tr>
<tr>
            <td>Date/Time Functions</td>
            <td>ABAP SQL now supports the following new date/time functions: Time functions TIMS_IS_VALID Time stamp functions TSTMP_IS_VALID TSTMP_CURRENT_UTCTIMESTAMP TSTMP_SECONDS_BETWEEN TSTMP_ADD_SECONDS Date/time conversions TSTMP_TO_DATS TSTMP_TO_TIMS TSTMP_TO_DST DATS_TIMS_TO_TSTMP </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>For certain SQL expressions and functions, an ABAP SQL read statement no longer bypasses table buffering. </td>
        </tr>
<tr>
            <td>Weaker Check</td>
            <td>ABAP SQL statements that exploit a database property not supported by all database platforms no longer produce a syntax check warning. </td>
        </tr>
<tr>
            <td>New Check</td>
            <td>If the data of the internal table needs to be transported to the database in cases where the internal table is used as a data source of the ABAP SQL statement SELECT, a syntax check warning occurs that can be hidden using the pragma ##itab_db_select. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 770 (1711)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>CDS Abstract Entities</td>
            <td>An CDS abstract entity defines the type properties of a CDS entity without creating an instance of a database object. A CDS abstract entity is defined using DEFINE ABSTRACT ENTITY in a CDS data definition. </td>
        </tr>
<tr>
            <td>Metadata Extensions for CDS Entities</td>
            <td>The variant ANNOTATE ENTITY was added to the previous statement ANNOTATE VIEW. This makes it possible to define CDS metadata extensions for any CDS entities with the exception of CDS table functions. Metadata extensions of this type can contain entity annotations, parameter annotations, and element annotations. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>Restrictions Removed</td>
            <td>ABAP SQL read statements no longer bypass table buffering in cases where a column is specified on the right side of comparisons or of BETWEEN in an SQL condition that is not required to identify a single row or a generic range. The prerequisite for this is that both operands are numeric but do not have the type DF16_DEC or DF34_DEC, that both operands are character-like, or that both operands have the type RAW with the same lengths. </td>
        </tr>
<tr>
            <td rowspan="1">AMDP</td>
            <td>Restrictions Removed</td>
            <td>The following restrictions were removed: Tabular input parameters are now allowed in AMDP function implementations for AMDP table functions. Previously, only elementary input parameters were allowed. In AMDP function implementations for CDS table functions, however, the restriction that only elementary input parameters are allowed still applies. Tabular input parameters of AMDP procedure implementations and of AMDP function implementations can now be made optional using OPTIONAL, but it is still not possible to specify a start value with DEFAULT. </td>
        </tr>
<tr>
            <td rowspan="1">DDIC</td>
            <td>Expanded Limits</td>
            <td>The following limits now apply to the number of fields and the length of the structure of a DDIC database table, of a DDIC database view, and of a CDS view: Database tables A database table that is not part of the software component SAP_BASIS can now contain 1000 fields for the storage type Row Store and 1500 fields for the storage type Column Store. The total of all field lengths is no longer checked in ABAP Dictionary. A database table that is part of the software component SAP_BASIS can contain a maximum of 749 fields (as before) and the total of the field lengths in ABAP Dictionary is still restricted to 4030. DDIC database views and CDS views A view that is not part of the software component SAP_BASIS can contain 1500 view fields. The total of all field lengths is no longer checked in ABAP Dictionary. A view that is part of the software component SAP_BASIS can contain a maximum of 749 fields (as before) and the total of the field lengths in ABAP Dictionary is still restricted to 4096. </td>
        </tr>
<tr>
            <td rowspan="1">EXCEPTIONS</td>
            <td>Setting the Attribute IS_RESUMABLE</td>
            <td>The attribute IS_RESUMABLE of an exception object is now set after exceptions raised by the statement RAISE RESUMABLE EXCEPTION only if the CATCH statement in question has the addition BEFORE UNWIND. Previously in catches, the attribute was set for every exception raised in this way. </td>
        </tr>
<tr>
            <td rowspan="1">MISC</td>
            <td>ABAP for Cloud Development</td>
            <td>In ABAP release 7.70, a new ABAP version ABAP for Cloud Development was introduced. The internal version ID in the column UCCHECK of the system table TRDIR is 5. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 769 (1708)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>Access Control</td>
            <td>The following enhancements have been implemented in CDS access control: CDS roles can now also be defined for CDS table functions. When a CDS table function is accessed in ABAP SQL, the access conditions are evaluated by default. Extensions in DCL for defining CDS roles: When access conditions cds_cond are specified, there are no longer any restrictions on how Boolean operators and parentheses are used. The Boolean operator NOT can now also be used and any combination of parentheses is possible. The operators BETWEEN and IS NULL are now available for literal conditions as part of an access condition. When a path is specified for an element in an access condition, multivalue CDS associations are now also tracked. In a literal condition [NOT] LIKE, it is now possible to specify an escape character using ESCAPE. Blanks are now no longer forced in certain positions. </td>
        </tr>
<tr>
            <td>Extensions</td>
            <td>CDS DDIC-based view extensions are now connected to Switch Framework whenever they are defined in a package that is assigned a switch. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_DOC</td>
            <td>Documentation Links</td>
            <td>The new syntax {@link ...} makes it possible to link to other documentation of repository objects (or its components) in ABAP Doc comments. </td>
        </tr>
<tr>
            <td>Further Changes</td>
            <td>The following changes have also been made to ABAP Doc comments: Opening and closing tags no longer need to be specified in lowercase letters. The restriction to 7 bit ASCII characters no longer applies. An ABAP Doc comment can now contain any 16 bit Unicode characters. &lt;br&gt; or &lt;br/&gt; can be used to specify a line break. &lt;br&gt;&lt;/br&gt; should no longer be used. The special characters @ and &lt; now only need to be escaped when they are placed directly in front of another character. Lists can now be nested. The tags &lt;p&gt;, &lt;ul&gt;, or &lt;ol&gt; can now be placed within &lt;h1&gt;, &lt;h2&gt;, &lt;h3&gt;, &lt;p&gt;, &lt;em&gt;, or &lt;strong&gt; tags. Text can now be specified directly within &lt;ol&gt; and &lt;ul&gt; tags. In the &lt;ol&gt; tag, the attributes reversed, start, and type can now be specified with their traditional HTML meaning. The attributes specified in tags are now checked by the syntax check. </td>
        </tr>
<tr>
            <td rowspan="6">ABAP_SQL</td>
            <td>Internal Tables as Data Sources</td>
            <td>An internal table can be specified as a data source data source of a query. This statement cannot be executed on all database systems, if the data from the internal table needs to be passed to the database. </td>
        </tr>
<tr>
            <td>Relational Expressions</td>
            <td>The following is now possible for conditions in expressions: Size comparisons can now be made between character-like data types and are no longer restricted to numeric data types. The operator BETWEEN is also no longer restricted to numeric data types and SQL expressions can now be specified on the right side. The operator LIKE is now also supported. </td>
        </tr>
<tr>
            <td>Path Expressions</td>
            <td>The following is now possible for path expressions: CDS associations can now be used whose association targets are CDS table functions. Parameters can now be passed after the CDS associations of a path expression in the specified columns of queries. Until now, this was only possible in path expressions in the data source of the FROM clause of a query. This makes it possible to specify paths whose CDS associations have CDS entities with input parameters as data sources. The cardinality and type of the join expression can now be specified as attributes in the square brackets after the CDS associations of a path expression. Until now, only filter conditions were possible. Attributes can now be specified after the CDS associations of a path expression in the specified columns of queries. Until now, attributes could only be specified in path expressions in the data source of the FROM clause of a query, and only filter conditions were possible. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The new addition WITH PRIVILEGED ACCESS disables CDS access control. </td>
        </tr>
<tr>
            <td>ORDER BY and UP TO, OFFSET in Subquery</td>
            <td>In a subquery, it is now possible to use an ORDER BY clause and the additions UP TO and OFFSET can be used after the clause. It is not possible to execute a subquery with an ORDER BY clause on all database systems </td>
        </tr>
<tr>
            <td>Addition NOT for BETWEEN and LIKE</td>
            <td>The addition NOT can now be specified in front of BETWEEN and LIKE in relation expressions for expressions. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_UNIT</td>
            <td>Test Relations</td>
            <td>The special ABAP Doc comment "! @testing ... can be used to define test relations between test classes or test methods and repository objects. This allows the tests for these repository objects to be displayed and executed in ADT. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Pseudo Component table_line in Mapping Rules</td>
            <td>It is now possible to specify the pseudo component table_line as a source component of a source table in mapping rules of the component operator CORRESPONDING, if the table has an elementary row type. In all other cases, the behavior is undefined when the pseudo component is specified. </td>
        </tr>
<tr>
            <td rowspan="2">MISC</td>
            <td>Surrogate Areas in iXML Library</td>
            <td>The iXML Library now supports surrogate areas with the same problems as in ABAP. </td>
        </tr>
<tr>
            <td>New Catchable Exceptions</td>
            <td>The following exceptions in IMPORT were previously non-catchable but are now assigned to the exception class CX_SY_IMPORT_FORMAT_ERROR and hence can be handled: CONNE_ILLEGAL_TRANSPORT_HEADER CONNE_ILLEGAL_TRANSPORT_VERS CONNE_COMPRESS_FLAG_INVALID CONNE_CONTAINER_TOO_SHORT CONNE_DESCRIPTION_FLAG_INVALID CONVERSION_CODEPAGE_UNKNOWN IMPORT_DESCR_ENDMARK_MISSING IMPORT_UNEXPECTED_END_OF_DATA IMPORT_CONTAINER_MISSING IMPORT_FROM_DATA_BUFFER_EMPTY is now also IMPORT_CONTAINER_MISSING IMPORT_FROM_INTTABLE_EMPTY is now also IMPORT_CONTAINER_MISSING IMPORT_CONTAINER_MISSING IMPORT_DECOMPRESS_FAILED IMPORT_OBJECT_DESTROYED </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 768 (1705)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>Conversion Functions</td>
            <td>The new type conversion functions BINTOHEX and HEXTOBIN make it possible to convert byte strings to character strings (and the other way round) in SQL expressions, which is not possible with a CAST expression. </td>
        </tr>
<tr>
            <td rowspan="1">AMDP</td>
            <td>AMDP Options</td>
            <td>The new addition AMDP OPTIONS for METHODS and CLASS-METHODS statements can be used to define attributes of AMDP methods in their declaration: The READ-ONLY option only allows reads in the implementation of the AMDP methods. The CDS SESSION CLIENT option sets the session variable CDS_CLIENT of the database that can be addressed under the name $session.client in the CDS DDL of the ABAP CDS to a particular value when the method is called from ABAP. It avoids the warning from the syntax check and the exception CX_AMDP_CDS_CLIENT_MISMATCH when an AMDP method accesses the CDS-managed DDIC view (obsolete) of a CDS view whose client handling is determined by the annotation @ClientHandling.algorithm: #SESSION_VARIABLE. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>System Class for Dynamic Mapping</td>
            <td>The system class CL_ABAP_CORRESPONDING now has a new method CREATE_WITH_VALUE, which allows the values of any suitable data objects to be assigned to the components of the target structure or target table. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 767 (1702)</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="1">ABAP_OBJECTS</td>
            <td>Implementing Interfaces</td>
            <td>When implementing interfaces in classes with the statement INTERFACES, the following restrictions do not apply: Methods and attributes of component interfaces of the implemented interface can now be specified after the additions ABSTRACT METHODS, FINAL METHODS, and DATA VALUES using the name of the component interface and the interface component selector~. Until now, this was only possible for methods by using an alias name, for attributes the interface itself had to be included. Attributes that are declared in component interfaces can now be specified after the addition DATA VALUES by using alias names. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>Path Expressions</td>
            <td>The following enhancements have been made to path expressions that are specified as a data source of the FROM clause of SELECT statements of a query: When using a path expression, actual parameters can now be passed to CDS entities with input parameters. Path expressions can now contain CDS associations for entities with input parameters. Filter conditions for CDS associations can now be specified in path expressions. Path expressions can now be split over several source code rows at the blanks in the syntax for parameter passes and filter conditions and also before slashes (/). </td>
        </tr>
<tr>
            <td rowspan="2">EXPRESSIONS</td>
            <td>Exception Object After RAISE EXCEPTION</td>
            <td>The operand position for the reference variable oref of the statement RAISE EXCEPTION oref is now a general expression position. </td>
        </tr>
<tr>
            <td>Object Component Selector After Table Expressions</td>
            <td>If an object component selector -&gt; is specified directly after a table expression, the restriction that this is not possible for table expressions whose result is determined with the value operator VALUE no longer applies. </td>
        </tr>
<tr>
            <td rowspan="1">MESSAGES</td>
            <td>Implicit Message Specification in RAISE EXCEPTION MESSAGE</td>
            <td>A new short form USING MESSAGE of statement RAISE_EXCEPTION with the addition MESSAGE makes it possible to pass the content of system fields sy-msg... implicitly to the exceptions of exception classes that include the system interface IF_T100_DYN_MSG. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 766</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="1">ABAP_CDS</td>
            <td>Cardinality in LEFT OUTER JOIN</td>
            <td>In a LEFT OUTER JOIN, an addition TO ONE or TO MANY can be specified for the cardinality. This addition is evaluated by an SAP HANA database as a note for optimization. This option exists in earlier releases in ABAP CDS, but has been documented only from ABAP release 7.66. In ABAP SQL, the corresponding syntax was introduced in ABAP release 7.66. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>Cardinality in LEFT OUTER JOIN</td>
            <td>In a LEFT OUTER JOIN, an addition ONE TO MANY can now be specified for the cardinality. This is evaluated as a note for optimization by SAP HANA databases. </td>
        </tr>
<tr>
            <td rowspan="1">AMDP</td>
            <td>Reference to ABAP Types</td>
            <td>When an AMDP method is implemented in an AMDP class with SQLScript, the following new AMDP macro "$ABAP.type( [name =] abap_type )" can be used to reference ABAP types. The ABAP runtime framework replaces these schemas on the database with the associated database types. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Key Specification for the FILTER Operator</td>
            <td>In the variant of the constructor expression where a filter table is specified, the addition USING KEY can be used either for the filter table or for the source table. Previously, the filter table had to have a sorted key or a hash key. This restriction does not apply if such a key can be specified for the source table instead. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 765</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="5">ABAP_CDS</td>
            <td>Metadata Extensions</td>
            <td>Metadata extensions are new CDS objects that allow CDS annotations for a CDS entity to be created and transported separately from their DDL source code. </td>
        </tr>
<tr>
            <td>Annotations</td>
            <td>For each element annotation that is not part of an annotation array, the special value null can be specified (without quotation marks). </td>
        </tr>
<tr>
            <td>Cross Join</td>
            <td>As well as an inner and outer join, it is now possible to use a cross join in a SELECT statement. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: Aggregate expressions can now be used as operands in a CAST expression. In a CAST expression, the data types CHAR, SSTR, and NUMC can now be cast to ACCP, and the other way round. The following additional string functions are now supported: UPPER and LOWER The following additional date/time functions are now supported: ABAP_SYSTEM_TIMEZONE, ABAP_USER_TIMEZONE, TSTMP_TO_DATS, TSTMP_TO_TIMS, TSTMP_TO_DST, and DATS_TIMS_TO_TSTMP An addition AS dtype can now be specified for the aggregate expression AVG to determine the data type of the return value. A new built-in conversion function FLTP_TO_DEC can be used to convert arguments of type FLTP to packed numbers. Built-in functions can now be specified on the right side of a cds_cond condition of a WHERE condition, an ON condition, a filter condition, or a complex case distinction. In cds_cond conditions, fields of data sources of the type ACCP can now be compared with fields of the same type, and with literals of the type NUMC. A literal of a corresponding value is not handled as a field of type INT8. An exception to this are literals in the definition of CDS associations. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The following enhancements have been implemented in CDS access control: As well as conditional access rules, there are now also full access rules. There is a new operator ?= for access conditions, which checks not only for a specified value but also the initial value or the null value. A new user condition compares the value of an element of a CDS entity with the current user name. </td>
        </tr>
<tr>
            <td rowspan="4">ABAP_SQL</td>
            <td>Common Table Expressions</td>
            <td>The new ABAP SQL statement WITH enables common table expressions (CTEs) to be defined for use in the WITH statement. A common table expression creates a result set that is used in the queries of the WITH statement as a data source. The main query of the WITH statement has an INTO clause and transfers its result set to ABAP data objects. </td>
        </tr>
<tr>
            <td>Cross Join</td>
            <td>As well as an inner and outer join, it is now possible to use a cross join in a SELECT statement. </td>
        </tr>
<tr>
            <td>SQL Functions</td>
            <td>The following changes have been made: The new numeric function DIVISION enables divisions with decimal places. The new string functions LOWER and UPPER implement uppercase and lowercase. The new date functions DATS_IS_VALID, DATS_DAYS_BETWEEN, DATS_ADD_DAYS and DATS_ADD_MONTHS execute operations with date fields. An addition AS dtype can now be specified in the aggregate function AVG to define the data type of the result. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>The following restrictions were removed: The addition ORDER BY PRIMARY KEY of the SELECT statement can now also be specified if a column is specified multiple times in the SELECT list, without the same name being blocked by alternative names. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>Enumerated Types</td>
            <td>The statement TYPES BEGIN OF ENUM can be used to define enumerated types for enumerated variables, which can have only enumerated values defined for this type. The document Enumerated Objects summarizes the properties of enumerated types and enumerated objects. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 764</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_CDS</td>
            <td>Client Handling</td>
            <td>The new annotation @ClientHandling specifies the client handling of CDS views and CDS table functions. It replaces the annotation @ClientDependent and makes it obsolete. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: In a CAST expression, operands of the types CLNT, LANG, TIMS, and UNIT can now be cast to the types CHAR and SSTRING. Here, the target type must be specified as a data element. </td>
        </tr>
<tr>
            <td>Session Variables</td>
            <td>When a CDS view is accessed using ABAP SQL, it is possible to access the new session variable $session.system_date in which the values of the system field sy-datum are available. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_SQL</td>
            <td>SQL Functions</td>
            <td>In the string functions INSTR, LPAD, LTRIM, RPAD, and RTRIM, arguments passed as constants or literals can now contain special characters. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>The components of a replacement object must no longer be in the same order as the associated components of the replaced database table or DDIC view. </td>
        </tr>
<tr>
            <td rowspan="1">MESSAGES</td>
            <td>Implicit Message Type in IF_T100_DYN_MSG</td>
            <td>If the object reference variable oref in the variant MESSAGE oref of the statement MESSAGE (used to send a message) points to an object that implements the system interface IF_T100_DYN_MSG, the addition TYPE can be omitted. The message type from the interface attribute MSGTY of the object is then used implicitly. </td>
        </tr>
<tr>
            <td rowspan="5">TRANSFORMATIONS</td>
            <td>New Domains for Mapping from ABAP to XML</td>
            <td>The following new special domains have been introduced, which override the default mapping of elementary ABAP types to asXML. XSDUUID_BASE64 for 16-byte UUIDs in base64 format XSDCURRCODE for ISO currency keys XSDUNITCODE for ISO unit keys </td>
        </tr>
<tr>
            <td>New Formats for format in the Attribute option of tt:value</td>
            <td>The following new formats can be specified in parentheses after format in the option attribute of the ST statement tt:value: hex for a hexadecimal display of byte-like values uri and uri_full for escaping special characters in URIs uri1 and uri2 for displaying literal values in URIs for OData. currCode, unitCode for converting SAP-specific currency and unit keys in ISO units. currency=CURRCODE, unit=UNITCODE for formatting numbers in accordance with a currency or unit key alpha for handling leading zeros in strings Other enhancements: dateTimeLocal, dateTimeOffset, and ticksOffset can now be used on the UTC time stamp types TIMESTAMP and TIMESTAMPL. guid can now also be used on the type c of length 22 for 16-byte UUIDs in base64 format. </td>
        </tr>
<tr>
            <td>decimals New for the Attribute option of tt:value</td>
            <td>Decimals specified in the attribute option of the ST statement tt:value specifies the number of decimal places in numeric types. </td>
        </tr>
<tr>
            <td>regime New for the Attribute option of tt:value</td>
            <td>regime(num|char|bin) can now be specified in the attribute option of the ST statement tt:value, where it can be used to force numeric, character-like, or byte-like handling. </td>
        </tr>
<tr>
            <td>noError New for the Attribute option of tt:value</td>
            <td>noError specified in the attribute option of the ST statement tt:value prevents exceptions in language, currCode, and unitCode formattings, which evaluate entries in database tables. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 763</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">ABAP_CDS</td>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: In a CAST expression, operands of the type SSTRING can now be cast to types other than themselves and back. Here, the type SSTRING behaves like the data type CHAR. The following changes have been made: In CAST expressions to data elements, the restriction no longer applies that the data type, the length, and the number of decimal places of operand and target data type must match exactly. This restriction can now be applied as an optional restriction using the new addition PRESERVING TYPE. This addition specifies explicitly that casts are to be applied to the semantic properties of a data element. PRESERVING TYPE suppresses the syntax warning that handles casts of identical technical types. </td>
        </tr>
<tr>
            <td>CDS Associations</td>
            <td>The following changes have been made: WITH DEFAULT FILTER can be used to specify a default filter condition for a CDS association. This condition is used as a filter condition in a path expression if no condition is specified for the CDS association here. CDS associations can now be exposed for union sets formed with UNION. In this case, special rules apply. In a path expression, *: can be used to declare a CDS association as a non-unique CDS association explicitly. </td>
        </tr>
<tr>
            <td>CDS DDIC-Based View Extensions</td>
            <td>The statement EXTEND VIEW can now be used to extend the following CDS views too: CDS views with aggregate expressions and a GROUP-BY clause CDS views with a UNION clause for union sets For extensions of the GROUP-BY clause and UNION clauses, the existing CDS view must contain the new annotation array AbapCatalog.viewEnhancementCategory[ ] with suitable values. The value #NONE of this annotation array can be used to prevent any enhancements being made to a CDS view using CDS DDIC-based view extensions. </td>
        </tr>
<tr>
            <td>Key Fields</td>
            <td>The following changes have been made: The key fields of a CDS view that are defined with KEY must now, like the key fields of a CDS table function, be placed without gaps at the start of the SELECT list. The new view annotation AbapCatalog.preserveKey can be used to override the default behavior of the addition KEY for defining key fields of a CDS view. If the annotation is specified with the value true, the key fields defined using KEY are also used for the associated CDS-managed DDIC view (obsolete). </td>
        </tr>
<tr>
            <td rowspan="3">ABAP_SQL</td>
            <td>New Addition OFFSET in SELECT</td>
            <td>An addition OFFSET can now be specified to specify the first row of the result set. </td>
        </tr>
<tr>
            <td>SQL Functions</td>
            <td>The new string functions LEFT, CONCAT_WITH_SPACE, INSTR, and RPAD perform operations on strings. This means ABAP SQL now supports the same string functions as ABAP CDS. </td>
        </tr>
<tr>
            <td>New Additions in DELETE dbtab</td>
            <td>In the variant DELETE FROM target of the statement DELETE, the additions ORDER BY, OFFSET, and UP TO can now be specified to restrict the number of rows to delete. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 762</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_CDS</td>
            <td>Session Variables</td>
            <td>When a CDS view is accessed using ABAP SQL, three session variables ($session.user, $session.client, and $session.system_language) can be accessed here. In these variables, the values of the system fields sy-uname, sy-mandt and sy-langu are available. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following changes have been made: A CAST expression now contain nested cast expressions and case distinctions as an operand. The following additional string functions are now supported: CONCAT_WITH_SPACE, LEFT, INSTR, RIGHT, and RPAD. Input parameters from the parameter list parameter_list of a CDS view can be passed to many parameters of built-in functions. A one character literal with the type NUMC can now be compared with a data source of the type LANG. </td>
        </tr>
<tr>
            <td>Exposing CDS Associations</td>
            <td>Path expressions can now be exposed with more than one CDS association in the SELECT list of a CDS view. The fields of the association source from the ON condition of the exposed CDS association, which then also need to be specified, must be defined using an appropriately specified path. </td>
        </tr>
<tr>
            <td rowspan="8">ABAP_SQL</td>
            <td>Subquery as Data Source of INSERT</td>
            <td>In the ABAP SQL write statement INSERT, a parenthesized subquery SELECT subquery_clauses can now be specified as a data source after FROM. The rows of the result set of the subquery are inserted into the target table directly on the database. No data transport is required between the database and AS ABAP. </td>
        </tr>
<tr>
            <td>SQL Functions</td>
            <td>The following changes have been made: New Numeric Function The new function ROUND rounds numeric values. New String Functions The new functions CONCAT, LPAD, LENGTH, LTRIM, REPLACE, RIGHT, RTRIM, and SUBSTRING perform operations on strings. Coalesce Function Expanded The coalesce function can now have 255 arguments instead of just two and returns the value of the first argument that does not have the null value. </td>
        </tr>
<tr>
            <td>SQL Expressions</td>
            <td>Columns of the built-in dictionary type SSTRING can now be used in the SQL expressions CASE and the coalesce function. </td>
        </tr>
<tr>
            <td>Host Expressions</td>
            <td>From ABAP release 7.62, host expressions can be specified for the work areas wa or the internal tables itab (from which the data is taken) in the write statements INSERT, UPDATE, MODIFY, and DELETE. </td>
        </tr>
<tr>
            <td>CDS Path Expressions</td>
            <td>From ABAP release 7.62, path expressions can be used as data sources of the FROM clause of the statement SELECT. </td>
        </tr>
<tr>
            <td>Access to Global Temporary Tables</td>
            <td>When the new global temporary tables in ABAP Dictionary are accessed using ABAP SQL, all temporary data stored here is guaranteed to be deleted before the next implicit database commit, If not, a runtime error occurs. </td>
        </tr>
<tr>
            <td>ON Conditions</td>
            <td>From ABAP release 7.62, the full ON condition or subconditions of joins can be specified dynamically as (cond_syntax). This is not possible if the full FROM clause is specified dynamically as (cond_syntax). </td>
        </tr>
<tr>
            <td>System Classes</td>
            <td>The new system class CL_DBI_UTILITIES contains utility methods for the database interface. The documented method IS_LOGGING_ON can be used to verify whether logging is currently switched on for a database table. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>System Class for Dynamic Mapping</td>
            <td>The new methods CREATE_USING and EXECUTE_USING for making assignments between internal tables by component while using lookup tables have been added to the system class CL_ABAP_CORRESPONDING. If the method EXECUTE for simple assignment is used, the restriction that source and target cannot be identical has been lifted. It should be noted, however, that no temporary copy of the source is created as a target object (like in MOVE-CORRESPONDING), which means that the result in the case of overlapping source and target components is different than when the operator CORRESPONDING is used reflexively. </td>
        </tr>
<tr>
            <td rowspan="2">EXPRESSIONS</td>
            <td>Predicate Expression for Type Inspection</td>
            <td>The new predicate expression IS INSTANCE OF can be used to detect the dynamic type of an object reference variable. This makes it possible to check the feasibility of a downcast before it is executed. </td>
        </tr>
<tr>
            <td>Case Distinction for Type Inspection</td>
            <td>The special statement CASE TYPE OF makes it possible to check the dynamic type of an object reference variable as a case distinction. </td>
        </tr>
<tr>
            <td rowspan="1">MISC</td>
            <td>Short Texts in ABAP Doc</td>
            <td>It is now possible to define short texts in ABAP Doc comments. </td>
        </tr>
<tr>
            <td rowspan="3">TYPES</td>
            <td>New Built-In ABAP Type int8</td>
            <td>The new built-in data type int8 enables 8-byte integers with signs to be declared. The associated data type in ABAP Dictionary was introduced with the name INT8. The value range of the new data type int8 is -9,223,372,036,854,775,808 to +9,223,372,036,854,775,807. Apart from the extended value range, the new data type int8 has the same properties as the existing data type i for 4-byte integers, with the following exceptions: The alignment required for data objects of type int8 is an address divisible by 8. A new calculation type has been introduced for int8, situated between i and p in the hierarchy. </td>
        </tr>
<tr>
            <td>Global Temporary Tables</td>
            <td>Database tables in ABAP Dictionary can be defined using the table category global temporary table. A global temporary table (GTT) can only be filled with temporary data during a database LUW. When a GTT is filled using ABAP SQL, it must be emptied again explicitly before the next implicit database commit. If not, an uncatchable exception is raised. </td>
        </tr>
<tr>
            <td>Value Ranges of Domains</td>
            <td>When the value range of a domain is defined, the data types INT1, INT2, and INT4 are now checked (like INT8) to determine whether the fixed values and interval boundaries are valid values, that is, that they lie within the value range defined by the technical properties. Existing domains with invalid value ranges must be corrected. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 761</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="5">ABAP_CDS</td>
            <td>CDS Table Functions</td>
            <td>The new CDS DDL statement DEFINE TABLE FUNCTION can be used to define CDS table functions as a new category of CDS entities. In platform-dependent SQL, a CDS table function is implemented in an associated AMDP function implementation. </td>
        </tr>
<tr>
            <td>Annotation for Input Parameters</td>
            <td>An input parameter of a CDS view or a CDS table function can now be annotated with an annotation @Environment.systemField. The potential values of the annotation assign ABAP system fields to the input parameters. If a CDS entity of this type is used a data source in ABAP SQL, the assigned values can be passed implicitly. In particular, the value #CLIENT enables the client ID of the current client to be passed implicitly, which provides support for client handling in platform-dependent SQL of the implementation of a CDS table function. </td>
        </tr>
<tr>
            <td>API for Evaluation of Annotations</td>
            <td>The class CL_DD_DDL_ANNOTATION_SERVICE has been made available for evaluating annotations. It contains methods used to read the annotations of CDS entities from the associated system tables. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: The new date function DATS_IS_VALID checks the validity of dates. The new time function TIMS_IS_VALID checks the validity of times. The new time stamp functions TSTMP_IS_VALID, TSTMP_CURRENT_UTCTIMESTAMP, TSTMP_SECONDS_BETWEEN, and TSTMP_ADD_SECONDS perform operations with UTC time stamps in packed numbers. CAST expressions can now be used to cast operands of the types DATS and TIMS to CHAR (if the length of the target type is sufficient). The following changes have been made: In CAST expressions, the restriction no longer applies that casts of operands of the types DEC, CURR, and QUAN to the same types expect the target type to be long enough. In casts from NUMC to NUMC, however, the lengths must now match exactly. In the conversion functions UNIT_CONVERSION and DECIMAL_SHIFT, the result type was set to the data type QUAN or CURR with length 31 and 14 decimal places. In the function DATS_ADD_MONTHS, an invalid input date is now initialized or set to the value 00010101 and no longer produces an error. </td>
        </tr>
<tr>
            <td>Extensions</td>
            <td>The statement EXTEND VIEW for CDS DDIC-based view extensions was expanded as follows: CDS associations for the SELECT statement of the extended CDS view can now be specified after EXTEND VIEW. The following can now be specified in the extension list select_list_extension: Input parameters of the extended CDS view Path expressions for dedicated CDS associations and CDS associations of the extended CDS view Special functions </td>
        </tr>
<tr>
            <td rowspan="6">ABAP_SQL</td>
            <td>SQL Expressions</td>
            <td>From ABAP release 7.61, other SQL expressions can be used as operands in a cast expression and FLTP is not the only data type that can be specified. </td>
        </tr>
<tr>
            <td>ON Conditions</td>
            <td>From ABAP release 7.61, it is possible to Use SQL expressions of the left side of the ON condition of any join. Use the expression IS [NOT] NULL in an ON condition of an outer join. </td>
        </tr>
<tr>
            <td>Unions</td>
            <td>As of ABAP release 7.61, the set operator UNION can also be used in subqueries. </td>
        </tr>
<tr>
            <td>Host Expressions</td>
            <td>From ABAP release 7.61, host expressions can also be specified as an operand n after UP TO and PACKAGE SIZE in the SELECT statement. </td>
        </tr>
<tr>
            <td>Access to CDS Entities</td>
            <td>From ABAP release 7.61, the new CDS table functions can also be specified as data sources of a SELECT statement alongside CDS views. If an input parameter of a CDS entity is annotated with the new annotation @Environment.systemField, ABAP SQL can pass the system value that matches the value of the annotation implicitly. The annotation value #CLIENT even prevents an actual parameter from being passed to input parameters explicitly that are annotated in this way for client IDs. </td>
        </tr>
<tr>
            <td>Arrangement of SELECT Clauses and FROM Clauses</td>
            <td>From ABAP release 7.61, the FROM clause of a SELECT statement can also be specified in front of the SELECT clause. In this case, the SELECT clause must be introduced using the new addition FIELDS. This arrangement supports tools such as Code Completion. </td>
        </tr>
<tr>
            <td rowspan="3">AMDP</td>
            <td>AMDP Table Functions</td>
            <td>Alongside the existing AMDP procedures, the AMDP framework now also supports AMDP table functions in the form of table functions in the SAP HANA database. AMDP table functions now have the new addition BY DATABASE FUNCTION of the statement METHOD in implementations of AMDP methods in AMDP classes. These methods are known as AMDP function implementations to distinguish them from the existing AMDP procedure implementations. Unlike AMDP procedure implementations, AMDP function implementations have a tabular return value, but cannot be called like regular functional methods in ABAP. </td>
        </tr>
<tr>
            <td>AMDP Table Functions for AMDP Methods</td>
            <td>In other AMDP methods and using the associated SQL syntax, it is possible to access the AMDP table function of an AMDP function implementation with an explicitly defined parameter interface </td>
        </tr>
<tr>
            <td>AMDP Table Functions for CDS Table Functions</td>
            <td>An AMDP function implementation in whose declaration the addition FOR TABLE FUNCTION is specified implements a CDS table function from ABAP CDS. The parameter interface of an AMDP function implementation of this type is specified by the definition of the CDS table function. The associated AMDP function is executed as a data source of an Open ABAP SQL read statement when accessing the CDS table function directly or indirectly. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 760</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="2">ABAP_CDS</td>
            <td>CDS Access Control</td>
            <td>ABAP CDS access control was expanded to include implicit evaluations of CDS roles defined in the ABAP CDS CDS DCL in ABAP SQL. If a CDS entity is linked with a CDS role, an additional access condition is checked by default when the CDS entity is accessed using ABAP SQL. Only that data is read for which the current user has an authorization or that matches a literal condition. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: The new SQL functions LENGTH, LTRIM,RTRIM, BINTOHEX, and HEXTOBIN are available for CDS views. The special date functions DATS_DAYS_BETWEEN, DATS_ADD_DAYS, and DATS_ADD_MONTHS make it possible to calculate with values of the built-in dictionary type DATS in CDS views. Data elements can now be specified as the target type in CAST expressions in a CDS view. This passes the semantic properties of the data element to the result. This also makes it possible to map more built-in types to itself than previously. </td>
        </tr>
<tr>
            <td rowspan="6">ABAP_SQL</td>
            <td>Unions</td>
            <td>From ABAP release 7.60, the set operator UNION creates the union of the result sets of two SELECT statements. </td>
        </tr>
<tr>
            <td>SQL Expressions</td>
            <td>From ABAP release 7.60, SQL expressions can be specified in the following operand positions (except in the SELECT list): Left side of any WHERE condition Left side of a HAVING condition Left side of a complex case distinction If an SQL expression can be specified, any individual literals, host variables, and host expressions can also be specified. </td>
        </tr>
<tr>
            <td>Host Expressions</td>
            <td>From ABAP release 7.60, host expressions with the syntax @( expr ) can be specified in many operand positions in which host variables are possible. For expr, all ABAP expressions can calls are possible that can be specified in general expression positions. Operand positions for host expressions in ABAP release 7.60: Operands of SQL expressions and hence all operand positions in which these are possible. Right sides of WHERE, ON, or HAVING conditions, except for LIKE and IN. Actual parameters for input parameters of CDS views. Right side of a SET expression in UPDATE. </td>
        </tr>
<tr>
            <td>Columns Specified After BETWEEN</td>
            <td>From ABAP release 7.60, numeric columns can be specified on the right side in an interval condition using BETWEEN, providing the name of the database table or view is prefixed using ~. </td>
        </tr>
<tr>
            <td>Result Type of COUNT</td>
            <td>From ABAP release 7.60, the result type of all aggregate functions COUNT is INT8. </td>
        </tr>
<tr>
            <td>CDS Path Expressions</td>
            <td>From ABAP release 7.60, path expressions closed with an element can be specified for columns specified in SELECT statements that access CDS views with CDS associations exposed for outside use. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_UNIT</td>
            <td>Test Seams and Injections</td>
            <td>Test seams and injections have been introduced for unit tests in ABAP Unit. When tests are executed, code wrapped in TEST-SEAM and END-TEST-SEAM as part of production code can be replaced by code from test classes wrapped by TEST-INJECTION and END-TEST-INJECTION. Test classes with injections must be created in test includes. With the exception of test includes for class pools and function pools, no test includes can currently be created. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>System Class for Dynamic Mapping</td>
            <td>The new system class CL_ABAP_CORRESPONDING makes it possible to specify dynamic mapping rules for the component-by-component assignment of structures and internal tables. </td>
        </tr>
<tr>
            <td rowspan="2">EXCEPTIONS</td>
            <td>New System Interface for Messages</td>
            <td>The new system interface IF_T100_DYN_MSG adds predefined attributes for the message type and the placeholders of the message to the interface IF_T100_MESSAGE. IF_T100_DYN_MSG makes it possible to associate any message with exception classes. </td>
        </tr>
<tr>
            <td>MESSAGE Addition for RAISE EXCEPTION and THROW</td>
            <td>The new addition MESSAGE of the statement RAISE EXCEPTION and of the addition THROW in a conditional expression associates any message with an exception object. The exception class in question must include the new system interface IF_T100_DYN_MSG. It is also possible to use the addition with exception classes that include only the system interface IF_T100_MESSAGE, but with restrictions. </td>
        </tr>
<tr>
            <td rowspan="2">EXPRESSIONS</td>
            <td>Enhanced Type Interference in Constructor Expressions</td>
            <td>If the character # is specified for the result type, enhancements were made for the following constructor expressions: In the case of the constructor operator REDUCE an attempt is now made to evaluate the first declaration after INIT if the type of the operand position cannot be identified. When the constructor expressions CONV #( ... ) VALUE #( ) REDUCE #( ... ) COND #( ... ) SWITCH #( ... ) are passed to generically typed formal parameters, no type could be derived for # from the operand position until now. From ABAP release 7.60, a concrete type is derived for # for generic formal parameter types where this is possible and feasible if this cannot be determined in any other way. This prevents syntax errors when procedures are called in cases where a previously concrete type of a formal parameter is expanded to a generic type. </td>
        </tr>
<tr>
            <td>Object Component Selector After Table Expressions</td>
            <td>From ABAP release 7.60, the object component selector -&gt; can be specified directly after table expressions that return a reference variable. This makes it possible to access components of the referenced object. An exception are table expressions whose result is determined with the value operator VALUE. </td>
        </tr>
  </table>
</details>  



<p align="right"><a href="#top">⬆️ back to top</a></p>

## Standard ABAP Documentation Release News

<details>
  <summary>🟢 Release 758</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="31">ABAP_CDS</td>
            <td>CDS Analytical Projection View, Projected Entity</td>
            <td>CDS analytical projection views can now also have analytical dimension views as projected entity. </td>
        </tr>
<tr>
            <td>@AbapCatalog.preserveKey is Obsolete</td>
            <td>The CDS annotation @AbapCatalog.preserveKey is obsolete and has partly a different behavior now. The annotation was used before Release 7.58 to define the key fields of the CDS-managed DDIC view of an obsolete CDS DDIC-based view. The annotation can be kept in existing data definitions, but it no longer has an effect any more and leads to a syntax check warning. The key fields of the CDS-managed DDIC view are always defined by the addition key in the SELECT list. The change in behavior between 7.57 and 7.58 is incompatible. The key fields of the CDS-managed DDIC view of an obsolete CDS DDIC-based view are defined differently now for views where the annotation was not specified or where it was specified with the value false. The value false was used to define the key fields of the CDS-managed DDIC view as for a DDIC database views in ABAP Dictionary, regardless of the addition KEY. It was also the standard value when the annotation was not specified. Now, the behavior is always as for the value true. </td>
        </tr>
<tr>
            <td>Release of CDS Simple Types</td>
            <td>CDS simple types define elementary data types natively in ABAP CDS. A CDS simple type can be enriched with metadata using CDS annotations. The syntax statement for defining a CDS simple type is DEFINE TYPE. </td>
        </tr>
<tr>
            <td>Release of CDS Enumerated Types</td>
            <td>CDS enumerated types define enumerated types natively in ABAP CDS. The syntax statement for defining a CDS enumerated type is DEFINE TYPE ENUM. CDS enumerated types can be used as follows: In ABAP CDS for typing and casting, as operands in expressions, and in comparisons. In ABAP for typing after the TYPES statement. In ABAP SQL as elementary operands and in cast expressions. </td>
        </tr>
<tr>
            <td>Annotation Environment.sql.passValue, Scope Enhancement</td>
            <td>The ABAP annotation Environment.sql.passValue is now also available for CDS parameters in CDS view entities, CDS projection views, and CDS hierarchies. The annotation scope has been enhanced. </td>
        </tr>
<tr>
            <td>New Annotation Environment.sql.passValueForClient</td>
            <td>A new ABAP annotation is available in CDS view entities, CDS projection views, and CDS hierarchies: Environment.sql.passValueForClient. It works in a similar way to the annotation Environment.sql.passValue, but for client fields. It specifies whether a placeholder ? or a literal value is passed to the database in an ABAP SQL condition when the client field is compared with a host variable. </td>
        </tr>
<tr>
            <td>New Cardinality Syntax for Associations and Joins</td>
            <td>A new syntax for specifying the cardinality of CDS associations, CDS joins, and of filter conditions of CDS path expressions is now available: {MANY | ONE | EXACT ONE} TO {MANY | ONE | EXACT ONE} This syntax allows a source and a target cardinality to be specified, while the previously available numeric syntax only allowed the target cardinality to be specified. The new cardinality syntax can be used to improve query performance. It is available in CDS view entities, CDS projection views, CDS custom entities, and CDS abstract entities. </td>
        </tr>
<tr>
            <td>CDS Scalar Functions</td>
            <td>The new CDS entity is available: the CDS scalar function. It is defined using the CDS DDL statement DEFINE SCALAR FUNCTION. As of this release, scalar functions are available for two different runtimes: The HANA SQL runtime. SQL-based scalar functions can be created internally at SAP and externally by customers and partners. The ABAP Analytical Engine. Analytical scalar functions can be created internally at SAP only. They are delivered to customers. </td>
        </tr>
<tr>
            <td>Extension of the CAST Matrix</td>
            <td>The matrix of types that can be converted to each other using CAST has been enhanced for CDS DDIC-based views (obsolete) and for CDS view entities. Data types DECFLOAT16 and DECFLOAT34 can now be cast into data type CURR. See topic CDS DDL - DDIC-Based View, cast_expr and topic CDS DDL - CDS View Entity, cast_expr. </td>
        </tr>
<tr>
            <td>CDS Transactional Interface, WHERE Clause</td>
            <td>A WHERE condition is now available also for CDS transactional interfaces to restrict the result set that is returned when the transactional interface is accessed. </td>
        </tr>
<tr>
            <td>Further Operand Positions for Typed Literals</td>
            <td>Typed literals can now be used in more operand positions in ABAP CDS. They can now be used in the WHERE condition of CDS projection views and in the ON condition of CDS associations. </td>
        </tr>
<tr>
            <td>$projection References on Path Expressions</td>
            <td>A reuse expression $projection.Field can now also be used to reuse fields selected via a path expression. </td>
        </tr>
<tr>
            <td>New Type of Access Rule for CDS Projection Views</td>
            <td>A new type of access rule is available for CDS projection views of type CDS transactional query: the projection_rule defined with the statement GRANT SELECT ON ... AS PROJECTION ON ... FALLBACK ASSOCIATION .... </td>
        </tr>
<tr>
            <td>New DCL Functions</td>
            <td>Two new DCL functions are available: SWITCH_RUNTIME_STATE TOGGLE_RUNTIME_STATE Both functions retrieve the runtime state of a switch in the Switch Framework. </td>
        </tr>
<tr>
            <td>Interface BDEF, New Field Characteristics</td>
            <td>In RAP interface behavior definitions, the following RAP field characteristics are now available: readonly mandatory:create readonly:update </td>
        </tr>
<tr>
            <td>Interface BDEF, use event</td>
            <td>In RAP interface behavior definitions, RAP business events can be reused using the syntax use event. </td>
        </tr>
<tr>
            <td>Managed and Unmanaged RAP BO, New Field Characteristic</td>
            <td>A new RAP field characteristic is available for managed and unmanaged RAP BOs: notrigger[:warn]. Fields with this attribute must not be used in a trigger condition of a RAP validation or a RAP determination. </td>
        </tr>
<tr>
            <td>Static Default Factory Actions</td>
            <td>The syntax addition default is available for static factory actions in managed, unmanaged, and projection BDEFs. Exactly one static factory action per RAP BO entity can be defined as default static factory action. The addition default is evaluated by consuming frameworks, such as OData. </td>
        </tr>
<tr>
            <td>RAP Derived Events</td>
            <td>RAP derived events allow the reuse of existing RAP business events with a custom payload. RAP derived events are available in managed and unmanaged RAP BOs and in base BDEF extensions. They are defined using the syntax managed event. </td>
        </tr>
<tr>
            <td>RAP Change Documents</td>
            <td>RAP change documents are now available. They can be used to log changes of persisted RAP BO data in a change document object. </td>
        </tr>
<tr>
            <td>RAP Side Effects</td>
            <td>RAP side effects define interdependencies among RAP BO properties that trigger a reload of the affected properties on the user interface. Side effects are translated into annotations in the OData metadata of a RAP service. The syntax for defining RAP side effects is side effects. </td>
        </tr>
<tr>
            <td>Draft Action Activate Optimized</td>
            <td>The optional addition optimized is now available for the draft action Activate. SAP recommends always using this addition, since it speeds up the activation of draft instances. </td>
        </tr>
<tr>
            <td>Authorization Control on Action Level</td>
            <td>Two new RAP BO operation additions are available for actions and determine actions: authorization:global authorization:instance Both of these additions are specified on the action level and they replace the authorization control that is specified in the authorization master entity. In managed, unmanaged, and projection BDEFs, these additions are optional. In base BDEF extensions, authorization control on the action level is mandatory. </td>
        </tr>
<tr>
            <td>with managed instance filter</td>
            <td>The optional addition with managed instance filter is available for projection BDEFs and interface BDEFs. If specified, the WHERE condition of the underlying CDS transactional query or CDS transactional interface is evaluated when the BDEF is accessed with ABAP EML or OData requests from Web clients. </td>
        </tr>
<tr>
            <td>Abstract BDEF Extensions</td>
            <td>The new statement extension for abstract of the RAP BDL makes it possible to add abstract BDEF extensions to abstract behavior definitions. Abstract BDEFs are mainly used as parameters for RAP actions, RAP functions, and RAP business events. An abstract BDEF extension allows you to extend these parameters. </td>
        </tr>
<tr>
            <td>Abstract BDEF, mandatory:execute for Associations</td>
            <td>The optional addition mandatory:execute is now available for associations in abstract BDEFs. It declares the association in question as mandatory, that is, whenever the abstract BDEF is used as an input parameter for a RAP action or a RAP function, the %control flag must be set for this association. </td>
        </tr>
<tr>
            <td>Redefined Parameters, Projection or Interface BDEF</td>
            <td>The input or output parameter of a RAP action, RAP function, or a RAP business event that is reused in a projection BDEF or an interface BDEF can optionally be replaced with a dedicated projection-specific structure using the syntax additions deep parameter or deep result. </td>
        </tr>
<tr>
            <td>BDEF Extension Layering</td>
            <td>It is now possible to extend a BDEF extension with further BDEF extensions. This is referred to as extension layering. As a prerequisite, the BDEF extension in question must be explicitly enabled for extensibility. The rules are described in the topic RAP - Extensibility Enabling for BDEF Extensions. Technically, the BDEF extension that extends another extension is a regular extension and the respective rules apply. </td>
        </tr>
<tr>
            <td>Draft Action AdditionalSave</td>
            <td>A new draft action is available, draft action AdditionalSave. This draft action allows users to define a custom saving strategy for draft instances. It is intended in particular for draft actions with a user-defined implementation, defined using the addition with additional implementation. </td>
        </tr>
<tr>
            <td>Authorization Context for Disable, New Options</td>
            <td>There are further options available for the authorization context for disable after the addition for disable: save:early: Skips the authorization checks defined in the respective authorization context in the early save phases finalize and check_before_save. save:late: Skips the authorization checks defined in the respective authorization context in the late save phases adjust_numbers, save, and save_modified. </td>
        </tr>
<tr>
            <td>BOPF-Based RAP Business Objects</td>
            <td>A migration tool has been released to migrate CDS-based BOPF business objects to BOPF-based RAP business objects. For details on BOPF-based RAP business objects, see the topic RAP - BOPF-Based RAP Business Objects. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_DOCU</td>
            <td>External Mailbox for Documentation</td>
            <td>From Release 7.58 on, the ABAP Keyword Documentation display offers a function to send a feedback mail to f1_help@sap.com in non-SAP development systems and in the SAP Help Portal. Up to now, the possibility for a feedback mail existed only in SAP's own development systems. The function can be switched on or off in the configuration of the ABAP Keyword Documentation, see SAP Note 3051036. </td>
        </tr>
<tr>
            <td>ADT Documentation Layout</td>
            <td>From Release 7.58 on, the ABAP Keyword Documentation presents a new layout in ABAP development tools for Eclipse (ADT) where a toggle between Standard ABAP and ABAP Cloud is also available. The new layout includes: New header bar with links to the mail feedback and the web version Highlighted key blocks for hints, code blocks, and others Tables with zebra stripes </td>
        </tr>
<tr>
            <td rowspan="4">ABAP_SQL</td>
            <td>ABAP SQL Expressions on the Right Side</td>
            <td>ABAP SQL expressions can be defined on the right side of a condition enclosed in parentheses. </td>
        </tr>
<tr>
            <td>New Cardinality Syntax for Joins</td>
            <td>A new syntax for specifying the cardinality of joins is available: {MANY | ONE | {EXACT ONE}} TO {MANY | ONE | {EXACT ONE}} This syntax allows specifying a source and a target cardinality, while the previously available numeric syntax only allowed specifying the target cardinality. The new cardinality syntax can be used to improve query performance. It can also be used in SQL path expressions and CTE associations. </td>
        </tr>
<tr>
            <td>Access to Multiple Internal Tables</td>
            <td>It is now possible to process multiple internal tables accessed with FROM @itab within one ABAP SQL statement with the ABAP SQL in-memory engine. Currently, this is restricted to joins of internal tables where no database tables are involved. </td>
        </tr>
<tr>
            <td>New Addition PRIVILEGED ACCESS</td>
            <td>The new addition PRIVILEGED ACCESS can be used to disable CDS access control for a complete SELECT statement. </td>
        </tr>
<tr>
            <td rowspan="1">ADBC</td>
            <td>ADBC Interfaces</td>
            <td>The ADBC interface based on classes is now replaced by interfaces. Using ADBC with classes is now obsolete. </td>
        </tr>
<tr>
            <td rowspan="1">AMDP</td>
            <td>New AMDP Option CLIENT INDEPENDENT</td>
            <td>The new addition CLIENT INDEPENDENT can be used after AMDP OPTIONS to declare an AMDP method as client-independent. When used, only client-independent database objects can be accessed and specified after the addition USING. </td>
        </tr>
<tr>
            <td rowspan="2">ASSIGNMENTS</td>
            <td>Returning a Value with RETURN</td>
            <td>In functional methods, the statement RETURN can be used to assign the result of an expression expr to the return value when terminating the method. </td>
        </tr>
<tr>
            <td>DEFAULT as New Addition to the CORRESPONDING Operator</td>
            <td>The addition DEFAULT allows the assignment of values for a target component based on an expression. </td>
        </tr>
<tr>
            <td rowspan="7">EML</td>
            <td>SET ENTITIES</td>
            <td>It is now possible block or unblock the ABAP EML access to RAP business objects using SET ENTITIES statements. </td>
        </tr>
<tr>
            <td>RAISE ENTITY EVENT</td>
            <td>You can use RAISE ENTITY EVENT statements to raise a RAP business event. </td>
        </tr>
<tr>
            <td>METHODS, FOR ENTITY EVENT</td>
            <td>The METHODS, FOR ENTITY EVENT addition supports RAP event handler method definitions. </td>
        </tr>
<tr>
            <td>CLASS, FOR EVENTS OF</td>
            <td>The FOR EVENTS OF addition supports the creation of RAP event handler classes. </td>
        </tr>
<tr>
            <td>CL_ABAP_BEHAVIOR_EVENT_HANDLER</td>
            <td>Local classes that inherit from CL_ABAP_BEHAVIOR_EVENT_HANDLER can be implemented in the CCIMP include of a RAP event handler class to locally consume RAP business events. </td>
        </tr>
<tr>
            <td>CL_ABAP_TX</td>
            <td>The class CL_ABAP_TX is used to explicitly set RAP transactional phases. </td>
        </tr>
<tr>
            <td>Classifying APIs for Transactional Consistency</td>
            <td>Released APIs can contain classifications to ensure transactional consistency of custom extensions within the SAP LUW. Classifications with IF_ABAP_TX_* are available for this. IF_ABAP_TX_WEAK is now available. </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Improved Optimization of the WHERE Condition</td>
            <td>Up to now, an optimization of the WHERE condition by using key access took place only under very limited circumstances where the key value pairs had to be compared explicitly for equality and combined by AND. Now, the compiler analyzes the WHERE condition thoroughly and tries itself to extract the key/value pairs that are necessary for the key access. Furthermore, any binary comparison operators and the comparison operator BETWEEN are now also respected for sorted keys. </td>
        </tr>
<tr>
            <td rowspan="4">SYSTEM_CLASSES</td>
            <td>System Classes for Numbers</td>
            <td>Two new system classes for numeric calculations are available: CL_ABAP_BIGINT contains methods for calculations with any size of integer in ABAP. CL_ABAP_RATIONAL contains methods for calculating with rational numbers without any precision loss. </td>
        </tr>
<tr>
            <td>New Class CL_ABAP_DIFF</td>
            <td>The new class CL_ABAP_DIFF compares the content of internal tables and returns information about any differences found. </td>
        </tr>
<tr>
            <td>New Methods in CL_ABAP_TSTMP</td>
            <td>The new methods MOVE_TRUNC, MOVE_TO_SHORT_TRUNC, ADD_TO_SHORT_TRUNC and SUBTRACTSECS_TO_SHORT_TRUNC of system class CL_ABAP_TSTMP round the fractional seconds of long UTC time stamps down while the existing methods MOVE, MOVE_TO_SHORT, ADD_TO_SHORT and SUBTRACTSECS_TO_SHORT round commercially. Since the latter might be unexpected, the new methods can be used now in order to preserve the integer part of a long time stamp. </td>
        </tr>
<tr>
            <td>New Method in CL_ABAP_BEHV_AUX</td>
            <td>Using the new method GET_CURRENT_PHASE, you get information about the current RAP transactional phase. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>Absolute Type Names for Line Types</td>
            <td>The new specification \LINE allows the line type of an internal table to be specified in an absolute type name. \LINE can be followed by -comp to specify the type of a component of the line type. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 757</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="36">ABAP_CDS</td>
            <td>CDS View Entity, Table Buffering</td>
            <td>CDS view entity buffering was enhanced: View on view buffering is now supported, under the precondition that the CDS view entity used as data source meets certain requirements. </td>
        </tr>
<tr>
            <td>CDS View Entity, CASE ELSE NULL</td>
            <td>In CDS view entities, the addition ELSE NULL is available in simple and complex case distinctions. It defines the null value as return value of the ELSE branch. </td>
        </tr>
<tr>
            <td>CDS View Entity, Extended Cast Matrix</td>
            <td>New casting options have been added in CDS view entities. Casting from data type FLTP into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data type SSTRING into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data type CHAR into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data type DATS into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data type TIMS into data types DEC, CURR, QUAN, INT1, INT2, INT4, INT8, DECFLOAT16, and DECFLOAT34 is now possible. Casting from data types DECFLOAT16 and DECFLOAT34 into data type CHAR is now possible. </td>
        </tr>
<tr>
            <td>CDS View Entity, SUBSTRING Function</td>
            <td>In CDS view entities, the SUBSTRING function has been enhanced. It now accepts not only literals, but also fields, parameters, expressions, and built-in functions as arguments for parameters pos and len. </td>
        </tr>
<tr>
            <td>CDS View Entity, LEFT and RIGHT</td>
            <td>In CDS view entities, the functions LEFT and RIGHT have been enhanced. They now accept literals, fields, parameters, expressions, and built-in functions as argument for the parameter len. </td>
        </tr>
<tr>
            <td>CDS View Entity, Application Session Variables</td>
            <td>In CDS view entities, two new application session variables are available: bs_system_id bs_zone_id </td>
        </tr>
<tr>
            <td>ABAP Program That Lists Usages of CDS-Managed DDIC Views</td>
            <td>Using CDS-managed DDIC views has been declared obsolete. The following ABAP program lists all repository objects that use CDS-managed DDIC views: RUT_WHERE_USE_SQLVIEW. </td>
        </tr>
<tr>
            <td>CDS Analytical Projection Views</td>
            <td>CDS analytical projection views for modelling analytical queries are available. A CDS analytical projection view is defined using DEFINE TRANSIENT VIEW ENTITY AS PROJECTION ON. The value for the provider contract must be set to ANALYTICAL_QUERY. </td>
        </tr>
<tr>
            <td>CDS Transactional Interface</td>
            <td>A new type of CDS projection view is available: the CDS transactional interface. CDS transactional interfaces serve as stable public interface layer in a CDS data model. They are typically used in the context of the ABAP RESTful Application Programming Model to provide the basis for a RAP BO interface. A CDS transactional interface view is defined using DEFINE VIEW ENTITY AS PROJECTION ON. The value for the provider contract must be set to TRANSACTIONAL_INTERFACE. </td>
        </tr>
<tr>
            <td>Comparisons with CDS Amount Fields and CDS Quantity Fields</td>
            <td>Special handling for CDS amount fields and CDS quantity fields for comparisons in CDS view entities has been implemented. </td>
        </tr>
<tr>
            <td>Support of Input Parameters of Type abap.string</td>
            <td>Data type abap.string is now supported for input parameters in the parameter list of a CDS view entity and in the parameter list of a CDS table function. Data type abap.string is now supported when binding actual parameters to the input parameters of a CDS view entity, if a CDS table function is used as data source after FROM. </td>
        </tr>
<tr>
            <td>CDS Custom Entity Extensions</td>
            <td>The new statement EXTEND CUSTOM ENTITY of the DDL of ABAP CDS makes it possible to add new elements to existing CDS custom entities by using CDS custom entity extensions. </td>
        </tr>
<tr>
            <td>CDS DDIC-Based Views Are Obsolete</td>
            <td>CDS DDIC-based views (obsolete), defined using the statement DEFINE VIEW, are obsolete. When creating new data models, CDS view entities, defined using DEFINE VIEW ENTITY, should be used instead. </td>
        </tr>
<tr>
            <td>CDS Access Control Context</td>
            <td>An access control context is a hierarchical data structure with nodes that can have other nodes and values as child nodes. It provides access roles with specific information about the type of data selection that is being performed. Two functions are available to query the access control context: CONTEXT_NODE_EXISTS CONTEXT_NODE_VALUES Note: The access control context and the relating functions are available only internally at SAP. </td>
        </tr>
<tr>
            <td>CONTEXT_NODE_VALUES, New Parameter datatype</td>
            <td>The access control context function CONTEXT_NODE_VALUES now has a further optional keyword parameter: datatype. This parameter specifies a DDIC data element that is used to represent the context node values at runtime before they are used in the comparison operations. </td>
        </tr>
<tr>
            <td>IF ... THEN ... ELSE ... Control Structure Available in Access Conditions</td>
            <td>Control structures IF ... THEN ... ELSE ... can now be used as part of an access condition after the DEFINE ROLE statement. </td>
        </tr>
<tr>
            <td>DEFINE ROLE Statement Has New Addition SWITCHABLE</td>
            <td>The new addition SWITCHABLE is now available for the DEFINE ROLE statement. Using this addition, the content of the role reacts to status changes in the Switch Framework with respect to the package of the access control. </td>
        </tr>
<tr>
            <td>RAP Late Numbering for Managed and for Draft-Enabled RAP BOs</td>
            <td>RAP late numbering is now also available for managed RAP BOs, managed draft-enabled RAP BOs, and for unmanaged draft-enabled RAP BOs. </td>
        </tr>
<tr>
            <td>Instance-Bound Factory Actions for Managed RAP BOs</td>
            <td>Instance-bound factory actions are now also available for managed RAP BOs. </td>
        </tr>
<tr>
            <td>BDEF Privileged Mode for RAP Projection BOs</td>
            <td>A new syntax variant of with privileged mode is now available for RAP projection BOs. </td>
        </tr>
<tr>
            <td>CDS Interface Behavior Definitions</td>
            <td>A new implementation type is available: the RAP interface behavior definition. Such interface BDEFs are based on CDS transactional interfaces and define the behavior of a RAP BO interface. The overall purpose of a RAP BO interface is to project a business object for stable consumption. </td>
        </tr>
<tr>
            <td>Managed RAP BO, New Field Characteristic</td>
            <td>In managed RAP business objects, a new field characteristic is available: field(suppress). It removes the field in question from the BDEF derived types and from all RAP APIs. </td>
        </tr>
<tr>
            <td>Unmanaged RAP BO, New Field Characteristic</td>
            <td>In unmanaged RAP business objects, a new field characteristic is available: field(suppress). It removes the field in question from the BDEF derived types and from all RAP APIs. </td>
        </tr>
<tr>
            <td>Managed RAP BO, with full data</td>
            <td>In managed RAP business objects, the optional addition with full data can be used in combination with one of the RAP saving options to ensure that full instance data is handed over to the save_modified method of the RAP saver class in the ABAP behavior pool. </td>
        </tr>
<tr>
            <td>Variants to Define a RAP Own Authorization Context</td>
            <td>New variants are available for defining a RAP own authorization context: define own authorization context by privileged mode; define own authorization context by privileged mode and { ... } </td>
        </tr>
<tr>
            <td>BDEF Extensions for RAP BOs</td>
            <td>BDEF extensions for RAP BOs can be defined using the statement extension. </td>
        </tr>
<tr>
            <td>BDEF Extensions for RAP Projection BOs</td>
            <td>BDEF projection extensions for RAP projection business objects can be defined using the statement extension for projection. </td>
        </tr>
<tr>
            <td>New Version of BDEF Strict Mode</td>
            <td>A new version of BDEF strict mode is available: Strict mode version 2, specified using strict(2). It applies even more syntax checks than the first version. </td>
        </tr>
<tr>
            <td>Draft Query Views for Draft-Enabled RAP BOs</td>
            <td>The new syntax addition query is available to define a draft query view for a draft-enabled RAP BO. This addition is optional. Only in the context of RAP extensibility is it a mandatory prerequisite for the C0 release of the CDS behavior definition in question. </td>
        </tr>
<tr>
            <td>Abstract BDEF, New Field Characteristic</td>
            <td>In RAP abstract behavior definitions, a new RAP field characteristic is available: mandatory:execute. It declares the field in question as mandatory. Whenever the hierarchical type is used as input parameter for a RAP action or a RAP function, a value must be supplied. </td>
        </tr>
<tr>
            <td>result selective for Deep Result Types</td>
            <td>The addition result selective is now also possible for deep result types. It can be specified for an output parameter of a RAP action or RAP function to return only parts of the result structure. </td>
        </tr>
<tr>
            <td>RAP Business Events</td>
            <td>Using RAP business events, the ABAP RESTful Application Programming Model now offers native support for event-driven architecture. RAP business events are defined with the language element event. A flat or deep output parameter can optionally be specified. </td>
        </tr>
<tr>
            <td>RAP Save Actions with Phase Specification</td>
            <td>RAP save actions can now specify one or both of the RAP saver methods finalize or adjustnumbers in brackets to indicate the RAP saver method during which the action can be executed. </td>
        </tr>
<tr>
            <td>Repeatable RAP Actions and Functions</td>
            <td>RAP actions and RAP functions can be defined as repeatable. This syntax addition explicitly allows multiple executions of the same action or function on the same RAP BO entity instance within the same ABAP EML or OData request. The BDEF derived type of a repeatable action or function has an extra %cid component, in contrast to the BDEF derived type of non-repeatable actions or function. </td>
        </tr>
<tr>
            <td>CDS Service Definition Extension</td>
            <td>It is now possible to define service definition extensions in CDS SDL using the statement EXTEND SERVICE. </td>
        </tr>
<tr>
            <td>CDS Service Definition, Provider Contract</td>
            <td>The new statement PROVIDER CONTRACTS is now available for CDS service definitions. A provider contract defines the type of service binding that is to be used for a service definition. The effect is that stricter syntax checks are applied. </td>
        </tr>
<tr>
            <td rowspan="4">ABAP_SQL</td>
            <td>Support for ORDER BY in the ABAP SQL In-Memory Engine</td>
            <td>The ABAP SQL in-memory engine fully supports the ORDER BY clause now: ORDER BY no longer bypasses table buffering. Before, it bypassed table buffering in cases where single columns were specified as sort keys and these columns were not a left-aligned subset of the primary key in the correct order or if the addition DESCENDING was specified for a column. ORDER BY can now be used with SELECT FROM @itab without transporting the internal table to the database. This allows looping over an internal table in a sequence defined by ORDER BY and the sorting of internal tables by expressions. </td>
        </tr>
<tr>
            <td>ORDER BY n</td>
            <td>In the column list of the ORDER BY clause, a literal or a host constant of an integer type is handled as the column position in the result set of the query. This feature is undocumented and behaves partly undefined. With release 8.16 the behavior is defined and documented. </td>
        </tr>
<tr>
            <td>String Functions Processed by the ABAP SQL In-Memory Engine</td>
            <td>The string functions LEFT, LOWER, RIGHT, UPPER, and SUBSTRING can be processed by the ABAP SQL in-memory engine now. They no longer bypass table buffering and do not cause the transport of an internal table accessed by FROM @itab to the database. While LEFT, LOWER, RIGHT, and UPPER were not supported at all, SUBSTRING was supported under certain conditions. </td>
        </tr>
<tr>
            <td>Table Buffering for Decimal Floating Point Calculations</td>
            <td>Calculations for the built-in types DECFLOAT16 and DF16_DEC no longer bypass table buffering. The calculation in the table buffer is done with the respective decimal floating point arithmetic. The following differences to ABAP and to the database must be considered: In ABAP, the calculation type for decfloat16 is decfloat34. On the database, the built-in type DF16_DEC is handled as a packed number. Both can lead to different results. </td>
        </tr>
<tr>
            <td rowspan="3">ADBC</td>
            <td>Generic Method EXECUTE</td>
            <td>The new method EXECUTE of ADBC class CL_SQL_STATEMENT can be used to replace specific methods, see ADBC - All Statements. </td>
        </tr>
<tr>
            <td>Switching Result Sets</td>
            <td>The new method NEXT_RESULT_SET of ADBC class CL_SQL_RESULT_SET can be used to access more than one result set, see example ADBC - Multiple Result Sets. </td>
        </tr>
<tr>
            <td>New Method for Closing a Connection</td>
            <td>The class CL_SQL_CONNECTION has a new method CLOSE_NO_DISCONNECT that closes an ADBC connection object and sets the database connection to inactive instead of closing it completely. Such a connection must not be reopened but can be reused. </td>
        </tr>
<tr>
            <td rowspan="2">ASSIGNMENTS</td>
            <td>New Dynamic Component Specification in ASSIGN</td>
            <td>Components of structures can be assigned to field symbols with the new syntax struc-(comp) that largely replaces the variant ASSIGN COMPONENT OF. </td>
        </tr>
<tr>
            <td>New Addition ELSE UNASSIGN</td>
            <td>The new addition ELSE UNASSIGN can be specified for the following variants of the statement ASSIGN: dynamic assignments assignments of dynamic components dynamic access assignment of a table expression It can be also specified with the addition ASSIGNING of the following internal table statements: READ TABLE LOOP AT itab INSERT MODIFY All these statements have set sy-subrc. If an assignment is not successful, sy-subrc is set to the value 4 or sometimes 8. If the addition ELSE UNASSIGN is specified, the state of the field symbol is set to unassigned in that case. The field symbol is assigned only if the assignment is successful. If ELSE UNASSIGN is not specified, the field symbol keeps its previous state, which was the only behavior up to now. Using ELSE UNASSIGN introduces the same behavior as for the static variant to the above variants. In another way around, one can say that the static variant uses ELSE UNASSIGN implicitly. </td>
        </tr>
<tr>
            <td rowspan="1">DDIC</td>
            <td>Deprecation of DDIC External Views</td>
            <td>SAP HANA XSA replaces SAP HANA XSC. The access methods designed specifically for XSC objects are not suitable for XSA objects and have been declared obsolete. DDIC external views are one of those access methods. They have been declared obsolete. More information can be found in SAP Notes 2465027 and 3116165. </td>
        </tr>
<tr>
            <td rowspan="1">EML</td>
            <td>Entity Manipulation Language</td>
            <td>RAISE ENTITY EVENT It is now possible to raise a RAP entity event using this statement. READ and MODIFY The FIELDS ( ... ) WITH addition for READ and MODIFY supports the setting of %control regarding components in deep input parameters in the context of deep action and function parameters. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Inline Declarations</td>
            <td>The new declaration operator FINAL declares an immutable variable that cannot be assigned another value at other write positions of the same context. </td>
        </tr>
<tr>
            <td rowspan="3">ITAB</td>
            <td>New Addition STEP for Defining Step Size and Processing Order</td>
            <td>The new addition STEP defines the step size and the order for processing an internal table. For the statements LOOP and FOR, STEP can be used to control the step size and the processing order. For the statements APPEND, DELETE, INSERT, VALUE, and NEW, STEP can only be used to define the step size. It is not possible to change the processing order with STEP for these statements. </td>
        </tr>
<tr>
            <td>Exception when Mixing Index Access with Hash Key Access</td>
            <td>Access to a table index when accessing an internal table using a hash key (accessing a hashed table using its primary key or accessing any internal table using a hashed secondary key) is not allowed. When a hashed key is specified dynamically behind USING KEY in statement LOOP AT or expression FOR ... IN, usage of FROM and TO must result in an exception. This was not the case before release 7.57. From release 7.57 on, the runtime error ITAB_ILLEGAL_INDEX_OP occurs in such a situation. Before release 7.57, the behavior was undefined. This change is slightly incompatible. SAP-internal hint Before release 7.57, FROM n: LOOP over table not changed by DELETE or SORT starts with n + 1, LOOP over table changed by DELETE or SORT starts indeterministic. TO: Addition is ignored in a non-debug kernel or leads to runtime error ABAP_ASSERT in a debug kernel. </td>
        </tr>
<tr>
            <td>Correction for FROM Addition</td>
            <td>When a negative value is specified for FROM in statement LOOP AT or expression FOR ... IN, it is set to 1 implicitly. Before release 7.57, this was not the case in the following situation: The internal table is accessed using a sorted key. The internal table contains more than 10 table lines. A WHERE condition is specified that can be optimized. The loop was not processed at all. From release 7.57 on, the loop is processed as documented. This change is slightly incompatible. </td>
        </tr>
<tr>
            <td rowspan="4">MISC</td>
            <td>Support of ABAP Language Versions</td>
            <td>The obsolete ABAP language versions Static ABAP with restricted object use Standard ABAP with restricted object use are not supported any more and must no longer be used. </td>
        </tr>
<tr>
            <td>Obsolete Access Methods for SAP HANA XS</td>
            <td>SAP HANA XSA replaces SAP HANA XSC. The access methods designed specifically for XSC objects are not suitable for XSA objects and have been declared obsolete. This includes DDIC external views and database procedure proxies. More information can be found in SAP Notes 2465027 and 3116165. </td>
        </tr>
<tr>
            <td>Import of Type n into Type c</td>
            <td>The statement IMPORT now allows assignments of data of type n to data objects of type c if they have the same length. This change might lead to incompatible behavior if handling of the former exception leads to different results than the new behavior. </td>
        </tr>
<tr>
            <td>Dynamic Component</td>
            <td>The following syntax can be used for the structure component selector - to access components dynamically now: ... struct-(comp) This is especially possible for dynamic components in the statement ASSIGN. </td>
        </tr>
<tr>
            <td rowspan="1">RFC</td>
            <td>Parameter Tables in sRFC</td>
            <td>Using the additions PARAMETER-TABLE and EXCEPTION-TABLE, values for parameters and non-class-based exceptions can now be passed dynamically in sRFC. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 756</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="46">ABAP_CDS</td>
            <td>CDS Abstract Entity Extensions</td>
            <td>The new statement EXTEND ABSTRACT ENTITY of the DDL of ABAP CDS makes it possible to add new elements to existing CDS abstract entities by using CDS abstract entity extensions. </td>
        </tr>
<tr>
            <td>CDS Abstract Entity, To-Parent Association</td>
            <td>In CDS abstract entities, it is now possible to define to-parent associations without ON condition. The ON condition can be left out if the association target is also a CDS abstract entity. </td>
        </tr>
<tr>
            <td>CDS Hierarchy, Caching Policy</td>
            <td>A hierarchy definition DEFINE HIERARCHY can now use the new addition CACHE ON|OFF|FORCE to specify the caching policy for a generated hierarchy. </td>
        </tr>
<tr>
            <td>Using CDS-Managed DDIC Views is Obsolete</td>
            <td>For each CDS DDIC-based view (obsolete), a CDS-managed DDIC view (obsolete) is created in ABAP Dictionary upon activation. Using this CDS-managed DDIC view (obsolete), for example for typing or for accessing the CDS entity in question, is obsolete from now on. The reason is the release of a new type of CDS entity, the CDS view entity, which will replace CDS DDIC-based views (obsolete) in the future. CDS view entities do not have a CDS-managed DDIC view (obsolete) attached and in case of a migration from CDS DDIC-based view (obsolete) to CDS view entity, each usage of a CDS-managed DDIC view (obsolete) leads to a syntax error. </td>
        </tr>
<tr>
            <td>CDS View Entity, Reusing Expressions</td>
            <td>In CDS view entities, it is now possible to reuse expressions defined in the SELECT list in other operand positions of the same CDS view entity by using the syntax $projection.reuse_exp. </td>
        </tr>
<tr>
            <td>CDS View Entity, UNION Clause</td>
            <td>UNION clauses are now supported in CDS view entities. There are a few differences to UNION clauses in CDS DDIC-based views. The most important difference is that branches of union clauses can be nested within each other in CDS view entities. </td>
        </tr>
<tr>
            <td>CDS View Entity, DISTINCT</td>
            <td>The addition DISTINCT is now available for SELECT statements in CDS view entities. </td>
        </tr>
<tr>
            <td>CDS View Entity, New Set Operators</td>
            <td>In CDS view entities, two new set operators are available: EXCEPT INTERSECT </td>
        </tr>
<tr>
            <td>CDS View Entity, New Conversion Functions</td>
            <td>Two new conversion functions are available in CDS view entities: GET_NUMERIC_VALUE CURR_TO_DECFLOAT_AMOUNT </td>
        </tr>
<tr>
            <td>CDS View Entity, Typed Literals</td>
            <td>Typed literals are now available for CDS view entities. Typed literals allow an explicit type declaration and they are available for many built-in ABAP Dictionary data types. </td>
        </tr>
<tr>
            <td>CDS View Entity, Handling of Amounts and Quantities</td>
            <td>In CDS view entities, special handling for CDS amount fields and CDS quantity fields has been implemented. It differs from the handling of amount fields and quantity fields in DDIC, for example, more data types are possible and the currency key/unit key reference is considered in expressions. Moreover, a new type is available: the CDS calculated quantity. A CDS calculated quantity is the result type of a calculation using amount and/or quantity fields. </td>
        </tr>
<tr>
            <td>CDS View Entity, Extended Expression Matrix</td>
            <td>In CDS view entities, the expression matrix has been extended: In the WHERE clause, arithmetic expressions and case expressions are supported as operands. In the HAVING clause, arithmetic expressions and case expressions are supported as operands. In a searched case expression, arithmetic expressions and case expressions are supported as operands. </td>
        </tr>
<tr>
            <td>CDS Projection View, REDEFINE ASSOCIATION</td>
            <td>In CDS projection views, it is now possible to redefine the CDS associations from the projected entity in the header part. This is done using the keyword REDEFINE ASSOCIATION. Redefinition can include a new filter, alias name, and redirection to a new association target, which must also be a CDS projection view, thus moving the complete data model to the projection layer. </td>
        </tr>
<tr>
            <td>CDS Projection View, PROVIDER CONTRACT</td>
            <td>It is now possible to specify a provider contract for CDS projection views using the keyword PROVIDER CONTRACT. The provider contract specifies in which scenario a CDS projection view is used, and the scenario in turn determines in which runtime the view is executed and which features are available. In this release, there is only one provider contract option available: TRANSACTIONAL_QUERY. </td>
        </tr>
<tr>
            <td>New AbapCatalog Annotations</td>
            <td>The following new AbapCatalog.extensibility annotations have been released: AbapCatalog.extensibility.allowNewdataSources AbapCatalog.extensibility.dataSources AbapCatalog.extensibility.elementSuffix AbapCatalog.extensibility.extensible AbapCatalog.extensibility.quota.maximumBytes AbapCatalog.extensibility.quota.maximumFields </td>
        </tr>
<tr>
            <td>ABAP Program for Migration Analysis</td>
            <td>The following documented ABAP program is now available for evaluating whether a migration from a CDS DDIC-based view (obsolete) to a CDS view entity is possible: RUTDDLS_MIGRATION_CANDIDATES. </td>
        </tr>
<tr>
            <td>Migration Tool for CDS Views</td>
            <td>The following documented ABAP program is now available for migrating CDS DDIC-based views to CDS view entities: RUTDDLSV2MIGRATION. </td>
        </tr>
<tr>
            <td>New CDS System Entities to Generate Series</td>
            <td>The following new CDS system table functions are available: ... SERIES_GENERATE_DATE( step = ... from_value = ... to_value = ... ) ... ... SERIES_GENERATE_INTEGER( step = ... from_value = ... to_value = ... ) ... ... SERIES_GENERATE_TIME( step = ... from_value = ... to_value = ... ) ... ... SERIES_GENERATE_TIMESTAMP( step = ... from_value = ... to_value = ... ) ... </td>
        </tr>
<tr>
            <td>New Reference Type for Annotations</td>
            <td>A new reference type for annotations is available: LocalDefinitionRef </td>
        </tr>
<tr>
            <td>Graceful Behavior in Access Condition Inheritance for Missing Dictionary Elements</td>
            <td>From this release on, authors of access controls can mark a subset of the CDS elements used in their access conditions as optional, so that CDS entities which inherit their access conditions are not affected by a Day-1-impact any more. GRANT SELECT ON cds_entity WITH OPTIONAL ELEMENTS (      element1 DEFAULT (TRUE|FALSE), ...) WHERE ...  </td>
        </tr>
<tr>
            <td>Unified Syntax for Authorization Objects, Authorization Fields and SACF Scenario Names</td>
            <td>At all locations of access controls, references to authorization objects, authorization fields, and SACF scenario names can be written in identifier syntax when they comply with it and in string syntax with single apostrophes as an alternative. Before: ( ... ) = ASPECT PFCG_AUTH( S_OBJ IN SCENARIO 'THESCENARIO' ) Now allowed: ( ... ) = ASPECT PFCG_AUTH( S_OBJ IN SCENARIO TheScenario )  </td>
        </tr>
<tr>
            <td>Additional Filtering of User-Defined Aspect Usage</td>
            <td>When using user-defined aspects, these can now denominate an arbitrary set of their elements (path expressions are supported) as filter element. These filter elements can by referenced when using the user-defined aspect in an access condition. DEFINE ASPECT ... AS SELECT FROM ...   WITH USER ELEMENT ...   WITH FILTER ELEMENTS ( element1, element2 AS alias2 )   {       ...   } WHERE ( ... ) = ASPECT ... FILTER BY ( element1 = 'X' OR                                        alias2 IS NOT NULL )  </td>
        </tr>
<tr>
            <td>Condition Replacement for Role-Based Inheritance</td>
            <td>The REPLACING section formerly only available to entity-based inheritance INHERITING CONDITIONS FROM ENTITY cds_entity is now also available for role-based inheritance INHERIT role FOR GRANT SELECT ON cds_entity  </td>
        </tr>
<tr>
            <td>Generic Element Replacement for Condition Inheritance</td>
            <td>The REPLACING section of condition inheritance now supports a generic replacement step to replace an arbitrary field or association of the inheritance source with an arbitrary field or association of the inheritance target. WHERE INHERITING CONDITIONS FROM ENTITY Source REPLACING {   ELEMENT Element1OfSource WITH Element1OfTarget,   ELEMENT Assoc1OfSource WITH Assoc1[r = 4].Assoc2OfTarget,   ELEMENT Assoc2(p : $parameters.p1)[ q = 1].Field WITH MyShortField }  </td>
        </tr>
<tr>
            <td>Enabling/Disabling of Access Conditions Based on the State of SACF</td>
            <td>Respecting settings in the switchable authorization framework (SACF) was already possible for dedicated PFCG conditions: ASPECT PFCG_AUTH ( S_OBJECT IN SCENARIO ... ) Now, those settings can be used to enable or disable entire condition sets: GRANT SELECT ON cds_entity   WHERE     SACF_CHECK_IN_USE (NAME =&gt; NEW_AUTH_SWITCH ) IS INITIAL       AND     ( element ) = ASPECT PFCG_AUTH( OLD_AUTH, F )   OR     SACF_CHECK_IN_USE( NAME =&gt; NEW_AUTH_SWITCH ) IS NOT INITIAL       AND     ( element ) = ASPECT PFCG_AUTH( NEW_AUTH, F );  </td>
        </tr>
<tr>
            <td>Access Conditions for CDS Hierarchies Based on Elements of the Hierarchy Directory</td>
            <td>For CDS hierarchies, access control was restricted to the use of conditions not resulting in database filtering. Now elements located in the declared hierarchy directory DIRECTORY ... FILTER BY can be used to formulate such conditions. </td>
        </tr>
<tr>
            <td>New DCL Function</td>
            <td>The following new DCL function is available: OPTIONAL_ELEMENT_EXISTS. </td>
        </tr>
<tr>
            <td>Consideration of Special Runtime Modes</td>
            <td>When operating the system with the emergency user SAP*, CDS access control is now deactivated. During processing of an update task, PFCG conditions are now considered as fully authorized.  </td>
        </tr>
<tr>
            <td>BDEF Strict Mode</td>
            <td>BDEF strict mode is now available. It can be enabled using the syntax addition strict and it applies additional syntax checks to RAP behavior definitions. </td>
        </tr>
<tr>
            <td>Nested Determinations on Modify</td>
            <td>It is now possible to trigger a determination on modify by another determination on modify. </td>
        </tr>
<tr>
            <td>RAP BO Operation Addition authorization:update</td>
            <td>The new RAP BO operation addition authorization:update is available for managed and unmanaged BOs. It delegates the authorization control for an operation to the authorization control that is specified for the update operation. </td>
        </tr>
<tr>
            <td>Always Flag in Determine Actions</td>
            <td>The new addition always can be used for determinations and validations assigned to a determine action. When the determine action is called, determinations and validations with this flag are executed regardless of their trigger conditions. </td>
        </tr>
<tr>
            <td>Global Feature Control</td>
            <td>The new RAP BO operation addition features:global can be used to define global feature control for RAP BO operations. </td>
        </tr>
<tr>
            <td>Global Authorization Control</td>
            <td>Global authorization control is available. It can be defined using authorization master (global). </td>
        </tr>
<tr>
            <td>Non-Locking Actions</td>
            <td>The new RAP BO operation addition lock:none can be used to suppress the locking mechanism for an action. </td>
        </tr>
<tr>
            <td>Unmanaged Early Numbering in Managed BOs</td>
            <td>The entity behavior characteristic early numbering can be used to define unmanaged early numbering for all primary key fields of a managed RAP BO. </td>
        </tr>
<tr>
            <td>Implementing Cleanup in Managed BOs</td>
            <td>The new addition and cleanup is available for additional and unmanaged save in managed RAP BOs. It allows the application developer to implement the cleanup method. </td>
        </tr>
<tr>
            <td>Define Authorization Context</td>
            <td>It is now possible to define authorization contexts in a RAP behavior definition using the keyword define authorization context. There are different ways to activate an authorization context. When activated, all authorization objects listed in the respective context are always successful, that means, the respective authorization checks are skipped. </td>
        </tr>
<tr>
            <td>With Privileged Mode Disabling</td>
            <td>The new syntax with privileged mode disabling supersedes the deprecated version with privileged mode. The new syntax version disables an authorization contexts when accessing the RAP BO in question with a privileged EML call. </td>
        </tr>
<tr>
            <td>Projection BDEF, New Actions and Functions</td>
            <td>It is now possible to define and implement new actions and functions in a projection BDEF. </td>
        </tr>
<tr>
            <td>Projection BDEF, Authorization Concept</td>
            <td>It is now possible to define an authorization concept in a projection BDEF that controls access to the newly defined actions and functions in a projection BDEF. </td>
        </tr>
<tr>
            <td>Projection BDEF, Augmented Fields</td>
            <td>Field characteristics can be specified for augmented fields in a projection BDEF. </td>
        </tr>
<tr>
            <td>Projection BDEF, New Field Characteristic</td>
            <td>In projection BDEFs, a new field characteristic is available: field(suppress). It removes the field in question from the BDEF derived types and from all RAP APIs. </td>
        </tr>
<tr>
            <td>Projection BDEF, Operation Augment</td>
            <td>For projections BDEFs, the operation augment is available. Augmentation allows incoming requests with a custom implementation to be enhanced, for example with default values. </td>
        </tr>
<tr>
            <td>CDS Abstract Behavior Definitions</td>
            <td>A new implementation type is available: the CDS abstract behavior definition. Such abstract BDEFs mainly serve as typing mechanism for deep action or function parameters. </td>
        </tr>
<tr>
            <td>Abstract BDEF, with control</td>
            <td>The optional addition with control is available for abstract BDEFs. It adds a %control structure to the corresponding derived type structure. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_DOCU</td>
            <td>Configuration of the Documentation</td>
            <td>From release 7.56 on, important settings of the ABAP Keyword Documentation can be configured explicitly. Before, the documentation was configured implicitly from system settings. The configuration of the ABAP Keyword Documentation is saved in customizing table ABDOCCONFIG, that is maintained with transaction code ABAP_DOCU_CONFIG (based on executable program ABAP_DOCU_CONFIG) or with the configuration entity ABAP_DOCU_CONFIG_ENTITY. All repository objects related to the configuration are documented. The following properties can be set: CP If set to X, the ABAP Keyword Documentation uses terms appropriate for the SAP BTP ABAP environment (aka Steampunk) instead of Application Server ABAP in some footers and copyright texts. Recommendation: Set to X in systems of the SAP BTP ABAP environment. Set to initial in all other systems. ICF Enables or disables the Web Version of the ABAP Keyword Documentation based on ICF nodes /sap/public/bc/abap/docu and /sap/bc/abap/docu. The following settings can be applied: If set to X, the Web Version is enabled. The execution of example programs from the Web Version is disabled. If set to E, the Web Version is enabled. The execution of example programs from the Web Version is also enabled. Recommendation: Set to E in SAP development systems. Set to E in customer development systems. Set to X in SAP S/4HANA. Set to initial in SAP BTP ABAP environment and SAP S/4HANA Cloud. BATCH If set to X, the infrastructure of the ABAP Keyword Documentation is allowed to start batch jobs that prepare buffers that are needed for search in and display of the documentation. Recommendation: Set to X in SAP S/4HANA. Set to initial in SAP BTP ABAP environment. MAILBOX If set to X, the ABAP Keyword Documentation display offers a function to send a feedback mail to abap.docu@exchange.sap.corp. This mailbox is only available to SAP employees. Therefore, the setting makes sense for SAP's own development systems only. Recommendation: Set to X in SAP's own development systems. Set to initial in all other systems. VERSION Determines the ABAP language version for which the ABAP Keyword Documentation is displayed. Allowed values are defined in domain ABAPVRS. This setting can be overridden when calling the documentation using the respective APIs: CL_ABAP_DOCU for the SAP GUI version CL_ABAP_DOCU_EXTERNAL for the ADT or Web version. Calls to the documentation from ABAP Workbench and ADT set the language version depending on the object currently being edited. Calls to the documentation via the ABAPDOCU and ABAPHELP transactions use the version from ABDOCCONFIG by default. An initial value for VERSION in ABDOCCONFIG displays the documentation for Standard ABAP (X). Recommendation: Set to X in SAP S/4HANA Set to 5 in SAP BTP ABAP environment and SAP S/4HANA Cloud Program ABAP_DOCU_CONFIG allows a choice to be made between different sets of parameters: Parameter set typical for SAP development systems Parameter set typical for customer development systems Parameter set typical for SAP S/4HANA systems Parameter set typical for SAP S/4HANA Cloud ABAP environment Parameter set typical for SAP BTP ABAP environment Current parameter set The passed parameters are used as default values for a dialog window and can be overridden there. If customizing table ABDOCCONFIG is initial in a customer system, the first call of the ABAP Keyword documentation supplies it with values recommended for an SAP S/4HANA System. In SAP's own systems, values for a SAP development system are supplied. If the table is partly filled, default values are used for the missing rows. These values are initial for all properties except VERSION. If the language version is not passed by the caller and cannot be found in ABDOCCONFIG, the value X is used and a warning is shown in the documentation display. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_OBJECTS</td>
            <td>Dynamic Target</td>
            <td>The following syntax can be used for the object component selector -&gt; to access components and attributes dynamically now: ... { dref-&gt;(comp_name) }   | { cref-&gt;(attr_name) }   | { iref-&gt;(attr_name) } ... Before, this was possible for dynamic components and dynamic access in the statement ASSIGN only. </td>
        </tr>
<tr>
            <td rowspan="11">ABAP_SQL</td>
            <td>New String Function</td>
            <td>ABAP SQL now supports the new string function INITCAP. </td>
        </tr>
<tr>
            <td>New Date and Time Functions</td>
            <td>The following new generic date and time functions are available: SQL Function Date Time Time Stamp IS_VALID x x x EXTRACT_YEAR x - x EXTRACT_MONTH x - x EXTRACT_DAY x - x EXTRACT_HOUR - x x EXTRACT_MINUTE - x x EXTRACT_SECOND - x x DAYNAME x - x MONTHNAME x - x WEEKDAY x - x DAYS_BETWEEN x - x ADD_DAYS x - x ADD_MONTHS x - x </td>
        </tr>
<tr>
            <td>New Casts</td>
            <td>The following new casts are available: Source type Numeric target type Character-like target type Date/time field as target type CHAR, SSTRING, DATS, TIMS INT1, INT2, INT4, INT8, DEC, CURR, QUAN, DECFLOAT16, DECFLOAT34, FLTP - - FLTP INT1, INT2, INT4, INT8, DEC, CURR, QUAN, DECFLOAT16, DECFLOAT34 CHAR, SSTRING - DF16_DEC, DF34_DEC FLTP - - DATN - - DATS TIMN - - TIMS </td>
        </tr>
<tr>
            <td>New Set Operators</td>
            <td>ABAP SQL now supports the new set operators INTERSECT and EXCEPT. </td>
        </tr>
<tr>
            <td>New Function for Unit Conversion</td>
            <td>ABAP SQL now supports the new function UNIT_CONVERSION for unit conversions. </td>
        </tr>
<tr>
            <td>New Expression Null</td>
            <td>ABAP SQL now supports the new expression NULL. </td>
        </tr>
<tr>
            <td>Addition for the String Function REPLACE_REGEXPR</td>
            <td>The new parameter start can now be used in the function REPLACE_REGEXPR. Additionally, the parameter occurrence can now include expressions. </td>
        </tr>
<tr>
            <td>New String Function SUBSTRING_REGEXPR</td>
            <td>ABAP SQL now supports the new string function SUBSTRING_REGEXPR which supports regular expressions. </td>
        </tr>
<tr>
            <td>Byte Fields as Null Indicators</td>
            <td>The new addition INDICATORS ... NULL BITFIELD of the INTO clause of a SELECT statement allows a byte field type component to be specified as a null indicator. The single bits of the byte field serve for indicating null values in the result set of the query. For this purpose, condensed indicator structures can be declared with the addition AS BITFIELD of the TYPES statement. </td>
        </tr>
<tr>
            <td>Position of Null Indicators</td>
            <td>If CORRESPONDING FIELDS is used in the INTO clause of a SELECT statement, a null indicator defined by INDICATORS can be positioned anywhere in the target area. Otherwise, it must be the last component. </td>
        </tr>
<tr>
            <td>Strict Mode of the Syntax Check</td>
            <td>If one the new features listed above or one of the new CDS system table functions SERIES_GENERATE_ is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td rowspan="1">ABP</td>
            <td>ABAP Behavior Pools (ABP)</td>
            <td>ABAP Behavior Pools (ABP) are now available as part of the ABAP Keyword Documentation. The following topics are covered: CLASS, FOR BEHAVIOR OF RAP Handler Class METHODS, FOR RAP Saver Class finalize check_before_save adjust_numbers save cleanup cleanup_finalize save_modified </td>
        </tr>
<tr>
            <td rowspan="3">ASSIGNMENTS</td>
            <td>New Additions for MOVE-CORRESPONDING</td>
            <td>It is now possible to specify MOVE-CORRESPONDING statements with the following additions in the context of nested tables in deep structures. Both ensure that nested tables of the deep target structures are not deleted and new lines of nested tables in deep source structures are added: KEEPING TARGET LINES EXPANDING NESTED TABLES KEEPING TARGET LINES </td>
        </tr>
<tr>
            <td>New Additions for the Component Operator CORRESPONDING</td>
            <td>It is now possible to specify statements with the component operator CORRESPONDING with the following additions in the context of nested tables in deep structures. Both ensure that the nested tables of deep target structures are not deleted and new lines of nested tables in deep source structures are added: APPENDING BASE DEEP APPENDING BASE </td>
        </tr>
<tr>
            <td>New Parameter for CL_ABAP_CORRESPONDING=&gt;EXECUTE</td>
            <td>The method EXECUTE of the system class CL_ABAP_CORRESPONDING has a new parameter KEEPING_LINES. It has the same effect as the additions KEEPING TARGET LINES in MOVE-CORRESPONDING or BASE in CORRESPONDING. </td>
        </tr>
<tr>
            <td rowspan="1">BYTE</td>
            <td>Writable Expression in SET BIT</td>
            <td>For the operand byte_string of statement SET BIT, a writable expression can be specified now. </td>
        </tr>
<tr>
            <td rowspan="2">DDIC</td>
            <td>Extension for Load Unit</td>
            <td>In ABAP Dictionary, the load unit has been extended. There are now the following settings available: Column Preferred Page Preferred Column Enforced Page Enforced </td>
        </tr>
<tr>
            <td>C0 Developer Extensibility for DDIC Objects</td>
            <td>The following new extensibility annotations are available for dictionary objects. They are required for C0 release of a DDIC object, which is a prerequisite for developer extensibility. @AbapCatalog.enhancement.fieldSuffix @AbapCatalog.enhancement.quotaMaximumFields @AbapCatalog.enhancement.quotaMaximumBytes @AbapCatalog.enhancement.quotaShareCustomer @AbapCatalog.enhancement.quotaSharePartner </td>
        </tr>
<tr>
            <td rowspan="1">EML</td>
            <td>Entity Manipulation Language</td>
            <td>ABAP EML keywords are now available as part of the ABAP Keyword Documentation. The following keywords and topics are covered among others: MODIFY ... MODIFY ENTITY MODIFY ENTITIES MODIFY ENTITIES OPERATIONS READ ... READ ENTITY READ ENTITIES READ ENTITIES OPERATIONS COMMIT ENTITIES ... COMMIT ENTITIES COMMIT ENTITIES RESPONSE OF COMMIT ENTITIES RESPONSES OF COMMIT ENTITIES BEGIN, END including CONVERT KEY OF ROLLBACK ENTITIES GET PERMISSIONS ... GET PERMISSIONS GET PERMISSIONS OF GET PERMISSIONS OPERATIONS SET LOCKS ... SET LOCKS ENTITY SET LOCKS OF SET LOCKS (dynamic form) Type Mapping for RAP SET NAMES SET FLAGS RAP-specific variants of the CORRESPONDING operator ABAP EML for providing RAP BOs IN LOCAL MODE MODIFY AUGMENTING ENTITY </td>
        </tr>
<tr>
            <td rowspan="2">ITAB</td>
            <td>Access to Generically Typed Internal Tables</td>
            <td>Earlier, in statements for accessing internal tables, the internal table had to be known statically. The operand had to be typed at least with any table. Now, this restriction is partly removed. In the statements INSERT, APPEND, COLLECT, MODIFY, DELETE, READ, LOOP, and SORT, operands can be field symbols and formal parameters that are typed fully generically with TYPE data or TYPE any. Such operands can be used as if typed with any table. If an index access is involved, operands are still required that are typed at least with TYPE index_table. Hint This is not yet possible in expressions as FOR expressions or table expressions. Example The following was not possible in older releases. DATA itab TYPE TABLE OF scarr. FIELD-SYMBOLS &lt;itab&gt; TYPE ANY. ASSIGN itab TO &lt;itab&gt;. LOOP AT &lt;itab&gt; ASSIGNING FIELD-SYMBOL(&lt;fs&gt;).   ... ENDLOOP. </td>
        </tr>
<tr>
            <td>Alias Names for Secondary Keys</td>
            <td>Alias names can now be declared for secondary keys of internal tables by using the addition ALIAS of TYPES and DATA. This can be helpful when changing existing secondary keys without invalidating users. </td>
        </tr>
<tr>
            <td rowspan="1">REFERENCES</td>
            <td>Dereferencing Data References</td>
            <td>The dereferencing operator-&gt;* can be used for generically typed data reference variables in almost all operand positions now. Before, that was possible in the ASSIGN statement only. </td>
        </tr>
<tr>
            <td rowspan="2">STRINGS</td>
            <td>XPath and XSD Regular Expressions</td>
            <td>Besides the existing support of PCRE regular expressions and POSIX regular expressions (obsolete) ABAP supports now also XPath regular expressions and XSD regular expressions. Internally, those are transformed to PCRE regular expressions and processed by the PCRE2 Library. Both kinds of regular expressions can be used by the new (factory) methods CREATE_XPATH2 and CREATE_XSD of the system classes CL_ABAP_REGEX and CL_ABAP_MATCHER. XPath regular expressions can be used by the new parameter xpath in some built-in functions. </td>
        </tr>
<tr>
            <td>Callouts in PCRE Regular Expressions</td>
            <td>The class CL_ABAP_MATCHER supports callouts in PCRE syntax now. The method SET_CALLOUT can be used to register a handler class that implements the interface IF_ABAP_MATCHER_CALLOUT. The special characters (?C...) of a PCRE regular expression then call the interface method CALLOUT when the method MATCH is executed. </td>
        </tr>
<tr>
            <td rowspan="1">SYSTEM_CLASSES</td>
            <td>New Method for System Class CL_ABAP_BEHV_AUX</td>
            <td>The system class CL_ABAP_BEHV_AUX has a new method available: RAP runtime context information. Using the GET_CURRENT_CONTEXT method, you get context information about the current RAP handler and RAP saver method. </td>
        </tr>
<tr>
            <td rowspan="2">TYPES</td>
            <td>Condensed Indicator Structures</td>
            <td>The new addition AS BITFIELD behind addition INDICATORS of the statement TYPES allows a condensed indicator structure to be defined as a byte field type component of a given structured type. An condensed indicator structure can be used as a null indicator in ABAP SQL statements. </td>
        </tr>
<tr>
            <td>BDEF Derived Types</td>
            <td>BDEF derived types are now available as part of the ABAP keyword documentation. The following keywords and topics are covered: TYPE TABLE FOR TYPE STRUCTURE FOR TYPE RESPONSE FOR TYPE REQUEST FOR Components of BDEF Derived Types Declaration of Variables with BDEF Derived Types </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 755</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="22">ABAP_CDS</td>
            <td>Defining Associations in CDS Projection Views</td>
            <td>It is now possible to define new associations to external data sources in CDS projection views. </td>
        </tr>
<tr>
            <td>CDS View Entities</td>
            <td>A new kind of CDS view is available: the CDS view entity. CDS view entities represent an improved version of CDS DDIC-based views (obsolete) (DEFINE VIEW). They serve the same purpose and have the same structure as CDS DDIC-based views (obsolete), but offer many advantages. CDS view entities are planned to replace CDS DDIC-based views (obsolete) in the future. A CDS view entity is defined with the statement DEFINE VIEW ENTITY. </td>
        </tr>
<tr>
            <td>CDS View Entity Extensions</td>
            <td>The new statement EXTEND VIEW ENTITY of the DDL of ABAP CDS makes it possible to add new view fields to existing CDS views entities and CDS projection views by using CDS view entity extensions. </td>
        </tr>
<tr>
            <td>New Session Variables for User Time Zone and User Date</td>
            <td>Two new session variables are available in ABAP CDS: $session.user_timezone, which returns the time zone defined in the user master record for the local user time. $session.user_date, which returns the local date of a user. </td>
        </tr>
<tr>
            <td>New String Function for Regular Expressions</td>
            <td>ABAP CDS now supports the new string function REPLACE_REGEXPR that allows regular expressions to be replaced. </td>
        </tr>
<tr>
            <td>Check with element IS [NOT] INITIAL in a Literal Condition</td>
            <td>In a literal condition, IS [NOT] INITIAL can now be used to check whether the value of the left side matches (does not match) the initial value of the ABAP data type that matches the element. </td>
        </tr>
<tr>
            <td>Addition bypass when</td>
            <td>The addition BYPASS WHEN can now be used to specify a bypass condition for an element. If the condition is met, the element in question is not used for authorization filtering. </td>
        </tr>
<tr>
            <td>Check on the User Name in User Conditions</td>
            <td>When the user name is checked in user conditions, the following can now be checked instead of the user name: The value of the alias. The number of the business partner assigned to the user. </td>
        </tr>
<tr>
            <td>Data Types</td>
            <td>The operand that can be specified on the left side of a condition of an access rule of a CDS role can now have the built-in ABAP Dictionary data type RAW. </td>
        </tr>
<tr>
            <td>Aspect Bypass Conditions</td>
            <td>IS [NOT] INITIAL and IS INITIAL OR NULL are now possible as aspect bypass conditions. </td>
        </tr>
<tr>
            <td>Quantifier Operators</td>
            <td>With the new addition ALL, access conditions can express that access shall only be granted when from a set-valued association all values satisfy the condition. WHERE ALL toItem.State = 'A' To accompany this use case, the BYPASS WHEN operator has been extended to literal conditions also. A dedicated operator EXISTS can be used when different access conditions using the same set-valued association shall not be coupled in their fields by means of a common join expression.  </td>
        </tr>
<tr>
            <td>Type Mapping</td>
            <td>The new statement mapping for can be used to map field names from legacy code to field names from the current data model. Available for unmanaged and managed business objects. </td>
        </tr>
<tr>
            <td>Additional Save in Managed BOs</td>
            <td>The new statement with additional save can be used to enhance the default save sequence in a managed implementation scenario. </td>
        </tr>
<tr>
            <td>Unmanaged Save in Managed BOs</td>
            <td>The new statement with unmanaged save can be used to implement an own saving strategy in a managed implementation scenario. </td>
        </tr>
<tr>
            <td>Implementation Grouping</td>
            <td>The new statement group can be used to divide the implementation-relevant parts of a BO's business logic into several groups for behavior implementation. </td>
        </tr>
<tr>
            <td>Managed Internal Numbering for Managed RAP BOs</td>
            <td>The new statement numbering:managed can be used to automatically generate values for primary key fields with a UUID. Available for managed implementation scenarios. </td>
        </tr>
<tr>
            <td>New Options for Output Parameters</td>
            <td>For actions and functions, the output parameter can now be an entity or a structure. The addition selective can be used for an output parameter to return only parts of the result structure. </td>
        </tr>
<tr>
            <td>Unmanaged Lock in Managed RAP BOs</td>
            <td>The new statement lock master unmanaged can be used if the application developer wants to implement an own locking mechanism in a managed implementation scenario. An own locking mechanism can be used instead of the RAP locking mechanism provided by the RAP framework. </td>
        </tr>
<tr>
            <td>Draft Support for RAP BOs</td>
            <td>The new statement with draft can be used to enable the draft concept for a RAP BO. </td>
        </tr>
<tr>
            <td>Determine Actions</td>
            <td>Determine actions are a new type of action defined using determine action. With a determine action, determinations and validations can be executed on request. </td>
        </tr>
<tr>
            <td>Precheck for Modify Operations</td>
            <td>The new RAP BO operation addition precheck can be used to prevent illegal changes from reaching the application buffer by prechecking modify operations. </td>
        </tr>
<tr>
            <td>New Field Characteristics</td>
            <td>RAP BDL now supports the following new field characteristics: mandatory:create readonly:update </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SESSIONS</td>
            <td>Number of ABAP Sessions</td>
            <td>The default value of profile parameter rdisp/max_alt_modes, that determines the possible number of ABAP sessions per user session, was enhanced from 6 to 16 and is now the same as the maximum number of ABAP sessions per user session. </td>
        </tr>
<tr>
            <td rowspan="23">ABAP_SQL</td>
            <td>New Aggregate Function ALLOW_PRECISION_LOSS</td>
            <td>ABAP SQL now supports the following new aggregate function in combination with the aggregate expression SUM: ALLOW_PRECISION_LOSS </td>
        </tr>
<tr>
            <td>Optional Window Frame Specification within a Window Function</td>
            <td>The optional window frame specification defines a subset of rows within a window, called frame. Frames are determined with respect to the current row, which enables the frame to move within a window. </td>
        </tr>
<tr>
            <td>New Window Functions FIRST_VALUE and LAST_VALUE</td>
            <td>ABAP SQL now supports the following new window functions in window expressions: FIRST_VALUE and LAST_VALUE. </td>
        </tr>
<tr>
            <td>New Date/Time Conversion Functions</td>
            <td>ABAP SQL now supports the following new date/time conversion functions: TSTMPL_TO_UTCL and TSTMPL_FROM_UTCL DATS_TO_DATN and DATS_FROM_DATN TIMS_TO_TIMN and TIMS_FROM_TIMN </td>
        </tr>
<tr>
            <td>New Time Stamp Functions</td>
            <td>ABAP SQL now supports the following new time stamp functions: UTCL_CURRENT, UTCL_ADD_SECONDS, and UTCL_SECONDS_BETWEEN. </td>
        </tr>
<tr>
            <td>New Date Functions</td>
            <td>ABAP SQL now supports the following new date functions: DATN_DAYS_BETWEEN, DATN_ADD_DAYS, and DATN_ADD_MONTHS. </td>
        </tr>
<tr>
            <td>New Additions After the ORDER BY Clause</td>
            <td>ABAP SQL now supports the following additions after the ORDER BY clause: NULLS FIRST and NULLS LAST. </td>
        </tr>
<tr>
            <td>New Aggregate Functions</td>
            <td>ABAP SQL now supports the following new aggregate functions: MEDIAN, STDDEV, VAR, CORR, and CORR_SPEARMAN. </td>
        </tr>
<tr>
            <td>New Geometry Conversion Function as_geo_json</td>
            <td>ABAP SQL now supports the following new geometry conversion function: as_geo_json </td>
        </tr>
<tr>
            <td>SQL Conditions Revised</td>
            <td>The following ABAP SQL conditions were revised: The operator IN can now be used with a subquery that returns value tuples. SQL functions and cast expressions can now be used as operands on the right side of comparison operators. </td>
        </tr>
<tr>
            <td>New Window Function NTILE</td>
            <td>ABAP SQL now supports the following new window function in window expressions: NTILE </td>
        </tr>
<tr>
            <td>SELECT, INTO target Modification</td>
            <td>When using SELECT, INTO target, host variables can now be declared inline even when the FROM clause is dynamic, as long as all fields of the SELECT list are known statically. Previously, the structure of the result set, including the SELECT list, the FROM clause, and any indicators needed to be static. </td>
        </tr>
<tr>
            <td>New Type Conversion Function to_blob</td>
            <td>See New Type Conversion Function to_clob. </td>
        </tr>
<tr>
            <td>New Type Conversion Function to_clob</td>
            <td>ABAP SQL now supports the new type conversion functions to_clob and to_blob. </td>
        </tr>
<tr>
            <td>New Currency Conversion Function currency conversion</td>
            <td>ABAP SQL now supports the new currency conversion function currency_conversion. </td>
        </tr>
<tr>
            <td>Streaming and Locators Can Now Be Used on SQL Expressions</td>
            <td>Streaming and locators can now be used in combination with SQL expressions such as TO_CLOB, TO_BLOB, and AS_GEO_JSON. </td>
        </tr>
<tr>
            <td>Further Data Types Allowed in Elementary SQL Expressions</td>
            <td>Elementary SQL expressions can now also have the dictionary data types STRING and RAWSTRING. </td>
        </tr>
<tr>
            <td>Hierarchy Load Options</td>
            <td>The hierarchy generator HIERARCHY can now use the new addition LOAD BULK|INCREMENTAL|load_option to specify the load policy for a generated hierarchy. </td>
        </tr>
<tr>
            <td>Typed Literals</td>
            <td>Typed literals for many ABAP Dictionary types are now available in ABAP SQL. </td>
        </tr>
<tr>
            <td>New String Functions</td>
            <td>ABAP SQL now supports the new string functions REPLACE_REGEXPR, LIKE_REGEXPR, and OCCURRENCES_REGEXPR that support regular expressions. </td>
        </tr>
<tr>
            <td>Addition to the UPDATE FROM Clause</td>
            <td>Set indicators can now be used as additions after the UPDATE FROM clause to indicate columns to be updated. </td>
        </tr>
<tr>
            <td>Change Regarding the CONNECTION Addition</td>
            <td>The addition CONNECTION is supported by the ABAP SQL in-memory engine for the standard connection as well as for service connections. Only secondary connections from table DBCON cannot be processed by the ABAP SQL in-memory engine. </td>
        </tr>
<tr>
            <td>Strict Mode of the Syntax Check</td>
            <td>If one the new features listed above is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td rowspan="2">DDIC</td>
            <td>Load Unit</td>
            <td>In ABAP Dictionary, a new technical setting for database tables is available: the load unit. It specifies how the data of the table is loaded into the main memory of the SAP HANA database. It can be used to reduce the memory consumption in the HANA database server. </td>
        </tr>
<tr>
            <td>Internal Handling of the Name Table</td>
            <td>The internal handling of the name table (nametab) that stores the runtime objects of data types in the ABAP Dictionary has changed. This has the following consequences: The internal ABAP statement EXPORT NAMETAB cannot be used any more and leads to a runtime error. The internal ABAP statement IMPORT NAMETAB is still partly supported for reasons of downward compatibility. Any access to entries for DDIC table types leads to a runtime error. Any other access leads to errors from ATC. The former native database table DDNTF for separate nametab field descriptions is not supported any more and will be deleted. </td>
        </tr>
<tr>
            <td rowspan="1">EXCEPTIONS</td>
            <td>Declaration of CX_NO_CHECK Exceptions</td>
            <td>Exceptions of category CX_NO_CHECK are always declared implicitly in interfaces of procedures and can always be propagated. Now it is also possible to declare exceptions of category CX_NO_CHECK with RAISING in procedure interfaces, for example for methods. This allows it to document the possible occurrence of such exceptions and to change the category of existing exceptions into CX_NO_CHECK without leading to syntax errors in procedure interfaces. </td>
        </tr>
<tr>
            <td rowspan="1">EXPRESSIONS</td>
            <td>Calculation Assignments in Constructor Operator REDUCE</td>
            <td>In the assignments behind the addition NEXT of the constructor operator REDUCE the calculation assignment operators +=, +=, *=, /= or &amp;&amp;=, can be used now and the respective rules apply. </td>
        </tr>
<tr>
            <td rowspan="1">LISTS</td>
            <td>Tooltips for Input Ready Fields</td>
            <td>If the profile parameter dynp/enhanced_system_feedback has the value TRUE, the addition QUICKINFO is no longer ignored for checkboxes on classic lists in SAP GUI for Windows. </td>
        </tr>
<tr>
            <td rowspan="5">STRINGS</td>
            <td>Support of Perl Compatible Regular Expressions</td>
            <td>Besides the existing support of POSIX regular expressions, ABAP supports now also PCRE regular expressions that are processed by the PCRE2 Library implemented in the ABAP Kernel. PCRE regular expressions can be used in the same way as POSIX regular expressions. The distinction is made: By the new addition PCRE that can be used instead of REGEX in the statements FIND and REPLACE. By the new parameter pcre that can be used instead of regex in built-in functions. By new (factory) methods of the system classes CL_ABAP_REGEX and CL_ABAP_MATCHER. PCRE regular expressions are more powerful and have better performance than POSIX regular expressions. For more information, see Regular Expressions. </td>
        </tr>
<tr>
            <td>Verbatim Replacements</td>
            <td>The new addition VERBATIM of the REPLACE statement causes all characters of the replacement string to be taken literally. With that addition, special characters for regular expression replacement patterns have no special meaning. </td>
        </tr>
<tr>
            <td>New Catchable Exception CX_SY_STRING_SIZE_TOO_LARGE</td>
            <td>See Exception CX_SY_STRING_SIZE_TOO_LARGE in Transformations. </td>
        </tr>
<tr>
            <td>Exception CX_SY_STRING_SIZE_TOO_LARGE in Transformations</td>
            <td>The exception that occurs when an operation with a string exceeds its maximum size is now connected to the exception class CX_SY_STRING_SIZE_TOO_LARGE and can be handled. Previously, it always resulted in runtime error STRING_SIZE_TOO_LARGE. This exception can also be handled for the statement CALL TRANSFORMATION if some conditions are met. </td>
        </tr>
<tr>
            <td>Formatting Option CURRENCY for Decimal Floating Point Numbers</td>
            <td>The addition CURRENCY of the WRITE TO and WRITE statements formatting option CURRENCY in string templates can now also be applied to decimal floating point numbers. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>Indicator Structures</td>
            <td>The new addition INDICATORS of the statement TYPES defines an indicator structure as a substructure of a given structured type. An indicator structure can be used as a ABAP SQL indicator in ABAP SQL read and write statements. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 754</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="26">ABAP_CDS</td>
            <td>Temporal Hierarchies</td>
            <td>The new addition PERIOD of the statement DEFINE HIERARCHY can now be used to create temporal hierarchies in which the hierarchy nodes are limited by time intervals. </td>
        </tr>
<tr>
            <td>Annotation for Database Hints</td>
            <td>The framework-specific annotation @Consumption.dbHints replaces the ABAP annotation @AbapCatalog.dbHints and makes it obsolete. The ABAP annotation is evaluated by frameworks such as SADL and not by the ABAP runtime environment. </td>
        </tr>
<tr>
            <td>Annotations for Releasing Elements</td>
            <td>The new element annotations @API.element.releaseState @API.element.successor can be used to override releases of the individual elements and successors can be specified for forbidden elements. </td>
        </tr>
<tr>
            <td>CDS Projection Views</td>
            <td>A CDS projection view is a direct projection of the underlying CDS view and exposes only a subset of elements of the projected entity. A CDS projection view is defined using DEFINE VIEW ENTITY AS PROJECTION ON in a CDS data definition. </td>
        </tr>
<tr>
            <td>New Date Functions</td>
            <td>ABAP CDS now supports the following new date functions: DATN_DAYS_BETWEEN DATN_ADD_DAYS DATN_ADD_MONTHS </td>
        </tr>
<tr>
            <td>New Time Stamp Functions</td>
            <td>ABAP CDS now supports the following new time stamp functions: UTCL_CURRENT UTCL_ADD_SECONDS UTCL_SECONDS_BETWEEN </td>
        </tr>
<tr>
            <td>New Date/Time Conversion Functions</td>
            <td>ABAP CDS now supports the following new date/time conversion functions: TSTMPL_TO_UTCL and TSTMPL_FROM_UTCL DATS_TO_DATN and DATS_FROM_DATN TIMS_TO_TIMN and TIMS_FROM_TIMN </td>
        </tr>
<tr>
            <td>Hierarchy Load Options</td>
            <td>The hierarchy generator DEFINE HIERARCHY can now use the new addition LOAD BULK|INCREMENTAL|load_option to specify the load policy for a generated hierarchy. </td>
        </tr>
<tr>
            <td>CDS Custom Entities</td>
            <td>A new type of CDS entity is available: the CDS custom entity. CDS custom entities are used in the RAP framework to implement ABAP queries in CDS. </td>
        </tr>
<tr>
            <td>Handling of Annotation Values</td>
            <td>A change in the handling of annotation values has been introduced with the following consequences: Annotations that require an enumeration symbol as annotation value no longer accept string values. They only accept enumeration symbols. Example: Until release 7.54, the following was accepted: @AccessControl.authorizationCheck: '#CHECK'. From release 7.54, this is no longer accepted. The quotation marks must be removed. If # is the first or the only character of EndUserText.label or EndUserText.quickInfo, then it is not removed from the unescaped value any more. These changes are slightly incompatible. </td>
        </tr>
<tr>
            <td>Root Nodes and Compositions</td>
            <td>The new addition ROOT is available for CDS entities to mark an entity as a CDS root entity. In addition, CDS associations can be declared as COMPOSITION or TO PARENT. In this way, a CDS composition tree can be modeled for use in the ABAP RESTful Application Programming Model. </td>
        </tr>
<tr>
            <td>Addition COMBINATION MODE OR|AND of the Statement GRANT SELECT ON</td>
            <td>The optional addition COMBINATION MODE can now be used to define how the access conditions of multiple CDS access rules for the same CDS entity are combined. </td>
        </tr>
<tr>
            <td>Addition REDEFINITION of the Statement GRANT SELECT ON</td>
            <td>The addition REDEFINITION is used to indicate that the specified CDS access rule is the only existing access rule and any other access rules are ignored. </td>
        </tr>
<tr>
            <td>Addition IN SCENARIO of the Statement GRANT SELECT ON</td>
            <td>The optional addition IN SCENARIO can be used to apply the switchable authorization check to an authorization object. </td>
        </tr>
<tr>
            <td>Generic Aspect Condition of the Statement DEFINE ROLE</td>
            <td>An aspect condition can now be used to specify the generic aspect defined in an aspect definition in the right side introduced by ASPECT. </td>
        </tr>
<tr>
            <td>New Variant INHERITING CONDITIONS FROM SUPER</td>
            <td>There is now another variant of the inheritance condition in an access rule of the statement DEFINE ROLE, which applies the access conditions from SUPER. </td>
        </tr>
<tr>
            <td>Different REPLACING Operators</td>
            <td>The optional addition REPLACING of INHERITING CONDITIONS can be used to modify the inherited conditions. </td>
        </tr>
<tr>
            <td>REPLACING Operator ELEMENT ... WITH</td>
            <td>Any number of ELEMENT operators can be used to transform conditions that use fields of the inherited entity to a different field of the inheriting entity. </td>
        </tr>
<tr>
            <td>Definition of a Generic Aspect</td>
            <td>A generic aspect can now be defined as part of a CDS access policy itself defined using DEFINE ACCESSPOLICY. </td>
        </tr>
<tr>
            <td>Access Control for ABAP CDS Hierarchies</td>
            <td>ABAP CDS hierarchies can now be protected using access control with some restrictions. </td>
        </tr>
<tr>
            <td>DCL Restrictions for CDS Transactional Queries</td>
            <td>Transactional queries apply the access control of the underlying CDS entity with restrictions. </td>
        </tr>
<tr>
            <td>VOID</td>
            <td>The new addition VOID can be specified in an access condition. It defines that the access condition in question is ignored. </td>
        </tr>
<tr>
            <td>CONSTRAINT ID</td>
            <td>The new addition CONSTRAINT ID can be specified in a PFCG mapping as part of a CDS access policy. It defines further restrictions for authorization fields of authorization objects in a CDS access policy. </td>
        </tr>
<tr>
            <td>Managed RAP BO</td>
            <td>The new statement managed can be used to create managed RAP BOs within the framework of the ABAP RESTful Application Programming Model. This scenario is intended for greenfield developments that are developed from scratch. Standard operations and services are provided by the RAP framework. </td>
        </tr>
<tr>
            <td>Business Object Projection</td>
            <td>The new statement projection can be used to create projections of a business object. This projects and aliases a subset of a business object for a specific business service within the framework of the ABAP RESTful Application Programming Model. </td>
        </tr>
<tr>
            <td>CDS Service Definitions</td>
            <td>The new statement DEFINE SERVICE is available for defining CDS service definitions. </td>
        </tr>
<tr>
            <td rowspan="16">ABAP_SQL</td>
            <td>Extensions of the INTO Clause</td>
            <td>The INTO clause has been extended as follows: The new addition NEW can be used to implicitly create anonymous data objects as target areas. The addition NEW now also makes inline declarations possible when using dynamic tokens and after the statement FETCH. The new addition INDICATORS can be used to specify a null indicator. </td>
        </tr>
<tr>
            <td>Definition of Associations</td>
            <td>When associations of a common table expression are published using the addition WITH ASSOCIATIONS, new CTE associations can be defined by specifying JOIN TO ONE|MANY. These CTE associations can be used in the subsequent queries of the current WITH statement, either in path expressions or as hierarchy associations in the hierarchy generator HIERARCHY. Definitions of associations require strict mode from ABAP release 7.74. </td>
        </tr>
<tr>
            <td>New Aggregate Function STRING_AGG</td>
            <td>The new aggregate function STRING_AGG can be used to chain character-like results of the rows of the results set of a query or of the current group as a string. </td>
        </tr>
<tr>
            <td>Addition DISTINCT Optional in Aggregate Function COUNT</td>
            <td>The aggregate function COUNT( sql_exp ) can now be used without the addition DISTINCT. In this case, it counts all rows in which the value of the argument is not the null value. </td>
        </tr>
<tr>
            <td>Window Expressions</td>
            <td>Window expressions defined using OVER can now be used in the SELECT list of a query. Window expressions define windows as a subset of the results set and apply window functions to them. </td>
        </tr>
<tr>
            <td>Temporal SQL Hierarchies</td>
            <td>The hierarchy generator HIERARCHY can now use the new addition PERIOD FROM TO VALID FROM TO to create temporal hierarchies in which the hierarchy nodes are limited by time intervals. </td>
        </tr>
<tr>
            <td>Hierarchy Navigators</td>
            <td>The new hierarchy navigator HIERARCHY_DESCENDANTS_AGGREGATE can be used to calculate aggregate functions for descendant nodes. </td>
        </tr>
<tr>
            <td>Aggregate Expressions in SQL Expressions</td>
            <td>From ABAP release 7.54, aggregate expressions can be specified as operands of SQL expressions. </td>
        </tr>
<tr>
            <td>Extension of the CAST Matrix</td>
            <td>The matrix of types that can be converted to each other with a CAST expression has been expanded. In particular, the new data types of the ABAP Dictionary are taken into account. </td>
        </tr>
<tr>
            <td>Extension of Arithmetic Expressions</td>
            <td>In arithmetic expressions, decimal floating point expressions with operators of types DECFLOAT16 or DECFLOAT34 are possible now. </td>
        </tr>
<tr>
            <td>Syntax Check for Literals and Host Constants</td>
            <td>The fact that conversions of host variables in read positions need to be lossless is checked for untyped literals and host constants in the strict syntax check modes from ABAP release 7.62 and ABAP release 7.63 and hence can produce syntax errors. From ABAP release 7.54, a syntax check warning is produced when this rule is broken outside of the strict mode too. </td>
        </tr>
<tr>
            <td>SQL Conditions</td>
            <td>The SQL conditions were revised as follows: Unlike in all other relational expressions in ABAP SQL, the relational expression IS [NOT] NULL can now be used to check LOBs and geodata types. A new variant of the operator IN can be used to compare multiple operands with a list of value tuples. </td>
        </tr>
<tr>
            <td>New Window Functions</td>
            <td>ABAP SQL now supports the following new window functions in window expressions: LEAD and LAG. </td>
        </tr>
<tr>
            <td>New Built-In Functions</td>
            <td>ABAP SQL Now supports the following new built-in functions: UUID </td>
        </tr>
<tr>
            <td>Client Handling</td>
            <td>The new additions USING [ALL] CLIENTS [IN] in queries USING [ALL] CLIENTS [IN] in write statements make it possible to switch implicit client handling from the current default client to multiple clients. This makes the addition CLIENT SPECIFIED obsolete in queries and obsolete in the write statements UPDATE SET and DELETE FROM. </td>
        </tr>
<tr>
            <td>Strict Mode in the Syntax Check</td>
            <td>If one the new features listed above is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_UNIT</td>
            <td>Using Test Classes</td>
            <td>Test classes and their components cannot be addressed in the production code of programs and must be addressed only in other test classes. A static check and a full check at runtime are now applied to verify this. Until now it was possible to address certain components of test class in production code. In production systems, this required the profile parameter abap/test_generation to be set accordingly. </td>
        </tr>
<tr>
            <td rowspan="2">AMDP</td>
            <td>Specifying CDS Entities After USING</td>
            <td>In the implementation of the AMDP method, the name of the CDS entity can now be specified after the USING addition for CDS views, CDS table functions, and CDS hierarchies. The name of the CDS managed DDIC view of a CDS view or of the AMDP function of a CDS table function can still be specified, but is best replaced by specifying CDS entities. If a CDS entity is specified, all other database objects of CDS entities must be also be specified using this entity. </td>
        </tr>
<tr>
            <td>Quotation Marks for Logical Schemas</td>
            <td>If the predefined AMDP macro $ABAP.schema is specified for a logical schema, the optional addition quote can now be used to define which quotation marks are used when the macro is replaced. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>Calculation Assignments</td>
            <td>A calculation assignment is an assignment with an addition assignment operator (+=) subtraction assignment operator (-=) multiplication assignment operator (*=) division assignment operator (/=) concatenation assignment operator (&amp;&amp;=). A calculation takes place when the assignment is made. These new operators make the statements ADD, SUBTRACT, MULTIPLY, and DIVIDE obsolete. </td>
        </tr>
<tr>
            <td rowspan="3">DDIC</td>
            <td>New Built-In Data Types</td>
            <td>The following new built-in data types were introduced in ABAP Dictionary: Decimal floating point numbers DECFLOAT16 DECFLOAT34 Date fields, time fields, and time stamp fields DATN TIMN UTCLONG Geodata types GEOM_EWKB These data types are currently only supported by SAP HANA databases. These types can be mapped to HANA-specific data types but not to other vendor-specific data types. ABAP-managed database objects where these types are used can only be created on SAP HANA databases. </td>
        </tr>
<tr>
            <td>Flagging of Obsolete Data in Check Tables</td>
            <td>A special column can be used to flag obsolete data in check tables. This flag modifies input checks and input help in dynpros and Web Dynpros. This flag was ported back to all releases including ABAP release 7.40. SAP-internal hint See also SAP Note 2708906. </td>
        </tr>
<tr>
            <td>Dynamic Cached Views</td>
            <td>Dynamic cached views are a kind of HANA tuning object defined using the statement DEFINE DYNAMIC CACHE. A dynamic cache is a DDIC integration of an SAP HANA Dynamic Result Cache. </td>
        </tr>
<tr>
            <td rowspan="1">MESSAGES</td>
            <td>Implicit Message Type in IF_T100_DYN_MSG</td>
            <td>If the object reference variable oref in the variant MESSAGE oref of the statement MESSAGE for sending a message points to an object that includes the system interface IF_T100_DYN_MSG, the addition TYPE can be omitted and the message type from the interface attribute MSGTY of the object is used implicitly. Until now, however, the statement MESSAGE oref could only have the further additions RAISING and DISPLAY LIKE if TYPE was specified explicitly. These additions are now also possible if TYPE is not specified. </td>
        </tr>
<tr>
            <td rowspan="1">MISC</td>
            <td>Authorization Checks in Updates</td>
            <td>No authorization checks must be made in updates. This rule was not always applied before ABAP release 7.54: The statement AUTHORITY-CHECK made no authorization checks in update sessions but always set sy-subrc to 0 here. The statement AUTHORITY-CHECK made an authorization check In local updates, When using ABAP SQL to access a CDS entity for which a CDS role is defined, implicit CDS access control was applied by default in update sessions and in local updates. From ABAP release 7.54, the following applies to update sessions and to local updates: In updates, the statement AUTHORITY-CHECK does not make any authorization checks but sets the value sy-subrc always to 0. In updates, CDS access control is not allowed. If the ABAP SQL statement SELECT is used during an update to access a CDS entity for which access control is not disabled, the runtime error SYSTEM_UPDATE_TASK_ILL_STMT occurs. Access control can be disabled as follows: In the CDS entity: Using the value #NOT_ALLOWED for the annotation @AccessControl.authorizationCheck. In ABAP SQL: Using the addition WITH PRIVILEGED ACCESS in the FROM clause. </td>
        </tr>
<tr>
            <td rowspan="1">STRINGS</td>
            <td>Decimal Places in Time Stamps</td>
            <td>In embedded expressions of string templates and for the statements WRITE [TO], the formatting option DECIMALS can now be combined with TIMESTAMP and TIME ZONE to define the number of decimal places in UTC time stamps in packed numbers. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>New Built-In ABAP Type utclong</td>
            <td>The new built-in time stamp type utclong makes it possible to declare time stamp fields for UTC time stamps according to the POSIX standard. The new data type is taken into account in all relevant positions. Important points are: The associated data type in ABAP Dictionary was introduced with the name UTCLONG. The value range of the new data type are time stamps between 0001-01-01T00:00:00.0000000 and 9999-12-31T23:59:59.9999999 plus a special initial value. In assignments and comparisons, the data type utclong can only be converted to the types c and string, and vice versa. In assignments, any attempt to perform another conversion raises an exception of the new class CX_SY_CONVERSION_NOT_SUPPORTED. Time stamp fields are formatted specially in string templates or using the statement WRITE TO. There is a mapping rule for asXML. Time stamp fields must meet an alignment requirement. Their memory address must be divisible by eight. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 753</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="8">ABAP_CDS</td>
            <td>CDS Hierarchies</td>
            <td>The new statement DEFINE HIERARCHY can be used to create CDS hierarchies that are accessed as hierarchies in ABAP SQL read statements. </td>
        </tr>
<tr>
            <td>New Condition IS INITIAL</td>
            <td>The new condition IS INITIAL can be used to check the initial value of operands. </td>
        </tr>
<tr>
            <td>Literals</td>
            <td>A literal of a respective value is now always handled as a field of the type INT8, including in the definition of associations. </td>
        </tr>
<tr>
            <td>CDS Abstract Entities</td>
            <td>An CDS abstract entity defines the type properties of a CDS entity without creating an instance of a database object. An abstract CDS entity is defined using DEFINE ABSTRACT ENTITY in a CDS data definition. </td>
        </tr>
<tr>
            <td>Metadata Extensions for CDS Entities</td>
            <td>The variant ANNOTATE ENTITY was added to the previous statement ANNOTATE VIEW. This makes it possible to define CDS metadata extensions for any CDS entities with the exception of CDS table functions. Metadata extensions of this type can contain entity annotations, parameter annotations, and element annotations. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The following enhancements have been implemented in CDS access control: New Boolean predicates TRUE and FALSE can now be used as part of an access condition. New inheritance conditions can be used as access conditions in CDS roles. They make the previous inherited access rules obsolete. Some inheritance conditions apply conditions from existing CDS roles and some inheritance conditions apply access rules defined for other CDS entities. In a PFCG condition, a PFCG mapping can be mapped to an element list. This mapping assigns the CDS elements to the authorization fields of an authorization object. A PFCG mapping is defined in a CDS access policy using DEFINE PFCG_MAPPING. </td>
        </tr>
<tr>
            <td>Annotation Definitions</td>
            <td>In the DDLA source code of a CDS annotation definition delivered by SAP, the new annotation @CompatibilityContract must be used to specify the usability of the annotation in CDS entities, depending on their release contract. </td>
        </tr>
<tr>
            <td>New Scope for Annotations</td>
            <td>In a CDS annotation definition, the new enumeration symbol #ENTITY, which defines a scope for entity annotations, can be specified after the annotation @scope. Annotations with this scope can be specified in the definitions of all CDS entities in front of the statement DEFINE. The ABAP annotations @EndUserText.label and @Metadata.allowExtensions were switched to this scope. </td>
        </tr>
<tr>
            <td rowspan="19">ABAP_SQL</td>
            <td>Open SQL Renamed as ABAP SQL</td>
            <td>The existing name Open SQL has been changed to ABAP SQL. This renaming reflects that some parts of ABAP SQL now only support certain database platforms, specifically SAP HANA database, and hence that it is no longer fully platform-independent. </td>
        </tr>
<tr>
            <td>Access to Hierarchy Data</td>
            <td>In queries, both hierarchies and hierarchy navigators can be specified as data sources. Accessing hierarchy data triggers the strict mode from ABAP release 7.73. </td>
        </tr>
<tr>
            <td>Relational Expression IS INITIAL</td>
            <td>The relational expression IS [NOT] INITIAL can now be used in a condition sql_cond to compare operands with their type-dependent initial value. When used, this expression leads to the strict mode from ABAP release 7.72. </td>
        </tr>
<tr>
            <td>Date/Time Functions</td>
            <td>ABAP SQL now supports the following new date/time functions: Time functions TIMS_IS_VALID Time stamp functions TSTMP_IS_VALID TSTMP_CURRENT_UTCTIMESTAMP TSTMP_SECONDS_BETWEEN TSTMP_ADD_SECONDS Date/time conversions TSTMP_TO_DATS TSTMP_TO_TIMS TSTMP_TO_DST DATS_TIMS_TO_TSTMP Time zone functions ABAP_SYSTEM_TIMEZONE ABAP_USER_TIMEZONE When used, these functions require the strict mode from ABAP release 7.71. </td>
        </tr>
<tr>
            <td>Exposing CDS Associations of Common Table Expressions</td>
            <td>When CDS views are accessed within a common table expression, the addition WITH ASSOCIATIONS of the statement WITH can now be used to publish associations of these views for use in path expressions of the current WITH statement. The addition REDIRECTED TO can also be used to replace the target data source of the published association with a previous CTE or the current CTE. When used, this addition leads to the strict mode from ABAP release 7.72. </td>
        </tr>
<tr>
            <td>Numeric Literals in the SELECT List</td>
            <td>Until now, only those numeric literals whose value matched the value range of the type INT4 could be specified as elementary SQL expressions in the SELECT list of a query. Now numeric literals of up to 31 digits can be specified, which are interpreted as numbers of the type DEC if the value range of INT4 is exceeded. </td>
        </tr>
<tr>
            <td>Enhanced Cast Matrix</td>
            <td>A cast expression can now be used to convert the data types INT1, INT2, INT4, and INT8 to DEC. </td>
        </tr>
<tr>
            <td>Subquery as Data Source of MODIFY</td>
            <td>In the ABAP SQL write statement MODIFY, a parenthesized subquery SELECT subquery_clauses can now be specified as a data source after FROM. The rows of the results set of the subquery are modified or inserted in the target table directly on the database. No data transport is required between the database and AS ABAP. When used in MODIFY, a subquery demands strict mode from ABAP release 7.73. </td>
        </tr>
<tr>
            <td>Null Values in the Table Buffer</td>
            <td>The table buffer now supports real null values. In table buffering, null values are no longer transformed to type-dependent initial values. When the buffer is accessed, the same results are produced as when the database is accessed directly. The corresponding restrictions now no longer apply. The following are some of the aspects affected: Relational expressions with operands that contain null values. The result of a comparison of this type is now also unknown when the comparison is made in the buffer (except in the expression IS [NOT] NULL). If used, IS [NOT] NULL no longer bypasses table buffering. Accesses to buffered CDS views. When the buffer is accessed, the same results are now produced as when the database is accessed directly. Null values are often produced by outer joins or in expressions such as case distinction expressions. The restriction specifying that only those CDS views are buffered whose elements do not contain null values no longer applies. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>The following restrictions were removed: For certain SQL expressions and functions, an ABAP SQL read statement no longer bypasses table buffering. ABAP SQL read statements no longer bypass table buffering in cases where a column is specified on the right side of comparisons or of BETWEEN in a condition that is not required to identify a single row or a generic range. The prerequisite for this is that both operands are numeric but do not have the type DF16_DEC or DF34_DEC, that both operands are character-like, or that both operands have the type RAW with the same lengths. </td>
        </tr>
<tr>
            <td>New Check</td>
            <td>If the data of the internal table needs to be transported to the database in cases where the internal table is used as a data source of the ABAP SQL statement SELECT, a syntax check warning occurs that can be hidden using the pragma ##itab_db_select. </td>
        </tr>
<tr>
            <td>GROUP BY Addition GROUPING SETS</td>
            <td>In a SELECT statement, the GROUP BY addition GROUPING SETS can now be used. The addition GROUPING SETS makes it possible to group multiple grouping sets under one GROUP BY clause. This is the same as specifying UNION ALL with different GROUP BY clauses. The addition GROUPING SETS has an advantage over a UNION clause grouping because the SELECT clause only needs to be specified once. Using GROUPING SETS triggers strict mode from ABAP release 7.73. </td>
        </tr>
<tr>
            <td>Aggregate Function GROUPING</td>
            <td>The aggregate function GROUPING can now be used in a SELECT statement. The grouping function GROUPING can be used to verify whether a specific column is part of the aggregation. The grouping function can be used only if the GROUP BY clause contains the addition GROUPING SETS. </td>
        </tr>
<tr>
            <td>Secondary Connections</td>
            <td>On an AS ABAP with a SAP HANA database as its standard database, only those secondary connections should be used from the database table DBCON whose secondary database is also a SAP HANA database. Alongside the CONNECTION addition in ABAP SQL, this also applies to Native SQL (ADBC and EXEC SQL). SAP HANA Smart Data Access (SDA) should be used instead. In SDA, secondary databases are addressed from the SAP HANA database using special qualified names or by using virtual tables. If a SAP HANA database is used as a standard AS ABAP database, ABAP programs can use these names across the standard connection. This is only possible in AMDP and Native SQL. Database Shared Libraries (DBSL) are no longer required here. </td>
        </tr>
<tr>
            <td>USING CLIENT and Session Variable client</td>
            <td>In an ABAP SQL read in which multiple queries access client-specific CDS views and the client handling method is defined using the annotation @ClientHandling.algorithm:#SESSION_VARIABLE, the session variable client (which corresponds to the ABAP-specific session variable CDS_CLIENT on the SAP HANA database) must be set to the same value in all of these queries. The runtime error SAPSQL_DIFFERENT_CLIENT_VALUES occurs if USING CLIENT is used to specify a different client ID in one of these queries. This situation can occur in the statement WITH or when using UNION. </td>
        </tr>
<tr>
            <td>Client Handling in Subquery of the Statement INSERT</td>
            <td>The addition USING CLIENT of the statement INSERT can now be specified in a subquery. This means that the client specified for the target table of the insert operation can be different from the client specified for the data source of the subquery. If USING CLIENT is not specified in the subquery, the current client ID is now always applied to automatic client handling. Before ABAP release 7.73, the client ID specified after INSERT using USING CLIENT was also used in the subquery. The FROM clause of the subquery can now access the database table or classic view filled using the INSERT statement. This makes it possible to copy the data from one client to another. In the subquery, using USING CLIENT or accessing the database table or classic view filled by the INSERT statement enables strict mode from ABAP release 7.73. </td>
        </tr>
<tr>
            <td>Replacement Service in Program Calls</td>
            <td>The method ACTIVATE_REPLACEMENT of the class CL_OSQL_REPLACE has the new parameter FLG_SURVIVE_SUBMIT, which now also permits redirections in called programs. </td>
        </tr>
<tr>
            <td>Weaker Check</td>
            <td>ABAP SQL statements that exploit a database property not supported by all database platforms no longer produce a syntax check warning and produce a syntax warning in the extended program checks instead. </td>
        </tr>
<tr>
            <td>Stricter Checks on Syntax Rules</td>
            <td>A violation of the following rules now always produces a syntax error. In the strict modes of the syntax check, these violations already produced a syntax error. Outside the strict modes, they produced a syntax check warning and in most cases a runtime error when the program was executed. When a view is accessed, its key fields must be located together at the start. The additions USING CLIENT and CLIENT SPECIFIED cannot be used when a CDS entity is accessed that is associated with a CDS role. Even when using path expressions, the addition CLIENT SPECIFIED can only be used for client-specific data sources. When a column is specified, the actual names of the components must be used for a database table containing an include structure, and not the names of any groups defined in ABAP Dictionary. An alternative column name of the SELECT list defined using AS can have a maximum of 30 characters. Alternative column names with more than 30 characters are also not allowed after ORDER BY. Columns of the types LCHR and LRAW can be read in a query only if they are read together with the associated length fields. The following applies when using FOR ALL ENTRIES in the SELECT statement: The decimal places in the comparisons between columns of a data source and columns of the internal table of type p must match. All columns of the primary key specified after ORDER BY using PRIMARY KEY must also occur in the SELECT list. The pseudo component table_line can be specified only for internal tables with an elementary row type. A GROUP BY grouping must be applied to columns specified after HAVING outside aggregate functions. The same applies to columns specified directly in the SELECT list when a HAVING clause is specified, but that are not specified after GROUP BY. If a SELECT list is specified as *, a HAVING clause can only be used together with a GROUP BY clause. If an alternative name is used after ORDER BY, this name must be unique and cannot be the same name as a column that does not have any alternative names. A work area wa specified in the INTO clause has fewer components than explicit columns in the SELECT list. A column specified explicitly in the SELECT list cannot be assigned to the associated component of a structured work area wa specified in the INTO clause or to a data object dobj specified in a parenthesized comma-separated list. No character literals or constants can be specified on the right side of LIKE that are more than twice as long as the left side. The statements UPDATE FROM or MODIFY FROM are used to access a projection view in which all fields are key fields. A reference that is too general is specified in the statements INSERT FROM, UPDATE FROM, or MODIFY FROM when a writer stream is created. In the statement UPDATE, a column can occur only on the left side of a single update expression. </td>
        </tr>
<tr>
            <td rowspan="5">AMDP</td>
            <td>AMDP Scalar Functions</td>
            <td>AMDP scalar functions are now supported alongside AMDP table functions. The AMDP function implementation of an AMDP scalar function has an elementary return value and can be used in ABAP like a regular function method. In the implementation of AMDP scalar functions, it is possible to specify the database-specific option DETERMINISTIC after OPTIONS. This buffers the result of the function for the duration of a query. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>The following restrictions were removed: Tabular input parameters are now allowed in AMDP function implementations for CDS table functions. Previously, only elementary input parameters were allowed. In AMDP function implementations for CDS table functions, however, the restriction that only elementary input parameters are allowed still applies. Tabular input parameters of AMDP procedure implementations and of AMDP function implementations can now be made optional using OPTIONAL, but it is still not possible to specify a start value with DEFAULT. When a replacement parameter declared using DEFAULT is specified for an elementary input parameter of an AMDP method, the following (previously undocumented) restrictions were lifted: Constants declared using the addition VALUE IS INITIAL can now be specified for the data types d, t, and x. This previously produced a syntax error. Constants declared using the addition VALUE '00010101' can now be specified for the data type d, or the literal '00010101' can be specified directly. Both previously produced a syntax error. Constants declared by specifying numeric literals after VALUE and whose lengths are not precisely 8 or 6 can now be specified for the data types d and t. This previously produced a syntax error. Until now, values of literals with lengths of precisely 8 or 6 were handled like a string. This incorrect behavior was also modified and the numeric value is handled as the number of days since 01.01.001 or as the number of seconds since 00:00:00. This modification is incompatible if an AMDP procedure or function with an input parameter of this type is called from other database procedures or functions without an actual parameter being assigned to the parameter in question. In AMDP methods, the addition DEFAULT now behaves in the same way as in regular methods. It still cannot be specified for the data types string, xstring, decfloat16, and decfloat34, however, and no literals can be specified that cannot be converted into the data type of the input parameter. </td>
        </tr>
<tr>
            <td>Option CDS SESSION CLIENT Mandatory</td>
            <td>The option CDS SESSION CLIENT is now mandatory when an AMDP method accesses the CDS managed DDIC view of a CDS view whose client handling is determined by the annotation @ClientHandling.algorithm: #SESSION_VARIABLE. If this option is not specified in this case, a syntax error occurs. The option sets the session variable of the database that can be addressed under the name $session.client in the CDS DDL of the ABAP CDS to a particular value when the method is called from ABAP. An exception of the class CX_AMDP_CDS_CLIENT_MISMATCH can now no longer be raised. </td>
        </tr>
<tr>
            <td>Enhancement to Logical Local Database Schemas</td>
            <td>In logical local database schemas, a new flag can be specified that allows the current ABAP database schema to be mapped in the transaction DB_SCHEMA_MAP. The predefined name :abap_db_schema should be used for mappings of this type. </td>
        </tr>
<tr>
            <td>Access to the Current ABAP Database Schema</td>
            <td>In an AMDP method, a logical local database schema to which the current ABAP database schema is mapped (using the predefined name :abap_db_schema) can be specified in the macro $ABAP.schema. For each logical local database schema used in a macro like this, the new addition USING SCHEMA must be specified in the statement METHOD to declare the used objects. In this way, an AMDP method can access database objects located in different database schemas in different systems (including the ABAP database schema) without needing to modify the syntax. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>Convertibility of Structures</td>
            <td>The conversion rules for flat structures are based on their fragment views, where each alignment gap is considered as a fragment. Alignment gaps arise from the alignment requirements of the component's data types. For character-like components, the alignment requirement depends on the character representation: For character representation UCS-2 used by the ABAP programming language, the memory address of character-like data objects must be divisible by 2. For other character representations there are other alignment requirements. For example, the non-Unicode character representation ASCII has no alignment requirement while the Unicode character representation UTF-32 requires a divisibility by 4. Before only Unicode systems were supported, the conversion rules for structures had to take all possible alignment requirements for characters into account, regardless of the actual length of a character in its character representation. Meanwhile, only Unicode systems are supported. The system codepage is UTF-16 and its subset UCS-2 is supported in ABAP language. Any character is represented by 2 bytes and the alignment requirement is always a divisibility by 2. Because of this, the conversion rules for flat structures can be less strict than before and the new determination of possible alignment gaps was introduced in ABAP release 7.53. Example For example, the following assignment between two flat structures was not possible before ABAP release 7.53 but is possible now. TYPES:   BEGIN OF incl1,     num TYPE i,     c2  TYPE c LENGTH 2,   END OF incl1. DATA:   BEGIN OF struc1,     c1 TYPE c LENGTH 2.     INCLUDE TYPE incl1. DATA:     c3 TYPE c LENGTH 2,   END OF struc1. DATA:   BEGIN OF struc2,     c1  TYPE c LENGTH 2,     num TYPE i,     c2  TYPE c LENGTH 2,     c3  TYPE c LENGTH 2,   END OF struc2. struc1 = struc2. In the included substructure incl1, the character component has the same alignment as the integer component and there is no alignment gap. But as long as non-Unicode Systems were supported, an alignment gap had to be assumed behind the included structure in order to make the program executable in Unicode systems as well as in non-Unicode Systems. Hint When downporting programs from higher to lower releases, syntax errors might occur in the lower releases because of the stricter rules there. </td>
        </tr>
<tr>
            <td rowspan="1">DATABASE</td>
            <td>Only SAP HANA Database as Standard Database</td>
            <td>As of this release, only an SAP HANA database is supported as the standard database of an AS ABAP. For this reason, Open SQL was renamed to ABAP SQL (see ABAP SQL in ABAP Release 7.53). Note: Using a secondary connection in ABAP SQL, it is still possible to connect to other databases. </td>
        </tr>
<tr>
            <td rowspan="3">DDIC</td>
            <td>Expanded Limits</td>
            <td>The following limits now apply to the number of fields and the length of the structure of a database table, of a database view, and of a CDS view: Database tables A database table that is not part of the software component SAP_BASIS can now contain 1000 fields for the storage type Row Store and 1500 fields for the storage type Column Store. The total of all field lengths is no longer checked in ABAP Dictionary with regard to database limits. Note: The size limit for structures in ABAP is still checked by ABAP Dictionary. A database table that is part of the software component SAP_BASIS can contain a maximum of 749 fields (as before) and the total of the field lengths in ABAP Dictionary is still restricted to 4030. Database views and CDS views A view that is not part of the software component SAP_BASIS can contain 1500 view fields. The total of all field lengths is no longer checked in ABAP Dictionary. A view that is part of the software component SAP_BASIS can contain a maximum of 749 fields (as before) and the total of the field lengths in ABAP Dictionary is still restricted to 4096. SAP-internal hint This enhancement is done under the assumption that application programs will run on a SAP HANA Database only. </td>
        </tr>
<tr>
            <td>Dependency Rules</td>
            <td>In ABAP Dictionary, the Dictionary DDL can be used to define dependency rules in the SAP HANA database. A dependency rule makes it possible to derive additional selection conditions from existing conditions when a database table is accessed. In the data aging concept, for example, dependency rules are used in the optimization of access to old data. </td>
        </tr>
<tr>
            <td>Pooled Tables and Cluster Tables Obsolete</td>
            <td>All support for pooled tables and cluster tables has been dropped. Any existing pooled tables and cluster tables are transformed to transparent tables. Any existing table pools and table clusters are removed. All restrictions that applied when accessing pooled tables and cluster tables hence no longer apply. SAP-internal hint See also SAP Note 2565515. </td>
        </tr>
<tr>
            <td rowspan="1">DYNPROS</td>
            <td>Reference Value for Conversion Exits</td>
            <td>An optional input parameter with the predefined name REFVAL can be created for the function modules for conversion exits. When a dynpro field of the type CURR or QUAN is converted, this parameter is given the value of a an associated reference field of the type CUKY or UNIT automatically If WRITE USING EDIT MASK is used, the input parameter REFVAL is not filled. </td>
        </tr>
<tr>
            <td rowspan="3">EXCEPTIONS</td>
            <td>Raising Runtime Errors</td>
            <td>The new statement RAISE SHORTDUMP raises runtime errors associated with an exception object. This means more information can now be passed to the short dump than was previously possible in an exit message. </td>
        </tr>
<tr>
            <td>Last Message in a Chain of Exception Objects</td>
            <td>The new method GET_LATEST_T100_EXCEPTION in the class CL_MESSAGE_HELPER is used to return the last object in a chain of exception objects that has an exception text defined by a message. Here, the chain is created using the attribute PREVIOUS. </td>
        </tr>
<tr>
            <td>Setting the Attribute IS_RESUMABLE</td>
            <td>The attribute IS_RESUMABLE of an exception object is now set after exceptions raised by the statement RAISE RESUMABLE EXCEPTION only if the CATCH statement in question has the addition BEFORE UNWIND. Previously in catches, the attribute was set for every exception raised in this way. After an exception is raised using the statement RAISE RESUMABLE EXCEPTION and caught using the statement CATCH BEFORE UNWIND, the attribute IS_RESUMABLE is set for all previous exception objects referenced in the attribute PREVIOUS and not just for the current exception object. Up until the first resumable exception in the chain IS_RESUMABLE is set to the value of ABAP_TRUE and is set to the value of ABAP_FALSE otherwise. </td>
        </tr>
<tr>
            <td rowspan="3">ITAB</td>
            <td>Predicates in WHERE Conditions</td>
            <td>In WHERE conditions of the statements LOOP AT itab, DELETE itab, and MODIFY itab, and in table iterations with FOR, IS [NOT] INSTANCE OF can now be specified alongside the predicate expressions IS [NOT] INITIAL and IS [NOT] BOUND. </td>
        </tr>
<tr>
            <td>Stricter Syntax Check in COLLECT</td>
            <td>Before the statement COLLECT can be executed for an internal table, all components that are not part of the primary table key must have a numeric data type. Until now, if certain structured components broke this rule, a syntax check warning and a program runtime error occurred. Now a syntax error occurs here too. </td>
        </tr>
<tr>
            <td>Changes to Write-Protected Components</td>
            <td>Before ABAP release 7.73 it was possible to overwrite those write-protected components addressed using a data reference variable in a standard table, if the profile parameter abap/runt/write_check_fix was set to off. This profile parameter was removed in ABAP release 7.73 and it is no longer possible to modify write-protected components. </td>
        </tr>
<tr>
            <td rowspan="1">IXML_LIBRARY</td>
            <td>iXML Library for ABAP Cloud</td>
            <td>The iXML Library now provides a wrapper API to handle XML documents in ABAP for Cloud Development. To access the iXML Library, use the syntax cl_ixml_core()=&gt;create. </td>
        </tr>
<tr>
            <td rowspan="4">MISC</td>
            <td>ABAP for Cloud Development</td>
            <td>In ABAP release 7.53, a new ABAP version ABAP for Cloud Development was introduced. The internal version ID in the column UCCHECK of the system table TRDIR is 5. </td>
        </tr>
<tr>
            <td>Obsolete ABAP Language Versions</td>
            <td>The ABAP language versions Static ABAP with restricted object use Standard ABAP with restricted object use are now obsolete and should no longer be used. </td>
        </tr>
<tr>
            <td>New Methods for Handling Code Pages</td>
            <td>The methods CONVERT of the interfaces IF_ABAP_CONV_OUT and IF_ABAP_CONV_IN are more robust than the methods CONVERT_TO and CONVERT_FROM of the class CL_ABAP_CODEPAGE and have replaced them. </td>
        </tr>
<tr>
            <td>Error Message After SUBMIT VIA JOB</td>
            <td>The method GET_ERROR_MESSAGE of the class CL_ABAP_SUBMIT_HANDLING can now be used to read the last error message in cases where it was not possible to schedule a background task using SUBMIT VIA JOB. </td>
        </tr>
<tr>
            <td rowspan="1">NATIVE_SQL</td>
            <td>New Method GET_ABAP_CONNECTION in CL_SQL_CONNECTION</td>
            <td>The new method GET_ABAP_CONNECTION of the class CL_SQL_CONNECTION works in the same way as the existing method GET_CONNECTION if the latter is passed the value abap_true SHARABLE for a connection shared in ABAP SQL, Native SQL, and AMDP. GET_ABAP_CONNECTION can generally be used instead of GET_CONNECTION, unless it needs to be called with the value abap_false (default) for the parameter SHARABLE to create an exclusive connection. </td>
        </tr>
<tr>
            <td rowspan="1">SHARED_OBJECTS</td>
            <td>Cross-Server Invalidations and Releases</td>
            <td>Until now, cross-server invalidations and releases of area instance versions were only possible for transactional areas. From ABAP release 7.53, cross-server invalidations and releases can be performed for all areas with the Application Server area binding by using the associated parameter AFFECT_SERVER of the methods INVALIDATE_... and FREE_... of the area class in question. The parameter AFFECT_SERVER is added to the methods of existing area classes when a new generation is performed in transaction SHMA. </td>
        </tr>
<tr>
            <td rowspan="1">SYSTEM_CLASSES</td>
            <td>System Classes for RAP Runtime Context Information</td>
            <td>The new system class CL_ABAP_BEHV_AUX is available for retrieving RAP runtime context information. Using the GET_CURRENT_HANDLER_KIND method, you get the information about which RAP handler method is currently running. </td>
        </tr>
<tr>
            <td rowspan="1">TRANSFORMATIONS</td>
            <td>Resumable Exceptions in Deserializations with ST</td>
            <td>The new transformation option OPTIONS exceptions = 'resumable' can be used to make exceptions of the class CX_ST_DESERIALIZATION_ERROR into resumable exceptions. In handling with CATCH BEFORE UNWIND, the new attribute RESULT_REF_FOR_RESUME of the exception object of the class CX_ST_DESERIALIZATION_ERROR points to the target field where the exception was raised. The canceled transformation can be resumed using RESUME. </td>
        </tr>
<tr>
            <td rowspan="3">TYPES</td>
            <td>Checks on STRUCTURE Typing</td>
            <td>In obsolete STRUCTURE typing of formal parameters and field symbols, one assigned data object must be at least as long as the structure in question. Typing checks now respect the alignment gaps at the end of the data object and the structure. </td>
        </tr>
<tr>
            <td>Checks on Literals as Actual Parameters</td>
            <td>The value of a literal passed to a procedure must not be modified in the procedure. In certain cases this was, however, possible, namely when literals as actual parameters were passed to differently typed CHANGING parameters of subroutines. This is now prevented by stricter checks and always produces a runtime error. </td>
        </tr>
<tr>
            <td>Checks on Common Data Areas</td>
            <td>Obsolete common data areas with the same name and defined using COMMON PART must have identical layouts. Common data areas are now viewed as structures whose structure fragment view must be identical and whose deep components must be compatible (pairwise). If not, the runtime error LOAD_COMMON_PART_STRUCT occurs. In common data areas it is now no longer possible to declare object reference variables with the static type of program-local classes and interfaces, nor is it possible to declare data reference variables with the static type of program-local structured types. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 752</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">ABAP_CDS</td>
            <td>Annotations as CDS Objects</td>
            <td>From ABAP release 7.52, it is possible to define CDS annotations as standalone CDS objects using the statement DEFINE ANNOTATION in DDLA source code. A CDS annotation definition dictates precisely how the annotation needs to be specified in CDS source code in annotation syntax. In DDL source code, annotation definitions are covered by source code coloring and Code Completion. In DDLA source code and DDLX source code, the definitions are also covered by the syntax check. SAP delivers annotation definitions for the SAP annotations. No other annotations should be used at present. </td>
        </tr>
<tr>
            <td>Cardinality in LEFT OUTER JOIN</td>
            <td>In a LEFT OUTER JOIN, an addition TO ONE or TO MANY can be specified for the cardinality. This addition is evaluated by an SAP HANA database as a note for optimization. This option exists in earlier releases in ABAP CDS, but has been documented only from ABAP release 7.52. In ABAP SQL, the corresponding syntax was introduced in ABAP release 7.52. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The following enhancements have been implemented in CDS access control: CDS roles can now also be defined for CDS table functions. When a CDS table function is accessed in ABAP SQL. The access conditions are evaluated by default. Extensions in DDL for defining CDS roles: When access conditions cond_expr are specified, there are no longer any restrictions on how Boolean operators and parentheses are used. The Boolean operator NOT can now also be used and any combination of parentheses is possible. The operators BETWEEN and IS NULL are now available for literal conditions as part of an access condition. When a path is specified for an element in an access condition, multivalue associations are now also tracked. Further conditions can now be appended after INHERIT using AND. In a literal condition [NOT] LIKE, it is now possible to specify an escape character using ESCAPE. Blanks are now no longer forced in certain positions. </td>
        </tr>
<tr>
            <td>Extensions</td>
            <td>CDS DDIC-based view extensions are now connected to Switch Framework whenever they are defined in a package that is assigned a switch. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_DOC</td>
            <td>Documentation Links</td>
            <td>The new syntax {@link ...} makes it possible to link to other documentation of repository objects (or its components) in ABAP Doc comments. </td>
        </tr>
<tr>
            <td>Further Changes</td>
            <td>The following changes have also been made to ABAP Doc comments: Opening and closing tags no longer need to be specified in lowercase letters. The restriction to 7 bit ASCII characters no longer applies. An ABAP Doc comment can now contain any 16 bit Unicode characters. &lt;br&gt; or &lt;br/&gt; can be used to specify a line break. &lt;br&gt;&lt;/br&gt; should no longer be used. The special characters @ and &lt; now only need to be escaped when they are placed directly in front of another character. Lists can now be nested. The tags &lt;p&gt;, &lt;ul&gt;, or &lt;ol&gt; can now be placed within &lt;h1&gt;, &lt;h2&gt;, &lt;h3&gt;, &lt;p&gt;, &lt;em&gt;, or &lt;strong&gt; tags. Text can now be specified directly within &lt;ol&gt; and &lt;ul&gt; tags. In the &lt;ol&gt; tag, the attributes reversed, start, and type can now be specified with their traditional HTML meaning. The attributes specified in tags are now checked by the syntax check. SAP-internal hint Another enhancement for ABAP release 7.52 (Kernel 7.53), test references, was not downported to ABAP release 7.52 </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_OBJECTS</td>
            <td>Implementing Interfaces</td>
            <td>When implementing interfaces in classes with the statement INTERFACES, the following restrictions do not apply: Methods and attributes of component interfaces of the implemented interface can now be specified after the additions ABSTRACT METHODS, FINAL METHODS, and DATA VALUES using the name of the component interface and the interface component selector~. Until now, this was only possible for methods by using an alias name, for attributes the interface itself had to be included. Attributes that are declared in component interfaces can now be specified after the addition DATA VALUES by using alias names. </td>
        </tr>
<tr>
            <td rowspan="12">ABAP_SQL</td>
            <td>Internal Tables as Data Sources</td>
            <td>An internal table can be specified as a data source data source of a query. This statement cannot be executed on all database systems, if the data from the internal table needs to be passed to the database. </td>
        </tr>
<tr>
            <td>Relational Expressions</td>
            <td>The following is now possible for conditions in expressions: Size comparisons can now be made between character-like data types and are no longer restricted to numeric data types. The operator BETWEEN is also no longer restricted to numeric data types and SQL expressions can now be specified on the right side. The operator LIKE is now also supported. </td>
        </tr>
<tr>
            <td>Conversion Functions</td>
            <td>The new type conversion functions BINTOHEX and HEXTOBIN make it possible to convert byte strings to character strings (and the other way round) in SQL expressions, which is not possible with a CAST expression. </td>
        </tr>
<tr>
            <td>Path Expressions</td>
            <td>The following is now possible for path expressions: Path expressions can now be split over several source code rows at the blanks in the syntax for parameter passes and filter conditions and also before slashes (/). Associations can now be used whose target data sources are CDS table functions. Parameters can now be passed after the associations of a path expression. This makes it possible to specify paths whose associations have CDS entities with input parameters as data sources. In path expressions, it is now possible to specify the cardinality and type of the join expression as attributes. Filter conditions for associations can now be specified in path expressions. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The new addition WITH PRIVILEGED ACCESS switches CDS access control off. </td>
        </tr>
<tr>
            <td>ORDER BY and UP TO, OFFSET in Subquery</td>
            <td>In a subquery, it is now possible to use an ORDER BY clause and the additions UP TO, OFFSET can be used after the clause. It is not possible to execute a subquery with an ORDER BY clause on all database systems </td>
        </tr>
<tr>
            <td>Cardinality in LEFT OUTER JOIN</td>
            <td>In a LEFT OUTER JOIN, an addition ONE TO MANY can now be specified for the cardinality. This is evaluated as a note for optimization by SAP HANA databases. </td>
        </tr>
<tr>
            <td>Addition NOT for BETWEEN and LIKE</td>
            <td>The addition NOT can now specified in front of BETWEEN and LIKE in relation expressions for expressions. </td>
        </tr>
<tr>
            <td>Client Handling</td>
            <td>The following (stricter) rules for the additions USING CLIENT and CLIENT SPECIFIED now apply when switching and disabling implicit client handling in reads on CDS entities: CDS access control does not work for client-independent access. For this reason, the additions USING CLIENT and CLIENT SPECIFIED can only be used in ABAP SQL in reads on CDS entities for which access control is switched off using the annotation AccessControl.authorizationCheck.#NOT_ALLOWED or the addition WITH PRIVILEGED ACCESS in the FROM clause of an ABAP SQL query. Path expressions can only be evaluated if automatic client handling is switched on. This cannot be done using CLIENT SPECIFIED in cases where path expressions are used that contain associations whose target data source is client-specific In path expressions in the FROM clause, the source data sources of the associations cannot be client-specific either. </td>
        </tr>
<tr>
            <td>Replacement Service for ABAP SQL</td>
            <td>The class CL_OSQL_REPLACE can be used in unit tests with ABAP Unit to redirect database accesses in ABAP SQL to other databases. </td>
        </tr>
<tr>
            <td>FOR ALL ENTRIES and Strings in the SELECT List</td>
            <td>In the previous strict modes of the syntax check, the addition FOR ALL ENTRIES of statement SELECT could not be specified together with columns of the types STRING and RAWSTRING or LCHR and LRAW in the SELECT list. This restriction has been removed and now the syntax check simply issues a warning. </td>
        </tr>
<tr>
            <td>Strict Mode in the Syntax Check</td>
            <td>If one the new features listed above is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_UNIT</td>
            <td>Test Relations</td>
            <td>The special ABAP Doc comment "! @testing ... can be used to define test relations between test classes or test methods and repository objects. This allows the display and execution of tests for these repository objects in ADT. </td>
        </tr>
<tr>
            <td rowspan="3">AMDP</td>
            <td>Reference to ABAP Types</td>
            <td>When an AMDP method is implemented in an AMDP class with SQLScript, the following new AMDP macro "$ABAP.type( [name =] abap_type )" can be used to reference ABAP types. The ABAP runtime environment replaces these schemas on the database with the associated database types. </td>
        </tr>
<tr>
            <td>AMDP Options</td>
            <td>The new addition AMDP OPTIONS for METHODS and CLASS-METHODS statements can be used to define attributes of AMDP methods in their declaration: The READ-ONLY option only allows reads in the implementation of the AMDP methods. The CDS SESSION CLIENT option sets the session variable CDS_CLIENT of the database that can be addressed under the name$session.client in the CDS DDL of the ABAP CDS to a particular value when the method is called from ABAP. It avoids the warning from the syntax check and the exception CX_AMDP_CDS_CLIENT_MISMATCH when an AMDP method accesses the CDS managed DDIC view of a CDS view whose client handling is determined by the annotation @ClientHandling.algorithm: #SESSION_VARIABLE. </td>
        </tr>
<tr>
            <td>Logical HDI Containers</td>
            <td>Alongside the existing logical local database schemas, logical HDI containers can now be used as further logical schemas in the AMDP macro $ABAP.schema. The mapping of a physical database schema to a logical HDI container is made in the definition of an ABAP-managed HDI container, which itself links HDI objects to the Change and Transport System (CTS). </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>System Class for Dynamic Mapping</td>
            <td>The system class CL_ABAP_CORRESPONDING now has a new method CREATE_WITH_VALUE, which allows the values of any suitable data objects to be assigned to the components of the target structure or target table. </td>
        </tr>
<tr>
            <td rowspan="2">DATASET</td>
            <td>Placeholders in Paths Specified in Automatic Authorization Checks</td>
            <td>In the database table SPTH, which is relevant for automatic authorization checks, placeholders for specific profile parameters or for the current client can now be specified in the column PATH. These placeholders are then replaced accordingly when evaluated. The possible placeholders are specified in the documentation of the table SPTH. </td>
        </tr>
<tr>
            <td>Automatic Authorization Checks for the FILTER Addition</td>
            <td>If the addition FILTER is used in the statement OPEN DATASET and an automatic authorization check is made using an authorization group and the authorization object S_PATH when a file is accessed using the database table SPTH, the current user must have an authorization for the activity A6 (Read) or A7 (Change) when using the addition FILTER. For compatibility reasons, the undocumented empty value for the activity is still accepted. </td>
        </tr>
<tr>
            <td rowspan="4">EXPRESSIONS</td>
            <td>Exception Object After RAISE EXCEPTION</td>
            <td>The operand position for the reference variable oref of the statement RAISE EXCEPTION oref is now a general expression position. </td>
        </tr>
<tr>
            <td>Object Component Selector After Table Expressions</td>
            <td>If an object component selector -&gt; is specified directly after a table expression, the restriction that this is not possible for table expressions whose result is determined with the value operator VALUE no longer applies. </td>
        </tr>
<tr>
            <td>Key Specification for the FILTER Operator</td>
            <td>In the variant of the constructor expression where a filter table is specified, the addition USING KEY can be used either for the filter table or for the source table. Previously, the filter table had to have a sorted key or a hash key. This restriction does not apply if such a key can be specified for the source table instead. </td>
        </tr>
<tr>
            <td>Pseudo Component table_line in Mapping Rules</td>
            <td>It is now possible to specify the pseudo component table_line as a source component of a source table in mapping rules of the component operator CORRESPONDING, if the table has an elementary row type. In all other cases, the behavior is undefined when the pseudo component is specified. </td>
        </tr>
<tr>
            <td rowspan="1">ITAB</td>
            <td>Virtual Sorting of Internal Tables</td>
            <td>The new method VIRTUAL_SORT of class CL_ABAP_ITAB_UTILITIES enables virtual sorting of a set of internal tables with the same number of rows. The internal tables are handled internally like a single combined table containing all the columns of the involved internal tables. The result is an array of row numbers of the virtually sorted combined table. See the related executable examples. </td>
        </tr>
<tr>
            <td rowspan="1">MESSAGES</td>
            <td>Implicit Message Specification in RAISE EXCEPTION MESSAGE</td>
            <td>A new short form USING MESSAGE of statement RAISE EXCEPTION with the addition MESSAGE makes it possible to pass the content of system fields sy-msg... implicitly to the exceptions of exception classes that include the system interface IF_T100_DYN_MSG. </td>
        </tr>
<tr>
            <td rowspan="6">MISC</td>
            <td>Search in Subtrees in the SAP GUI Version of the ABAP Keyword Documentation</td>
            <td>In the documentation display of the SAP GUI version of the ABAP keyword documentation, the search for a search term can be restricted to the current subtree. This option can be selected from the context menu of a node or in the dialog window of the documentation. </td>
        </tr>
<tr>
            <td>Standard ABAP with restricted object use</td>
            <td>In ABAP release 7.52, a new ABAP version Standard ABAP with Restricted Object Use was introduced. The internal version ID in column UCCHECK of system table TRDIR is 4. </td>
        </tr>
<tr>
            <td>Surrogate Areas in iXML Library</td>
            <td>The iXML Library now supports surrogate areas with the same problems as in ABAP. </td>
        </tr>
<tr>
            <td>New Catchable Exceptions</td>
            <td>The following exceptions in IMPORT were previously non-catchable but are now assigned to the exception class CX_SY_IMPORT_FORMAT_ERROR and hence can be handled: CONNE_ILLEGAL_TRANSPORT_HEADER CONNE_ILLEGAL_TRANSPORT_VERS CONNE_COMPRESS_FLAG_INVALID CONNE_CONTAINER_TOO_SHORT CONNE_DESCRIPTION_FLAG_INVALID CONVERSION_CODEPAGE_UNKNOWN IMPORT_DESCR_ENDMARK_MISSING IMPORT_UNEXPECTED_END_OF_DATA IMPORT_CONTAINER_MISSING IMPORT_FROM_DATA_BUFFER_EMPTY is now also IMPORT_CONTAINER_MISSING IMPORT_FROM_INTTABLE_EMPTY is now also IMPORT_CONTAINER_MISSING IMPORT_CONTAINER_MISSING IMPORT_DECOMPRESS_FAILED IMPORT_OBJECT_DESTROYED </td>
        </tr>
<tr>
            <td>Introduction of ABAP Daemons</td>
            <td>An ABAP Daemon is an instance of an ABAP Daemon class in an ABAP Daemon session. An ABAP Daemon is created again automatically every time a runtime error or a message of type E, A, or X causes a program termination. </td>
        </tr>
<tr>
            <td>Timers for the Non-Blocking Mode</td>
            <td>ABAP Timers can be used to define wait times in the non-blocking mode. An ABAP Timer is created by ABAP Timer Manager and handled using ABAP Timer handlers. The associated class and interfaces are CL_ABAP_TIMER_MANAGER, IF_ABAP_TIMER_MANAGER, and IF_ABAP_TIMER_HANDLER. The new object types replace the previous types CL_APC_TIMER_MANAGER, IF_APC_TIMER_MANAGER, and IF_APC_TIMER_HANDLER. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 751</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="10">ABAP_CDS</td>
            <td>Client Handling</td>
            <td>The new annotation @ClientHandling specifies the client handling of CDS views and CDS table functions. It replaces the annotation @ClientDependent and makes it obsolete. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: CAST expression In a CAST expression, operands of the type SSTRING can now be cast to types other than themselves and back. Here, the type SSTRING behaves like the data type CHAR. In a CAST expression, operands of the types CLNT, LANG, TIMS, and UNIT can now be cast to the types CHAR and SSTRING. Here, the target type must be specified as a data element. In a CAST expression, the data types CHAR, SSTR, and NUMC can now be cast to ACCP, and the other way round. Aggregate expressions can now be used as operands in a CAST expression. New Functions The following additional string functions are now supported: UPPER and LOWER The following additional date/time functions are now supported: ABAP_SYSTEM_TIMEZONE, ABAP_USER_TIMEZONE, TSTMP_TO_DATS, TSTMP_TO_TIMS, TSTMP_TO_DST, and DATS_TIMS_TO_TSTMP A new built-in conversion function FLTP_TO_DEC can be used to convert arguments of type FLTP to packed numbers. An addition AS dtype can now be specified for the aggregate expression AVG to determine the data type of the return value. Conditions Built-in functions can now be specified on the right side of a cond_expr condition of a WHERE condition, an ON condition, a filter condition, or a complex case distinction. In cond_expr conditions, fields of data sources of the type ACCP can now be compared with fields of the same type, and with literals of the type NUMC. The following changes have been made: In CAST expressions to data elements, the restriction no longer applies that the data type, the length, and the number of decimal places of operand and target data type must match precisely. This restriction can now be applied as an optional restriction using the new addition PRESERVING TYPE. This addition specifies explicitly that casts are to be applied to the semantic properties of a data element. PRESERVING TYPE suppresses the syntax warning that handles casts of identical technical types. </td>
        </tr>
<tr>
            <td>Cross Join</td>
            <td>As well as an inner and outer join, it is now possible to use a cross join in a SELECT statement. </td>
        </tr>
<tr>
            <td>CDS Associations</td>
            <td>The following changes have been made: Associations can now be published for union sets formed with UNION. In this case, special rules apply. WITH DEFAULT FILTER can be used to specify a default filter condition for an association. This condition is used as a filter condition in a path expression if no condition is specified for the association here. In a path expression, *: can be used to declare an association as a non-unique association explicitly. </td>
        </tr>
<tr>
            <td>Session Variables</td>
            <td>When a CDS view is accessed using ABAP SQL, it is possible to access the new session variable $session.system_date in which the values of the system field sy-datum are available. </td>
        </tr>
<tr>
            <td>CDS DDIC-Based View Extensions</td>
            <td>The statement EXTEND VIEW can now be used to extend the following CDS views too: CDS views with aggregate expressions and a GROUP-BY clause CDS views with a UNION clause for union sets For enhancements of the GROUP-BY clause and UNION clauses, the existing CDS view must contain the new annotation array AbapCatalog.viewEnhancementCategory[ ] with suitable values. The value #NONE of this annotation array can be used to prevent any enhancements being made to a CDS view using CDS view enhancements. </td>
        </tr>
<tr>
            <td>Annotations</td>
            <td>The following changes have been made: Annotation for Key Fields The new view annotation AbapCatalog.preserveKey can be used to override the default behavior of the addition KEY for defining key fields of a CDS view. If the annotation is specified with the value true, the key fields defined using KEY are also used for the associated CDS database view. Null Values of Annotations For each element annotation that is not part of an annotation array, the special value null can be specified (without quotation marks). This means that the annotations are ignored in the evaluation with class CL_DD_DDL_ANNOTATION_SERVICE by default. </td>
        </tr>
<tr>
            <td>Metadata Extensions</td>
            <td>Metadata extensions are new CDS objects that allow CDS annotations for a CDS entity to be created and transported separately from their DDL source code. Metadata extensions are included by default in the evaluation of annotations with the class CL_DD_DDL_ANNOTATION_SERVICE. Metadata extensions are created using the DDL statement ANNOTATE VIEW. Each metadata extension is linked to a layer, such as a branch, customer, or partner, which determines the priority. </td>
        </tr>
<tr>
            <td>Access Control</td>
            <td>The following enhancements have been implemented in CDS access control: As well as conditional access rules, there are now also full access rules and inherited access rules. There is a new operator ?= for access conditions, which checks not only for a specified value but also the initial value or the null value. A new user condition compares the value of an element of a CDS entity with the current user name. </td>
        </tr>
<tr>
            <td>Key Fields</td>
            <td>The following changes have been made: The key fields of a CDS view that are defined with KEY must now, like the key fields of a CDS table function, be placed without gaps at the start of the SELECT list. The new view annotation AbapCatalog.preserveKey can be used to override the default behavior of the addition KEY for defining key fields of a CDS view. If the annotation is specified with the value true, the key fields defined using KEY are also used for the associated CDS-managed DDIC view (obsolete). </td>
        </tr>
<tr>
            <td rowspan="11">ABAP_SQL</td>
            <td>Common Table Expressions</td>
            <td>The new ABAP SQL statement WITH enables common table expressions (CTEs) to be defined for use in the WITH statement. A common table expression creates a results set that is used in the queries of the WITH statement as a data source. The main query of the WITH statement has an INTO clause and transfers its results set to ABAP data objects. </td>
        </tr>
<tr>
            <td>Cross Join</td>
            <td>As well as an inner and outer join, it is now possible to use a cross join in a SELECT statement. </td>
        </tr>
<tr>
            <td>New Addition OFFSET in SELECT</td>
            <td>In the additions named under additional_options in the statement SELECT, an addition OFFSET can now be specified to specify the first row of the results set. </td>
        </tr>
<tr>
            <td>SQL Functions</td>
            <td>The following changes have been made: The new numeric function DIVISION enables divisions with decimal places. The new string functions LOWER and UPPER implement uppercase and lowercase. The new string functions LEFT, CONCAT_WITH_SPACE, INSTR, and RPAD perform operations on strings. In the string functions LPAD, LTRIM, and RTRIM, arguments passed as constants or literals can now contain special characters. The new date functions DATS_IS_VALID, DATS_DAYS_BETWEEN, DATS_ADD_DAYS and DATS_ADD_MONTHS execute operations with date fields. An addition AS dtype can now be specified in the aggregate function AVG to define the data type of the result. </td>
        </tr>
<tr>
            <td>Extended Result</td>
            <td>The new addition EXTENDED RESULT of an INTO clause can be used to provide an extended result for an object of the class CL_OSQL_EXTENDED_RESULT, which can be queried using methods of the class. </td>
        </tr>
<tr>
            <td>New Additions in DELETE dbtab</td>
            <td>In the variant DELETE FROM target of the statement DELETE, the additions ORDER BY, OFFSET, and UP TO can now be specified to restrict the number of rows to delete. </td>
        </tr>
<tr>
            <td>Access to Cached Views of the SAP HANA Database</td>
            <td>Under certain conditions, a query can read data from the cache when accessing a cached view of the SAP HANA database. This can be checked using the addition EXTENDED RESULT of an INTO clause. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>The following restrictions were removed: The addition ORDER BY PRIMARY KEY of the SELECT statement can now also be specified if a column is specified multiple times in the SELECT list, without the same name being blocked by alternative names. The components of a replacement object must no longer be in the same order as the associated components of the replaced database table or classic view. When accessing CDS views that use session variables, the addition USING CLIENT can now be used. When accessing CDS views that do not use the session variable client, the addition CLIENT SPECIFIED can now be used. </td>
        </tr>
<tr>
            <td>Session Variables in the SAP HANA Database</td>
            <td>A new ABAP-specific session variable CDS_CLIENT for the client ID is modified by the addition USING CLIENT in ABAP SQL reads of CDS entities whose client handling algorithm is governed by that variable. The CDS session variable client is now linked to this HANA session variable. </td>
        </tr>
<tr>
            <td>Inline Declaration for OPEN CURSOR</td>
            <td>An inline declaration with the declaration operator DATA can now also be specified at the operand position for dbcur of the OPEN CURSOR statement. </td>
        </tr>
<tr>
            <td>Strict Mode in the Syntax Check</td>
            <td>If one the new features listed above is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td rowspan="2">AMDP</td>
            <td>Logical Local Database Schemas</td>
            <td>Logical local database schemas were introduced as symbolic names for physical database schemas in the SAP HANA database. Instead of physical database schemas, logical local database schemas can be used by frameworks (in particular AMDP methods) to access objects from different database schemas in Native SQL or AMDP. </td>
        </tr>
<tr>
            <td>Use of Logical Local Database Schemas in AMDP Methods</td>
            <td>When an AMDP method is implemented in an AMDP class with SQLScript, the following new AMDP macro "$ABAP.schema( [name =] schema_name )" can be used to specify logical local database schemas instead of physical database schemas. The ABAP runtime environment replaces these schemas on the database with the associated physical database schemas. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>System Class for Dynamic Mapping</td>
            <td>The new methods CREATE_USING and EXECUTE_USING for making assignments between internal tables by component while using lookup tables have been added to the system class CL_ABAP_CORRESPONDING. If the method EXECUTE for simple assignment is used, the restriction that source and target cannot be identical has been lifted. It should be noted, however, that no temporary copy of the source is created as a target object (like in MOVE-CORRESPONDING), which means that the result in the case of overlapping source and target components is different than when the operator CORRESPONDING is used reflexively. </td>
        </tr>
<tr>
            <td rowspan="1">MESSAGES</td>
            <td>Implicit Message Type in IF_T100_DYN_MSG</td>
            <td>If the object reference variable oref in the variant MESSAGE oref of the statement MESSAGE (used to send a message) points to an object that includes the system interface IF_T100_DYN_MSG, the addition TYPE can be omitted. The message type from the interface attribute MSGTY of the object is then used implicitly. </td>
        </tr>
<tr>
            <td rowspan="5">MISC</td>
            <td>Full Text Search in the ABAP Keyword Documentation for SAP GUI</td>
            <td>Double quotation marks (") can now be used in the input field of the documentation display in the SAP GUI version of the ABAP keyword documentation to force a full text search for a search term. Up until now, the full text search was only available by choosing radio buttons in the dialog box of the documentation. </td>
        </tr>
<tr>
            <td>Static ABAP with restricted object use</td>
            <td>In ABAP release 7.51, a new ABAP version Static ABAP with restricted object use was introduced. The internal version ID in column UCCHECK of system table TRDIR is 3. </td>
        </tr>
<tr>
            <td>New Structure for Export/Import Tables</td>
            <td>Export tables and import tables with a new structure can now be used to save data clusters in database tables and application buffers in the shared memory using EXPORT. In this structure, the field CLUSTD has the data type RAWSTRING (BLOB) and can save data cluster in a single row. The columns required for the administration of a data cluster across multiple rows, SRTF2 and CLUSTR, are now obsolete. The new structure is recommended for export tables and import tables. </td>
        </tr>
<tr>
            <td>Length Restriction for AMC and APC</td>
            <td>The length restriction for messages that can be sent using AMC has been extended from approximately 30,000 bytes to approximately 1 MB. This limit can be increased even further by changing the profile parameter rdisp/long_messages/max_length. The same applies to APC messages, which are sent with an attached client object using the interface IF_APC_WSP_CLIENT_CONN_ATTACH. </td>
        </tr>
<tr>
            <td>Suppressing AMC Messages</td>
            <td>The new parameter I_SUPPRESS_ECHO of the method CREATE_MESSAGE_PRODUCER of the class CL_AMC_CHANNEL_MANAGER can be used to control whether AMC messages can be sent to the current ABAP session or not. </td>
        </tr>
<tr>
            <td rowspan="5">TRANSFORMATIONS</td>
            <td>New Domains for Mapping from ABAP to XML</td>
            <td>The following new special domains have been introduced, which override the default mapping of elementary ABAP types to asXML. XSDUUID_BASE64 for 16-byte UUIDs in base64 format XSDCURRCODE for ISO currency keys XSDUNITCODE for ISO unit keys </td>
        </tr>
<tr>
            <td>New Formats for format in the Attribute option of tt:value</td>
            <td>The following new formats can be specified in parentheses after format in the option attribute of the ST command tt:value: hex for a hexadecimal display of byte-like values uri and uri_full for masking special characters in URIs uri1 and uri2 for displaying literal values in URIs for OData. currCode, unitCode for converting SAP-specific currency and unit keys in ISO units. currency=CURRCODE, unit=UNITCODE for formatting numbers according to a currency or unit key alpha for handling leading zeros in strings Other enhancements: dateTimeLocal, dateTimeOffset, and ticksOffset can now be used on the time stamp types TIMESTAMP and TIMESTAMPL. guid can now also be used on the type c of length 22 for 16-byte UUIDs in base64 format. </td>
        </tr>
<tr>
            <td>decimals New for the Attributeoption of tt:value</td>
            <td>decimals can now be specified in the attribute option of the ST command tt:value, where it defines the number of decimal places of numeric types. </td>
        </tr>
<tr>
            <td>regime New for the Attributeoption of tt:value</td>
            <td>regime(num|char|bin) can now be specified in the attribute option of the ST command tt:value, where it can be used to force numeric, character-like, or byte-like handling. </td>
        </tr>
<tr>
            <td>noError New for the Attributeoption of tt:value</td>
            <td>noError now can be specified in the attribute option of the ST command tt:value, where it prevents exceptions in the formattings language, currCode, and unitCode. These formattings evaluate entries in database tables. </td>
        </tr>
<tr>
            <td rowspan="1">TYPES</td>
            <td>Enumerated Types</td>
            <td>The statement TYPES BEGIN OF ENUM can be used to define enumerated types for enumerated variables, which can have only enumerated values defined for this type. The document Enumerated Objects summarizes the properties of enumerated types and enumerated objects. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 750</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="9">ABAP_CDS</td>
            <td>CDS Table Functions</td>
            <td>The new DDL statement DEFINE TABLE FUNCTION can be used to define CDS table functions as a new category of CDS entities. In platform-specific SQL, a CDS table function is implemented in an associated AMDP function implementation. </td>
        </tr>
<tr>
            <td>CDS Access Control</td>
            <td>ABAP CDS access control, introduced in ABAP release 7.40, SP10, was expanded to include implicit evaluations of CDS roles defined in the ABAP CDS DDL in ABAP SQL. If a CDS entity is associated with a CDS role, an additional access condition is checked by default when the CDS entity is accessed using ABAP SQL. Only that data is read for which the current user has an authorization or that matches a literal condition. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: Enhancements to the CAST expression of a CDS view: A CAST expression now contain nested cast expressions and case distinctions as an operand. Data elements can now be specified as the target type in CAST expressions. This passes its semantic properties to the result. This also makes it possible to map more built-in types to itself than previously. In CAST expressions, the restriction no longer applies that casts of operands of the types DEC, CURR, and QUAN to the same types expect the target type to be long enough. In casts from NUMC to NUMC, however, the lengths must now match exactly. CAST expressions can now be used to cast operands of the types DATS and TIMS to CHAR (if the length of the target type is sufficient). A one character literal with the type NUMC can now be compared with a data source of the type LANG. New SQL functions for CDS views: String functions: CONCAT_WITH_SPACE, INSTR, LEFT, LENGTH, LTRIM, RIGHT, RPAD,RTRIM Byte chain functions: BINTOHEX, HEXTOBIN Date functions and time functions for CDS views: The special date functions DATS_DAYS_BETWEEN, DATS_ADD_DAYS, and DATS_ADD_MONTHS make it possible to calculate with values of the built-in dictionary type DATS in CDS views. The new date function DATS_IS_VALID checks the validity of dates. The new time function TIMS_IS_VALID checks the validity of times. The new time stamp functions TSTMP_IS_VALID, TSTMP_CURRENT_UTCTIMESTAMP, TSTMP_SECONDS_BETWEEN and TSTMP_ADD_SECONDS perform operations with UTC time stamps in packed numbers. Input parameters from the parameter list parameter_list of a CDS view can be passed to many parameters of built-in functions. In the conversion functions UNIT_CONVERSION and DECIMAL_SHIFT, the result type was set to the data type QUAN or CURR with length 31 and 14 decimal places. </td>
        </tr>
<tr>
            <td>Session Variables</td>
            <td>When a CDS view is accessed using ABAP SQL, three session variables ($session.user, $session.client, and $session.system_language) can be accessed here. In these variables, the values of the system fields sy-uname, sy-mandt and sy-langu are available. </td>
        </tr>
<tr>
            <td>Annotation for Input Parameters</td>
            <td>An input parameter of a CDS view or a CDS table function can now be annotated with an annotation @Environment.systemField. The potential values of the annotation assign ABAP system fields to the input parameters. If a CDS entity of this type is used a data source in ABAP SQL, the assigned values can be passed implicitly. In particular, the value #CLIENT enables the client ID of the current client to be passed implicitly, which provides support for client handling in Native SQL of the implementation of a CDS table function. </td>
        </tr>
<tr>
            <td>API for Evaluation of Annotations</td>
            <td>The class CL_DD_DDL_ANNOTATION_SERVICE contains methods for evaluating the annotations of CDS entities. </td>
        </tr>
<tr>
            <td>Exposing CDS Associations</td>
            <td>Path expressions can now be published with more than one association in the SELECT list of a CDS view. The fields of the source data source from the ON condition of the published associated, which then also need to be specified, must be defined using an appropriately specified path. SAP-internal hint This feature was already documented, but it was not possible before. </td>
        </tr>
<tr>
            <td>Extensions</td>
            <td>The statement EXTEND VIEW for CDS DDIC-based view extensions was expanded as follows: Associations for the SELECT statement of the extended CDS view can now be specified after EXTEND VIEW. The following can now be specified in the extension list select_list_extension: Input parameters of the extended CDS view Path expressions for custom associations and for associations of the extended CDS view Special functions </td>
        </tr>
<tr>
            <td>CDS Views with Input Parameters</td>
            <td>In ABAP release 7.50 and higher, the input parameters of CDS views are supported by all database platforms and can be used in ABAP SQL. It is no longer necessary to query property VIEWS_WITH_PARAMETERS using method USE_FEATURES of class CL_ABAP_DBFEATURES. Querying this property results in a warning check from the syntax check. </td>
        </tr>
<tr>
            <td rowspan="16">ABAP_SQL</td>
            <td>Arrangement of SELECT Clauses and FROM Clauses</td>
            <td>From ABAP release 7.50, the FROM clause of a SELECT statement can also be specified in front of the SELECT clause. In this case, the SELECT clause must be introduced using the new addition FIELDS. This arrangement supports tools such as Code Completion in ABAP Editor. </td>
        </tr>
<tr>
            <td>Unions</td>
            <td>From ABAP release 7.50, the addition UNION creates the union of the results sets of two SELECT statements. </td>
        </tr>
<tr>
            <td>Host Expressions</td>
            <td>From ABAP release 7.50, host expressions with the syntax @( expr ) can be specified in many operand positions in which host variables are possible. For expr, all ABAP expressions can calls are possible that can be specified in general expression positions. Operand positions for host expressions in ABAP release 7.50: Arguments of SQL expressions and hence all operand positions in which these are possible. Operand n after UP TO and PACKAGE SIZE in the SELECT statement, Right sides of WHERE, ON, or HAVING conditions, except for LIKE and IN. Actual parameters for input parameters of CDS views. In the write statements INSERT, UPDATE, MODIFY, and DELETE, the work areas wa or the internal tables itab from which the data is taken. Right side of a SET expression in UPDATE. </td>
        </tr>
<tr>
            <td>SQL Expressions</td>
            <td>The following changes have been made: From ABAP release 7.50, SQL expressions can be specified in the following operand positions (except in the SELECT list): Left side of any WHERE condition Left side of a HAVING condition Left side of a complex case distinction As an operand of a cast expression. In the SQL expression CASE, columns with the built-in dictionary type SSTRING can now be used. If an SQL expression can be specified, any individual literals, host variables, and host expressions can also be specified. </td>
        </tr>
<tr>
            <td>SQL Functions</td>
            <td>The following changes have been made: New Numeric Function The new function ROUND rounds numeric values. New String Functions The new functions CONCAT, LPAD, LENGTH, LTRIM, REPLACE, RIGHT, RTRIM, and SUBSTRING perform operations on strings. Coalesce Function Expanded The coalesce function can now have 255 arguments instead of just two and returns the value of the first argument that does not have the null value. Columns with the built-in dictionary type SSTRING can now be used as arguments. </td>
        </tr>
<tr>
            <td>Result Type of COUNT</td>
            <td>From ABAP release 7.50, the result type of all aggregate functions COUNT is INT8. </td>
        </tr>
<tr>
            <td>ON Conditions</td>
            <td>The following is possible from ABAP release 7.50: SQL expressions can be used on the left side of the ON condition of any join expression. The expression IS [NOT] NULL can be used in an ON condition of an outer join. The full ON condition or subconditions of joins can be specified dynamically as (cond_syntax). This is not possible if the full FROM clause is specified dynamically as (cond_syntax). </td>
        </tr>
<tr>
            <td>Columns Specified After BETWEEN</td>
            <td>From ABAP release 7.50, numeric columns can be specified on the right side in an interval condition using BETWEEN, providing the name of the database table or view is prefixed using ~. </td>
        </tr>
<tr>
            <td>Subquery as Data Source of INSERT</td>
            <td>In the ABAP SQL write statement INSERT, a parenthesized subquery can now be specified as a data source after FROM. The rows of the results set of the subquery are inserted into the target table directly on the database. No data transport is required between the database and the application server. </td>
        </tr>
<tr>
            <td>Access to CDS Entities</td>
            <td>The following changes have been made: From ABAP release 7.50, the restriction no longer applies that a CDS entity can only be used together with database tables and classic views in a SELECT statement if addressed using its CDS database view. From ABAP release 7.50, CDS views can be addressed using the name of their CDS entity, even if associated with database tables or classic views using joins or subqueries. This makes accesses performed on a CDS using the CDS database view obsolete. From ABAP release 7.50, the new CDS table functions can also be specified as data sources of a SELECT statement alongside CDS views. If an input parameter of a CDS entity is annotated with the new annotation @Environment.systemField, ABAP SQL can pass the system value that matches the value of the annotation implicitly. The annotation value #CLIENT even prevents an actual parameter from being passed to input parameters explicitly that are annotated in this way for client IDs. </td>
        </tr>
<tr>
            <td>CDS Path Expressions</td>
            <td>From ABAP release 7.50, path expressions can be specified in SELECT statements that access CDS views with associations published for outside use as follows. Path expressions closed with an element can be specified as columns. Path expressions closed with a target data source can be used as data sources of the FROM clause. </td>
        </tr>
<tr>
            <td>Access to Global Temporary Tables</td>
            <td>When the new global temporary tables in ABAP Dictionary are accessed using ABAP SQL, all temporary data stored here is guaranteed to be deleted before the next implicit database commit, If not, a runtime error occurs. </td>
        </tr>
<tr>
            <td>CDS Views with Input Parameters</td>
            <td>In ABAP release 7.50 and higher, the input parameters of CDS views are supported by all database platforms and can be used in ABAP SQL. It is no longer necessary to query the property VIEWS_WITH_PARAMETERS using the method USE_FEATURES of the class CL_ABAP_DBFEATURES. Querying this property results in a warning check from the syntax check. </td>
        </tr>
<tr>
            <td>System Classes</td>
            <td>The following changes have been made: The class CL_ABAP_DBFEATURES can now be used to check all features of databases that can be integrated in ABAP programming but which cannot be used in all database systems. The new system class CL_DBI_UTILITIES contains utility methods for the database interface. The documented method IS_LOGGING_ON can be used to verify whether logging is currently switched on for a database table. </td>
        </tr>
<tr>
            <td>Access to Replacement Objects</td>
            <td>From ABAP release 7.50, it is possible to define a CDS view as a replacement object in ABAP Dictionary (transaction SE11) for a database table or a database view. If a replacement object is defined for a database table or database view specified as a data source of a SELECT statement, the SELECT statement accesses the CDS view and not the database table or the database view. This change was implemented using an internal tool a kernel patch after ABAP release 7.40, SP10. </td>
        </tr>
<tr>
            <td>Strict Mode in the Syntax Check</td>
            <td>If one the new features listed above (with the exception of dynamic join conditions) is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_UNIT</td>
            <td>Test Seams and Injections</td>
            <td>Test seams and injections have been introduced for unit tests in ABAP Unit. When tests are executed, code wrapped in TEST-SEAM and END-TEST-SEAM as part of production code can be replaced by code from test classes wrapped by TEST-INJECTION and END-TEST-INJECTION. Test classes with injections must be created in test includes. With the exception of test includes for class pools and function groups, no test includes can currently be created. </td>
        </tr>
<tr>
            <td rowspan="2">ABAP_VERSIONS</td>
            <td>Expanded Version ID</td>
            <td>Before ABAP release 7.50 there were only two language versions of ABAP: Standard ABAP and the obsolete Non-Unicode ABAP. The language versions were distinguished using an ID defined internally for each program in the column UCCHECK of the system table TRDIR. This ID was initial for Non-Unicode ABAP and X for Standard ABAP. From ABAP release 7.50, the meaning of the column UCCHECK in the database table TRDIR has been expanded to cover a general version ID. This new ID can have values for further ABAP versions other than the initial value and the value X. From ABAP release 7.50, the value 2 can be specified for ABAP for Key Users. These changes must be respected in all places where the column UCCHECK in the database table TRDIR is accessed implicitly or explicitly. This column is evaluated by the ABAP statements INSERT REPORT and SYNTAX-CHECK. For this reason, the addition UNICODE ENABLING of the statement INSERT REPORT has been replaced by the universal addition VERSION and is now obsolete. </td>
        </tr>
<tr>
            <td>Restricted ABAP for Key Users</td>
            <td>In ABAP release 7.50, a new ABAP version for ABAP for Key Users was introduced. This version is designed for enhancements in delivered enhancement points made by Key Users. The internal version ID in the column UCCHECK of the system table TRDIR is 2. </td>
        </tr>
<tr>
            <td rowspan="3">AMDP</td>
            <td>AMDP Table Functions</td>
            <td>Alongside the existing AMDP procedures, the AMDP framework now also supports AMDP functions in the form of table functions in the SAP HANA database. AMDP now have the new addition BY DATABASE FUNCTION of the statement METHOD in implementations of AMDP methods in AMDP classes. These methods are known as AMDP function implementations to distinguish them from the existing AMDP procedure implementations. Unlike AMDP procedure implementations, AMDP function implementations have a tabular return value, but cannot be called like regular functional methods in ABAP. </td>
        </tr>
<tr>
            <td>AMDP Table Functions for AMDP Methods</td>
            <td>In other AMDP methods and using the associated SQL syntax, it is possible to access the AMDP function of an AMDP function implementation with an explicitly defined parameter interface </td>
        </tr>
<tr>
            <td>AMDP Table Functions for CDS Table Functions</td>
            <td>An AMDP function implementation in whose declaration the addition FOR TABLE FUNCTION is specified implements a CDS table function from ABAP CDS. The parameter interface of an AMDP function implementation of this type is specified by the definition of the CDS table function. The associated AMDP function is executed as a data source of an ABAP SQL read statement when accessing the CDS table function directly or indirectly. </td>
        </tr>
<tr>
            <td rowspan="1">ASSIGNMENTS</td>
            <td>System Class for Dynamic Mapping</td>
            <td>The new system class CL_ABAP_CORRESPONDING makes it possible to specify dynamic mapping rules for the component-by-component assignment of structures and internal tables. </td>
        </tr>
<tr>
            <td rowspan="2">EXCEPTIONS</td>
            <td>New System Interface for Messages</td>
            <td>The new system interface IF_T100_DYN_MSG adds predefined attributes for the message type and the placeholders of the message to the interface IF_T100_MESSAGE. IF_T100_DYN_MSG makes it possible to associate any message with exception classes. </td>
        </tr>
<tr>
            <td>MESSAGE Addition for RAISE EXCEPTION and THROW</td>
            <td>The new addition MESSAGE of the statement RAISE EXCEPTION and of the addition THROW in a conditional expression associates any message with an exception object. The exception class in question must include the new system interface IF_T100_DYN_MSG. It is also possible to use the addition with exception classes that include only the system interface IF_T100_MESSAGE, but with restrictions. </td>
        </tr>
<tr>
            <td rowspan="4">EXPRESSIONS</td>
            <td>Predicate Expression for Type Inspection</td>
            <td>The new predicate expression IS INSTANCE OF can be used to detect the dynamic type of an object reference variable. This makes it possible to check the feasibility of a downcast before it is executed. </td>
        </tr>
<tr>
            <td>Case Distinction for Type Inspection</td>
            <td>The special statement CASE TYPE OF makes it possible to check the dynamic type of an object reference variable as a case distinction. </td>
        </tr>
<tr>
            <td>Enhanced Type Interference in Constructor Expressions</td>
            <td>If the character # is specified for the result type, enhancements were made for the following constructor expressions: In the case of the constructor operator REDUCE an attempt is now made to evaluate the first declaration after INIT if the type of the operand position cannot be identified. When the constructor expressions CONV #( ... ) VALUE #( ) REDUCE #( ... ) COND #( ... ) SWITCH #( ... ) are passed to generically typed formal parameters, no type could be derived for # from the operand position until now. From ABAP release 7.50, a concrete type is derived for # for generic formal parameter types where this is possible and feasible if this cannot be determined in any other way. This prevents syntax errors when procedures are called in cases where a previously concrete type of a formal parameter is expanded to a generic type. </td>
        </tr>
<tr>
            <td>Object Component Selector After Table Expressions</td>
            <td>From ABAP release 7.50, the object component selector -&gt; can be specified directly after table expressions that return a reference variable. This makes it possible to access components of the referenced object. An exception are table expressions whose result is determined with the value operator VALUE. </td>
        </tr>
<tr>
            <td rowspan="4">MISC</td>
            <td>Specifying the Logon Language in SUBMIT VIA JOB</td>
            <td>The new addition LANGUAGE of the statement SUBMIT VIA JOB makes it possible to set the logon language of the background session, which was previously only possible when using the function module JOB_SUBMIT directly. </td>
        </tr>
<tr>
            <td>Results Table in ADBC</td>
            <td>The method SET_PARAM_TABLE of the class CL_SQL_STATEMENT, used to access sets in ADBC queries, now accepts sorted and hashed tables as well as standard tables. </td>
        </tr>
<tr>
            <td>Short Texts in ABAP Doc</td>
            <td>It is now possible to define short texts in ABAP comments and synchronize them with the short texts of methods and function modules in ABAP Workbench. </td>
        </tr>
<tr>
            <td>Full Text Search in the Web Version of the ABAP Keyword Documentation</td>
            <td>Quotation marks (") can be placed around a search term in the input fields of the Web version of the ABAP keyword documentation to force a full text search. If there are no quotation marks, an index search is performed and a full text search is made only if the index search does not find any hits. The full text search option has been available in the dialog box of the SAP GUI Version of the ABAP keyword documentation for some time now using radio buttons. From ABAP release 7.50, SP02 this function can also be accessed using quotation marks (") to enter a search term in the documentation display input field. </td>
        </tr>
<tr>
            <td rowspan="4">RFC</td>
            <td>Dynamic Destinations in RFC</td>
            <td>The methods of the class CL_DYNAMIC_DESTINATION are used to manage dynamic RFC destinations in ABAP release 7.50 and higher. In particular, the method CREATE_RFC_DESTINATION makes it possible to create a dynamic destination, which can be used in the current sessions for RFCs. </td>
        </tr>
<tr>
            <td>Point-to-Point Communication for AMC</td>
            <td>Alongside the general publish-and-subscribe mechanism, a point-to-point communication option was introduced for the ABAP messaging channels (AMC). Here, a sender object addresses precisely one receiver session. The send action can be either synchronous or asynchronous. This is done using the new factory method CREATE_MESSAGE_PRODUCER_BY_ID of the class CL_AMC_CHANNEL_MANAGER. The ID of a receiver session is provided by the method GET_CONSUMER_SESSION_ID of the same class. </td>
        </tr>
<tr>
            <td>Enhancements for APC</td>
            <td>The framework for ABAP channels was expanded as follows: Alongside communication using the WebSocket protocol, communication using TCP sockets is now also possible. An AS ABAP used as an APC server can now also stateful. Until now, only stateless servers were possible. An AS ABAP as operate as an APC client. Clients known as detached clients make it possible to open an APC connection to a stateless or stateful APC server and then detach it again immediately so that it can be accessed using attached clients. A connection handle can be used to access existing APC connections across the entire system. A non-blocking model was introduced for stateful APC applications. The class CL_APC_TIMER_MANAGER creates timer managers that can be accessed using the interface IF_APC_TIMER_MANAGER. A timer manager makes it possible to start and stop a timer in stateful APC applications in which the statement WAIT is not allowed. The interface IF_APC_TIMER_HANDLER is used to handle timer events. See also the ABAP Channels documentation in SAP Help Portal. </td>
        </tr>
<tr>
            <td>Forbidden Accesses for APC</td>
            <td>Up to now, any repeated attempts to bind a push channel to a bound messaging channel or any attempts to remove a nonexistent binding were ignored. From ABAP release 7.50, both situations raise an exception. </td>
        </tr>
<tr>
            <td rowspan="4">TYPES</td>
            <td>New Built-In ABAP Type int8</td>
            <td>The new built-in data type int8 enables 8-byte integers with signs to be declared. The associated data type in ABAP Dictionary was introduced with the name INT8. The value range of the new data type int8 is -9,223,372,036,854,775,808 to +9,223,372,036,854,775,807. Apart from the extended value range, the new data type int8 has the same properties as the existing data type i for 4-byte integers, with the following exceptions: The alignment required for data objects of type int8 is an address divisible by 8. The value of the output length of data objects of type int8 is 20. A new calculation type has been introduced for int8, situated between i and p in the hierarchy. </td>
        </tr>
<tr>
            <td>Global Temporary Tables</td>
            <td>Database tables in ABAP Dictionary can be defined using the table category global temporary table. A global temporary table (GTT) can only be filled with temporary data during a database LUW. When a GTT is filled using ABAP SQL, it must be emptied again explicitly before the next implicit database commit. If not, a non-handleable exception is raised. </td>
        </tr>
<tr>
            <td>Value Ranges of Domains</td>
            <td>When the value range of a domain is defined, the data types INT1, INT2, and INT4 are now checked (like INT8) to determine whether the fixed values and interval boundaries are valid values, that is, that they lie within the value range defined by the technical properties. Existing domains with invalid value ranges must be corrected. </td>
        </tr>
<tr>
            <td>Replacement Objects</td>
            <td>A CDS can be assigned as a replacement object to a transparent database table or a classic database view. In ABAP SQL reads, the replacement object is then accessed instead of the original object. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 740_SP10</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="1">ABAP_CDS</td>
            <td>CDS Access Control</td>
            <td>A dedicated access control was introduced for ABAP CDS. The new CDS DCL in ABAP CDS makes it possible to define CDS roles. If a CDS entity of a CDS view is linked with a CDS role, additional access conditions are evaluated by default when the CDS entity is accessed by a query processed by SADL and only that data is read for which the current user has an authorization or that matches a literal condition. Hint From ABAP release 7.65, SADL uses ABAP SQL to access CDS entities. From ABAP release 7.62, ABAP SQL evaluates CDS roles implicitly. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_SQL</td>
            <td>Access to Replacement Objects</td>
            <td>In ABAP release 7.40, SP10 and higher, it is possible to define a CDS view as a replacement object in ABAP Dictionary for a database table or a DDIC database view. If a replacement object is defined for a database table or DDIC database view specified as a data source of a SELECT statement, the SELECT statement accesses the CDS view and not the database table or the DDIC database view. This change was implemented using a kernel patch after ABAP release 7.40, SP10. A replacement object can only be defined using an internal tool. It is currently not possible to use transaction SE11 to define the replacement object. In ABAP release 7.40, the definition of replacement objects is reserved for specific internal SAP developments and should be otherwise transparent. From ABAP release 7.61 onwards (but not in ABAP release 7.60), it will be possible to explicitly define a replacement object in transaction SE11 and implicitly evaluate the object. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 740_SP08</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="7">ABAP_CDS</td>
            <td>CDS Annotations</td>
            <td>The following enhancements have been introduced for CDS annotations: Annotations in a SELECT list of a CDS view can now be entered after an element. Before the name of an annotation, @&lt; must be written instead of @. A new syntax with square and curly brackets makes it possible to use value lists and annotation lists as value specification after the colon of an annotation specification. The new ABAP annotation AbapCatalog.compiler.compareFilter can be used to specify whether, in cases when a CDS association is used more than once, the filter conditions are compared for the path expressions of a view. If the filter condition has identical semantics, the associated join expression is created only once in the variant of the view on the database. </td>
        </tr>
<tr>
            <td>CDS Views with Parameters</td>
            <td>In the statement DEFINE VIEW, input parameters can now be defined for CDS views that can be used in operand positions in the view. When using a CDS view with parameters in a CDS view or in ABAP SQL, the input parameters must be given actual parameters; new additions are available for this in shape of parenthesized, comma-separated lists in the statements SELECT of the DDL and SELECT of ABAP SQL. </td>
        </tr>
<tr>
            <td>CDS View Extensions</td>
            <td>The new statement EXTEND VIEW of the DDL of the ABAP CDS makes it possible to add new view fields to existing CDS views - without making changes - by using CDS DDIC-based view extensions. </td>
        </tr>
<tr>
            <td>Expressions and Functions</td>
            <td>The following enhancements have been implemented: Division with operator / is now supported in arithmetic expressions. The following new built-in functions have been added: String functions CONCAT and REPLACE Numeric functions ABS, DIV, DIVISION, FLOOR, MOD, and ROUND The new coalesce function has been added. In addition to simple case distinction, complex case distinction (searched case) is now also available. The special conversion functions UNIT_CONVERSION, CURRENCY_CONVERSION, and DECIMAL_SHIFT enable unit conversions and currency conversions. </td>
        </tr>
<tr>
            <td>Join Type for CDS Associations</td>
            <td>The new attributes INNER and LEFT OUTER for a CDS association of a path expression enable to declare explicitly in which join the CDS association is performed. </td>
        </tr>
<tr>
            <td>Path Expressions with Filter Conditions in Conditions</td>
            <td>The new addition 1: before a filter condition of a path expression declares this condition as unique and enables the path expression to be used in a WHERE clause or HAVING clause. </td>
        </tr>
<tr>
            <td>Checking Literals Against Fixed Values of Domains</td>
            <td>The new syntax #domain.literal enables literal values literal of a CDS view to be checked against the fixed values of a DDIC domain. </td>
        </tr>
<tr>
            <td rowspan="1">ABAP_OBJECTS</td>
            <td>Optional Interface Methods</td>
            <td>The new addition DEFAULT of the statements METHODS and CLASS-METHODS can be used to make general methods, functional methods, plus event handlers of interfaces optional. An optional interface method does not need to be implemented explicitly in a class when an interface is implemented. Instead, a default behavior is specified for calls of non-implemented methods in the definition. DEFAULT IGNORE calls an empty method and DEFAULT FAIL raises an exception. </td>
        </tr>
<tr>
            <td rowspan="9">ABAP_SQL</td>
            <td>New Column dbtab~* Specified After SELECT</td>
            <td>In the definition of the result set in the SELECT list of a SELECT statement, data_source~* can be specified as an element of the SELECT list from ABAP release 7.40, SP08 to include all columns of different database tables or views used after FROM in the result set. If specified, data_source~* can be combined with individual specified columns col_spec (with the exception of aggregate expressions). When data_source~* is specified in the result set, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Inline Declarations for the Target Area of SELECT</td>
            <td>After the addition INTO of a SELECT statement, inline declarations can be made from ABAP release 7.40, SP08 using the declaration operator DATA(...) with the prefixed escape character @. Inline declarations can be made for individual parenthesized data objects (@DATA(elem1),@DATA(elem2),...), for individual work areas INTO @DATA(wa), and for internal tables INTO TABLE @DATA(itab). Either an elementary data object, a structure, or an internal table is declared depending on the result set defined in the SELECT list and the database tables used after FROM. When inline declarations are used, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>SQL Expressions</td>
            <td>The SQL expressions introduced in ABAP release 7.40, SP05 were revised in the following ways: The operator CASE can now be used to perform complex case distinctions (searched case) as well as simple case distinctions. SQL expressions can be specified after GROUP BY. SQL expressions can be specified together with aggregate expressions in the SELECT list. SQL expressions can be specified as arguments of aggregate functions (except avg) in the SELECT list and the HAVING clause. If one of the new features is used, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Reads on CDS Views with Parameters</td>
            <td>From ABAP release 7.40, SP08, CDS views can be defined with input parameters that are assigned actual parameters when used. To enable this, the option of a parenthesized comma-separated list for parameter passing was added to the data source specified in the statement SELECT: ( pname1 = act1, pname1 = act2, ...) Since not all database systems support views with parameters, the new class CL_ABAP_DBFEATURES with the method USE_FEATURES is available, which detects whether this is possible for the current database system. Furthermore, accessing a view with parameters triggers a warning from the syntax check; this warning can be hidden by a pragma. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>The following previous restrictions were removed: For SQL expressions: From ABAP release 7.40, SP08, a negative sign - can be placed in front of an operand of an arithmetic expression that does not follow an arithmetic operator directly. From ABAP release 7.40, SP08, an operand operand1, operand2, ... in a simple case distinction can now also be an SQL expression. From ABAP release 7.40, SP08, SQL expressions can also be specified together with aggregate expressions and the addition GROUP BY. For joins: From ABAP release 7.40, SP08, the operators LIKE and IN (...) can be used in ON conditions. From ABAP release 7.40, SP08, individual comparisons can be joined using OR and negated using NOT. From ABAP release 7.40, SP08, outer joins no longer need to contain at least one comparison between columns on the left and right side. For dynamic WHERE conditions: Subqueries can now also be specified dynamically. When one of the rule changes is exploited, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Position of the INTO Clause</td>
            <td>From ABAP release 7.40, SP08, the INTO clause can and should be specified as the final clause of a SELECT statement. In this case, the additions UP TO, OFFSET and the additions behind OPTIONS of the SELECT statement must be specified after INTO. If the INTO clause is specified as the final clause, the syntax check is performed in a strict mode. This handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Stricter Checks for Syntax Rules</td>
            <td>From ABAP release 7.40, SP08, the following syntax constructs that have always contained errors now produce syntax errors or runtime errors. Correction for the HAVING Condition Any columns that are specified in a HAVING clause outside of an aggregate expression must also be specified after GROUP BY. Before ABAP release 7.40, SP08, this was not recorded by the static syntax check, but did raise a catchable exception. From ABAP release 7.40, SP08, this produces a syntax check warning and raises an uncatchable exception outside of the strict mode of the syntax check. Example From ABAP release 7.40, SP08, a syntax warning and uncatchable exception for: DATA itab TYPE TABLE OF scarr. SELECT *        FROM scarr        INTO TABLE itab        HAVING carrid = 'LH'. </td>
        </tr>
<tr>
            <td>Strict Mode in the Syntax Check</td>
            <td>If one the new features listed here is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Comparable Types</td>
            <td>A table of comparable types was constructed for comparisons performed on the database. The results of comparisons made between non-comparable types are determined by the database system and produce a syntax error (in the strict mode of the syntax check) or a syntax warning. </td>
        </tr>
<tr>
            <td rowspan="6">AMDP</td>
            <td>Tabular CHANGING Parameters for SQLScript Procedures</td>
            <td>In ABAP release 7.40, SP08, the restriction that SQLScript procedures managed using AMDP cannot have tabular input/output parameters is lifted. The technical restriction that SQLScript procedures do not support INOUT parameters is bypassed by generating a pair of IN and OUT parameters. Here, the OUT parameter is assigned the value of the IN parameter at the start of the procedure and hence can be used like an INOUT parameter. </td>
        </tr>
<tr>
            <td>Catchable Exceptions in AMDP Procedure Implementations</td>
            <td>From ABAP release 7.40, SP08, the exception classes specified under AMDP - Exceptions can be declared in the interface of an AMDP procedure implementation using RAISING and handled when the method is called. Before ABAP release 7.40, SP08, the exception situations in question raised an uncatchable exception and always produced a runtime error. </td>
        </tr>
<tr>
            <td>AMDP BAdIs</td>
            <td>From ABAP release 7.40, SP08, special AMDP BAdIs were introduced for AMDP procedure implementations. These apply the effect of the switches from Switch Framework to the implementation of database procedures in the current database. When an AMDP procedure calls an AMDP procedure managed by an AMDP BAdI, the implementation is executed that matches the current switch setting. </td>
        </tr>
<tr>
            <td>Service Connections</td>
            <td>In ABAP release 7.40, SP08, an input parameter with the previously invalid parameter name connection can be created for an AMDP procedure implementation to create a database connection explicitly. Only the standard connection or service connections R/3*name to the standard database can be used. </td>
        </tr>
<tr>
            <td>Suppressing Syntax Error Messages</td>
            <td>When an AMDP method is implemented after the addition OPTIONS of the statement METHOD, the new option SUPPRESS SYNTAX ERRORS can be specified (for internal use at SAP only). This suppresses any syntax errors caused by database objects addressed in the AMDP procedure but that do not yet exist in the syntax check. </td>
        </tr>
<tr>
            <td>Support for the Language L on SAP HANA</td>
            <td>For internal use at SAP only, AMDP procedure implementations can be implemented in SAP's own low-level programming language L. See AMDP - L for the SAP HANA Database. </td>
        </tr>
<tr>
            <td rowspan="9">EXPRESSIONS</td>
            <td>Predicative Method Calls</td>
            <td>The predicate expression meth( ) IS NOT INITIAL can now be specified in a short form as a predicate method call ... meth( ) ... This makes is possible to use predicate methods in logical expressions as if their return value had a real Boolean data type. </td>
        </tr>
<tr>
            <td>New Boolean Function</td>
            <td>The new Boolean function xsdbool returns the value X or a blank of the type c with the length 1, depending on the truth value of the logical expression specified as the argument. This expands the existing function boolc, whose return value has the type string. This can produce unexpected results in comparisons with text fields and in checks on the initial value. The return value of xsdbool still references the special type XSDBOOLEAN from ABAP Dictionary. This means it is handled like a real truth value in serializations and deserializations to or from asXML and asJSON. Critical uses of boolc now produce a syntax check warning. </td>
        </tr>
<tr>
            <td>Iteration Expressions</td>
            <td>The iteration expressions introduced using FOR (until now only available for table iterations in table comprehensions) have been expanded to include conditional iterations with the additions UNTIL and WHILE. This makes it possible to program any iteration in the constructor expressions NEW and VALUE for creating internal tables. A new reduction operator REDUCE can execute these conditional iterations and table iterations to construct the results of any data types. In the case of table iterations, this is also known as table reduction. </td>
        </tr>
<tr>
            <td>Table Filtering</td>
            <td>The new filter operator FILTER can be used to perform table filtering in which conditions are used to select or remove lines from an internal table. The result is used to construct a new internal table. </td>
        </tr>
<tr>
            <td>Start Value for Constructor Expressions</td>
            <td>The new addition BASE can be used to provide the return value of a constructor expression for structures or internal tables with a start value, before the actual construction starts. The addition BASE can be used in the following constructor expressions: Instance operator NEW and value operator VALUE for structures Instance operator NEW and value operator VALUE for internal tables Component operator CORRESPONDING in the basic form </td>
        </tr>
<tr>
            <td>Inserting Table Lines in Constructed Tables</td>
            <td>When internal tables are constructed using the instance operator NEW and the value operator VALUE, LINES OF can now be used to insert multiple lines from an existing internal table in the target table. </td>
        </tr>
<tr>
            <td>Grouping Internal Tables</td>
            <td>The new variants FOR GROUPS ... OF and FOR ... IN GROUP in an iteration expression for table iterations using FOR can be used to group the lines of internal tables and to evaluate the groups. </td>
        </tr>
<tr>
            <td>Default Value for Table Expressions</td>
            <td>If the type of the result of a table expression or a chaining of table expressions is controlled using the constructor operators VALUE or REF, the additions OPTIONAL and DEFAULT can be used to specify a default value. If no lines are found, no exception is raised and the default value is returned instead. </td>
        </tr>
<tr>
            <td>Restrictions Removed</td>
            <td>The following restrictions were removed: In the conversion operator CONV: A string expression can now be converted to any data type. Previously only character-like data types were allowed. A bit expression can now be converted to the character-like data types c and string. Previously only byte-like data types were allowed. All other types remain invalid. In conversions to a compatible type, a syntax check warning no longer appears when a LET expression exists. This makes the conversion operator CONV a full replacement for the value operator VALUE for elementary data types. As before, the value operator cannot be used to construct any values for elementary data objects except for the initial value. If a casting operator CAST is specified in a result position and the result of a string expression is assigned, any data type can now be specified after CAST. Previously only character-like data types were allowed. and the result of a bit expression is assigned, the data type specified after CAST can now also be a character-like data type c and string. Previously only byte-like data types were allowed. All other types remain invalid. A bit expression on the right side of an assignment can now also be assigned to character-like data types c and string. Previously only byte-like data types were allowed. All other types remain invalid. </td>
        </tr>
<tr>
            <td rowspan="2">ITAB</td>
            <td>Groupings</td>
            <td>A new variant LOOP AT itab ... GROUP BY of the statement LOOP AT itab enables the lines in an internal table to be grouped and then the groups evaluated in a group loop. The new statement LOOP AT GROUP for member groups is used to access the lines in a group. The new variants FOR GROUPS ... OF and FOR ... IN GROUP in an iteration expression for table iterations using FOR can be used to perform the same groupings (and evaluate them) in table comprehensions and table reductions as well. The new options for grouping internal tables exist alongside the existing group level processing functions and can replace them in many cases. </td>
        </tr>
<tr>
            <td>Expressions for Internal Tables</td>
            <td>The following new aspects of expressions apply in particular to the processing of internal tables: Custom iteration expressions in constructor expressions using NEW and VALUE for creating internal tables. Table reductions with the new reduction operator REDUCE. Table filtering with the new filter operator FILTER. LINES OF for specifying lines in the constructor operators NEW and VALUE. Default value OPTIONAL or DEFAULT for table expressions. </td>
        </tr>
<tr>
            <td rowspan="6">MISC</td>
            <td>Constants for Maximum Lengths of Elementary Data Types</td>
            <td>From ABAP release 7.40, SP08, the system class CL_ABAP_ELEMDESCR contains the constants TYPE_P_MAX_LENGTH, TYPE_P_MAX_DECIMALS, TYPE_C_MAX_LENGTH, TYPE_N_MAX_LENGTH, and TYPE_X_MAX_LENGTH for the maximum lengths and decimal places of the generic elementary data types p, c, n, and x. </td>
        </tr>
<tr>
            <td>Time Resolution in WAIT UP TO</td>
            <td>In ABAP release 7.40, SP08, the operand type of the operand sec of the statement WAIT UP TO was changed from i to f and the time resolution changed from seconds to milliseconds. There is no longer any difference here from the variants WAIT FOR ASYNCHRONOUS TASKS and WAIT FOR MESSAGING CHANNELS. </td>
        </tr>
<tr>
            <td>Reference Documentation for ABAP Dictionary</td>
            <td>Reference documentation for the most important ABAP Dictionary objects in the programming language is now available under the ABAP Dictionary node in the ABAP keyword documentation. </td>
        </tr>
<tr>
            <td>Protocol for ABAP Messaging Channels</td>
            <td>From ABAP release 7.40, SP05, ABAP Messaging Channels (AMC) can be used to send and receive messages in SAP's own Push Channel Protocol (PCP). This uses the message type PCP and the class CL_AC_MESSAGE_TYPE_PCP for serializing and deserializing ABAP data. The documentation for AMC and the associated executable examples have been revised to reflect this. </td>
        </tr>
<tr>
            <td>New Classes and Interfaces for ABAP Push Channels</td>
            <td>The previous classes and interfaces in the APC frameworks, introduced using the prefixes IF_APC_WS_ and CL_APC_WS_, are replaced by the (identically named) classes and interfaces with the prefixes IF_APC_WSP_ and CL_APC_WSP_. Unlike the previous classes and interfaces, the new classes and interfaces also support optional subprotocols of the WebSocket protocol. It is recommended that the new classes and interfaces are used. When new ABAP push channels are created, the APC handler class becomes the subclass of the new class CL_APC_WSP_EXT_STATELESS_BASE automatically. The previous classes and interfaces have been kept for compatibility reasons but can be switched easily to the new classes and interfaces. New functions, such as the method ON_ACCEPT of the interface IF_APC_WSP_EXTENSION, are accepted only for the new classes and interfaces. </td>
        </tr>
<tr>
            <td>Subprotocol for ABAP Push Channels</td>
            <td>A subprotocol can be specified when creating an ABAP Push Channel (APC). Currently, SAP's own Push Channel Protocol (PCP) can be specified. The APC framework contains new classes and interfaces for handling messages in the PCP format. The APC handler class of an ABAP push channel with PCP is generated accordingly. ABAP push channels can be linked with an ABAP Messaging Channel (AMC) with the appropriate message type, PCP. For WebSocket clients, the API for PCP is provided as a JavaScript file in the MIME repository. </td>
        </tr>
<tr>
            <td rowspan="3">TOOLS</td>
            <td>Use of System Fields</td>
            <td>The system fields sy-host, sy-sysid, and sy-mandt in logical expressions can indicate potential back doors, like when using sy-uname, and are now checked accordingly. It is possible to define additional system fields, for which this check is performed, by implementing BAdI SLIN_BADI_SEC_BACKDOOR. </td>
        </tr>
<tr>
            <td>Security-Relevant Function Modules</td>
            <td>The check that ensures that the return code sy-subrc is evaluated when a security-relevant function module (such as AUTHORITY_CHECK_TCODE or FILE_VALIDATE_NAME) is called was revised so that the list of predefined function modules can be expanded using the BAdI SLIN_BADI_SEC_PROCEDURES. The program RSLIN_SEC_DISPLAY_SECREL_PROC displays the full list. </td>
        </tr>
<tr>
            <td>Administration Transaction</td>
            <td>The new transaction SLIN_ADMIN is used for the administration of the extended program check and the security checks. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 740_SP05</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="10">ABAP_SQL</td>
            <td>Comma-Separated Lists</td>
            <td>In ABAP SQL, all operands in lists can now be separated by commas and this is also the recommended way of separating them from ABAP release 7.40, SP05. Until now, comma-separated lists could only be used when single target fields were specified in parentheses after INTO in SELECT and when data objects were specified in parentheses after WHERE. Comma-separated lists are now also possible in programs of ABAP language version Standard ABAP where the program property fixed point arithmetic is activated: The following restrictions apply in the statement SELECT When columns, aggregation expressions or SQL expressions are specified in the SELECT list. When columns are specified after GROUP BY When columns are specified after ORDER BY In the statement UPDATE When set expressions are specified after SET This makes blank-separated lists obsolete. If one of these lists is separated by commas, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. The constraint that blanks were forbidden after the opening parenthesis in comma-separated lists after INTO and IN if more than one comma occurs (if more than one data object is specified) no longer applies from SP05. </td>
        </tr>
<tr>
            <td>Escape Character for Host Variables</td>
            <td>ABAP data objects used in ABAP SQL statements (usually variables) are now interpreted as host variables, as in statically embedded Native SQL. From ABAP release 7.40, SP05, host variables can and should be prefixed with the escape character @. Host variables without the escape character are obsolete. If the escape character is used in front of a name of an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. The escape character can only be used in programs of language version Standard ABAP, in which the program property fixed point arithmetic is activated. Using the escape character requires a strict syntax check of the complete statement. During this check, any errors that would normally only be displayed as syntax warnings are reported as syntax errors. </td>
        </tr>
<tr>
            <td>SQL Expressions</td>
            <td>From ABAP release 7.40, SP05, SQL expressions can be specified in a comma-separated SELECT list. The result of an expression of this type (whose operands can be the names of columns or host variables) is determined by the database system and passed to AS ABAP in the appropriate column of the result set. SQL expressions can only be used in programs of language version Standard ABAP, in which the program property fixed point arithmetic is activated. When SQL expressions are used, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Switching Implicit Client Handling</td>
            <td>The new addition USING CLIENT clnt, which can be specified in all ABAP SQL statements as an alternative to CLIENT SPECIFIED, switches implicit client handling to the client specified in clnt. When the addition USING CLIENT is used, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Rule Changes for Joins</td>
            <td>The following previous constraints on joins have been lifted: From ABAP release 7.40, SP05, it is no longer the case that all comparisons of the ON condition must contain a column from a database table or view on the right side as an operand. From ABAP release 7.40, SP05, the right side of a join expression is no longer restricted to single tables or views. The right side can itself be a (parenthesized) join expression whose result set is then evaluated. In ABAP release 7.40, SP05 and higher, RIGHT OUTER JOIN can be used in addition to LEFT OUTER JOIN. From ABAP release 7.40, SP05, fields from the right side in the WHERE condition of the current SELECT statement can be specified in LEFT OUTER JOIN. Fields from the left side can be specified in RIGHT OUTER JOIN. In SELECT statements that exploit the rule changes above, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Evaluating INTO CORRESPONDING</td>
            <td>If all required components are known statically, the assignment of the fields in the addition CORRESPONDING after INTO is now determined when the program is generated and is not delayed until runtime. The addition INTO CORRESPONDING now also modifies the actual SELECT list that is passed to the database and hence also the SELECT statement. If one or more names match, all the columns for which there are no name matches are removed from the SELECT list implicitly. If there are no name matches, none of the columns are removed from the result set. If * is specified for the SELECT list, a list of columns may be updated implicitly. </td>
        </tr>
<tr>
            <td>Access to CDS Entities</td>
            <td>SELECT can be used to access CDS entities. Potential CDS entities are currently CDS views defined in the ABAP CDS DDL using DEFINE VIEW. Implicit client handling is performed for client-dependent CDS views. If this handling is disabled using the addition CLIENT SPECIFIED, the client column is part of the result set, even though the column is not an element of the CDS view. The new addition CLIENT SPECIFIED of statement TYPES can be used to declare a suitable target area. New additions for CLIENT SPECIFIED after FROM make it possible to address the column in the SELECT statement. If (as recommended) the name of the CDS entity is used for accesses and not the name of the CDS-managed DDIC view (obsolete), the syntax check is performed in a strict mode, which subjects the statement to stricter checks than in the regular syntax check. </td>
        </tr>
<tr>
            <td>Strict Mode in the Syntax Check</td>
            <td>If one of the new features specified above is used in an ABAP SQL statement, the syntax check is performed in a strict mode, which handles the statement more strictly than the regular syntax check. </td>
        </tr>
<tr>
            <td>Stricter Checks on Syntax Rules</td>
            <td>In ABAP release 7.40, SP02, a new SQL parser was introduced for ABAP SQL. These parser performs stricter checks on some rules than the old parser. More specifically, the same parser is now used for statically specified ABAP SQL and for the content of dynamic tokens. In ABAP release 7.40, SP02, this parser will initially only be used for the statement SELECT. From ABAP release 7.40, SP05, the new parser will be used for all ABAP SQL statements. One consequence of this is that any following syntax constructs that have always contained errors now produce syntax errors or runtime errors. Corrections for the WHERE Condition All corrections in ABAP release 7.40, SP02 that apply to the WHERE condition now also apply to the statements DELETE, OPEN CURSOR, and UPDATE from ABAP release 7.40, SP05. Example From ABAP release 7.40 SP05, syntax errors for: DELETE FROM spfli WHERE NOT NOT carrid = 'LH'. Corrections for dynamic tokens All corrections in ABAP release 7.40, SP02 that apply to dynamic tokens now also apply to all ABAP SQL statements from ABAP release 7.40, SP05. Example From ABAP release 7.40 SP05, exception for: DELETE FROM (`SPFLI .`) WHERE (`. CARRID = 'LH'`). Correction for OPEN CURSOR The addition WITH HOLD of the statement OPEN CURSOR can be used only in reads performed on the standard database. If the addition CONNECTION is specified at the same time, a runtime error was produced before ABAP release 7.40 SP05 (and not a syntax error), if the database table was specified dynamically. This gap was closed in ABAP release 7.40 SP05. Example From ABAP release 7.40 SP05, syntax errors for: OPEN CURSOR WITH HOLD cursor      FOR  SELECT *                 FROM ('SPFLI') CONNECTION con. </td>
        </tr>
<tr>
            <td>Specifying Dynamic Tokens</td>
            <td>From ABAP release 7.40, SP05 and higher, internal tables, which are specified as dynamic tokens can also have secondary keys in modifying ABAP SQL statements. </td>
        </tr>
<tr>
            <td rowspan="2">AMDP</td>
            <td>Tag Interface for AMDP Classes</td>
            <td>The new tag interface IF_AMDP_MARKER_HDB flags a class as an AMDP class, which can contain AMDP methods for SAP HANA database. </td>
        </tr>
<tr>
            <td>Implementation of AMDP Methods</td>
            <td>The new addition BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT for the statement METHOD turns a method of an AMDP class into an AMDP procedure implementation. This is implemented in the SQLScript language of the SAP HANA database and not in ABAP. The ABAP runtime framework creates a corresponding database procedure in SAP HANA database. This procedure is executed when the AMDP method is called. </td>
        </tr>
<tr>
            <td rowspan="3">EXPRESSIONS</td>
            <td>LET Expressions</td>
            <td>The new LET expressions in the form LET ... IN make it possible to define variables or field symbols as helper fields in expressions. Currently, LET expressions can be used in all constructor expressions where this makes sense. </td>
        </tr>
<tr>
            <td>Component Operator</td>
            <td>The component operator CORRESPONDING is a new constructor operator that enables component by component assignments to be made between structures or between internal operands at operand positions. Mappings between components are based by default on matching names and can be defined using mapping rules. </td>
        </tr>
<tr>
            <td>Table Comprehensions</td>
            <td>Table comprehensions are an enhancement of the instance operator NEW or the value operator VALUE and are used to create the content of internal tables. One or more FOR expressions can now be specified as a subexpression of the constructor expression. These FOR expressions evaluate existing internal tables whose content can be used to construct the result within the loops. </td>
        </tr>
<tr>
            <td rowspan="2">ITAB</td>
            <td>MOVE-CORRESPONDING for Internal Tables</td>
            <td>From ABAP release 7.40, SP05, the operands of the statement MOVE-CORRESPONDING can be internal tables, as well as structures. This has been enabled by a new variant of this statement, which assigns identically named components of internal tables line by line. The new addition EXPANDING NESTED TABLES enables tabular components of structures to be resolved. The addition KEEPING TARGET LINES adds lines to target tables instead of overwriting them. Component by component assignments between internal tables are also possible using the new component operator CORRESPONDING. </td>
        </tr>
<tr>
            <td>Table Comprehensions</td>
            <td>Table comprehensions are an enhancement of the instance operator NEW or the value operator VALUE and are used to create the content of internal tables. One or more FOR expressions can now be specified as subexpressions of the constructor expression. These FOR expressions evaluate existing internal tables whose content can be used to construct the result within the loops. </td>
        </tr>
<tr>
            <td rowspan="2">MESH</td>
            <td>Data Types for Meshes</td>
            <td>Mesh types can be created using the following new variant of the TYPES statement: TYPES BEGIN OF MESH   ...   TYPES snode ... ASSOCIATION _assoc                   TO tnode ON tcomp1 = scomp1 [AND ...].   ... TYPES END OF MESH Meshes are instances of these types and can be used in suitable expressions and statements. </td>
        </tr>
<tr>
            <td>Processing Meshes</td>
            <td>These relationships (defined using mesh associations) between mesh nodes in a mesh can be evaluated in mesh paths. These mesh paths can be used in the following expressions and statements: ... mesh_path ... LOOP AT mesh_path FOR ... IN mesh_path INSERT ... INTO ... mesh_path MODIFY ... mesh_path DELETE ... mesh_path SET ASSOCIATION mesh_path </td>
        </tr>
<tr>
            <td rowspan="3">MISC</td>
            <td>Database Commit for HTTP/HTTPS/SMTP Communication</td>
            <td>In ABAP release 7.40, SP05 and higher, during HTTP/HTTPS/SMTP communication using Internet Communication Framework, a database commit is executed in an ICF server program or ICF client program (except while updating before every response is sent). In previous versions (before ABAP release 7.40, SP05), a call from an ICF client program only raised a database commit, if the work process was switched due to the maximum waiting time being exceeded. </td>
        </tr>
<tr>
            <td>ABAP Push Channels</td>
            <td>ABAP Push Channels (APC) enable bidirectional communication between AS ABAP and the internet using the WebSocket protocol. </td>
        </tr>
<tr>
            <td>New Variant of WAIT UNTIL</td>
            <td>The new variant WAIT FOR PUSH CHANNELS waits for APC messages in ABAP Push Channels (APC). </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 740</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">ABAP_OBJECTS</td>
            <td>Parameter Interface of Functional Methods</td>
            <td>Alongside the return value defined using RETURNING, a functional method can now have any number of other formal parameters. Previously, only input parameters were possible. Accordingly, functional method calls can now be used with the additions EXPORTING, IMPORTING, and CHANGING, so that actual parameters can be bound to the additional formal parameters. When the input parameters of a method are declared, the addition PREFERRED PARAMETER can now also be specified, if the method has output parameters or input/output parameters. The only requirement is that all input parameters and input/output parameters are optional. </td>
        </tr>
<tr>
            <td>Interfaces in Test Classes</td>
            <td>In test classes, the new addition PARTIALLY IMPLEMENTED can be specified in the statement INTERFACES, which allows only parts interfaces to be implemented. This is particularly useful in test doubles. </td>
        </tr>
<tr>
            <td>Exception Handling</td>
            <td>If a class is specified dynamically after TYPE in the statement CREATE OBJECT and the class does not match the static type of the reference variable, the exception can now be handled using the exception class CX_SY_CREATE_OBJECT_ERROR. </td>
        </tr>
<tr>
            <td>Execution of Static Constructor</td>
            <td>Until now, the static constructor of a class was called before each use of the class. It is not necessary to execute the static constructor to use a constant of the class. Accessing a constant of a class no longer results in its static constructor being called. </td>
        </tr>
<tr>
            <td rowspan="12">ABAP_SQL</td>
            <td>Optimized Table Buffering</td>
            <td>The following improvements were made: Table buffering was optimized so that, if the database table is specified statically, its secondary indexes are also respected when data is read from the table buffer (in cases where generic buffering or full buffering is activated). If SELECT is used with FOR ALL ENTRIES, table buffering is now also used when accessing tables with single record buffering and is no longer bypassed. SAP-internal hint Internally, a new buffer management was implemented that replaces the former buffer management. The new buffer management is based on the kernel-internal handling of internal tables and can make use of their secondary keys. In the new buffer management, the buffer is not bypassed any more when isolation level committed read is set. Before, the buffer had to be bypassed during committed read in order to prevent the selection of phantom data. This bypassing during committed read was not documented. </td>
        </tr>
<tr>
            <td>Result Type of the Aggregate Function COUNT( * )</td>
            <td>In cases where the aggregate function COUNT( * ) or COUNT(*) is specified as the only element in the SELECT list and without a GROUP BY clause, INT8 was added to the internal data type of the result. If the value range is to be used in full, a target object with the data type p or decfloat34 must be used after INTO. The system field sy-dbcnt is set to the value -1 in events outside its value range. </td>
        </tr>
<tr>
            <td>Decimal Places in the INTO Clause</td>
            <td>The assignment rules of the INTO clause of the statement SELECT were modified so that surplus decimal places are now always cut off when numbers are assigned to target fields with too few decimal places. Until now, it was possible to round the numbers (depending on the database and table buffering). </td>
        </tr>
<tr>
            <td>Conditions in Outer Joins</td>
            <td>The restriction in previous versions, which meant that only equality comparisons (=, EQ) were possible in the ON condition of outer joins, no longer applies. </td>
        </tr>
<tr>
            <td>Enhancements for Sorting by Primary Key</td>
            <td>If the addition PRIMARY KEY is used after ORDER BY, the following restrictions no longer apply: If individual columns are specified in the SELECT list, it is not necessary to specify the client column explicitly if the addition DISTINCT is used. A view can also be specified statically after FROM, provided that the view contains fewer key fields than view fields. The addition PRIMARY KEY can now also be specified dynamically. </td>
        </tr>
<tr>
            <td>Handling Strings</td>
            <td>The following (previously undocumented) restrictions were lifted: Before ABAP release 7.40 SP02, it was not possible to use DISTINCT * to read database tables containing short strings of type SSTRING. Before ABAP release 7.40, SP02, it was not possible to access database tables with short or long strings of the data types SSTRING, STRING, or RAWSTRING using * in the SELECT list in cases where a join is specified dynamically after FROM. </td>
        </tr>
<tr>
            <td>Field Symbols and Data Reference Variables in SELECT Loops</td>
            <td>When field symbols or dereferenced reference variables are specified for the work area, individual data objects, or internal tables in a SELECT loop after INTO, the data object that is the target of a field symbol or reference variable is identified exactly once, from ABAP release 7.40, SP02, when the loop is entered. This data object is used as a target area in each loop pass. Any modifications to the assignment of a field symbol or reference variable within the loop are ignored. From ABAP release 7.40, SP02, the assignment of a field symbol or reference variable is determined again for each loop pass and the current data object is used as the target area. </td>
        </tr>
<tr>
            <td>Comma-Separated List in INTO Clause</td>
            <td>With ABAP release 7.40 SP02 and higher, a whitespace character can be placed behind the opening bracket of a comma-separated list in the INTO clause. Then, a whitespace character must also be placed in front of the closing bracket. </td>
        </tr>
<tr>
            <td>Specifying Dynamic Tokens</td>
            <td>With ABAP release 7.40 SP02 and higher, internal tables, which are specified as dynamic tokens of statement SELECT, can also have secondary keys. </td>
        </tr>
<tr>
            <td>Stricter Checks on Syntax Rules</td>
            <td>In ABAP release 7.40 SP02, a new SQL parser was introduced for ABAP SQL. These parser performs stricter checks on some rules than the old parser. More specifically, the same parser is now used for statically specified ABAP SQL and for the content of dynamic tokens. In ABAP release 7.40, SP02, this parser will initially only be used for the statement SELECT. One consequence of this is that any following syntax constructs that have always contained errors now produce syntax errors or runtime errors. General corrections From ABAP release 7.40, SP02, the content of the operand n of the additions UP TO n ROWS and PACKAGE SIZE of the statement SELECT meet the rules of a lossless assignment for the data type i. Before ABAP release 7.40, SP02, the operator IN range_tab of a WHERE condition was not always checked statically to see whether the columns LOW and HIGH of the ranges table range_tab could be converted to the data type of the database and non-convertible columns did not produce a runtime error in cases where the ranges table was empty. Now, a static check is always made a non-convertible columns always raise an exception. Example From ABAP release 7.40 SP02, syntax errors for: DATA: range_tab TYPE RANGE OF t,       itab TYPE TABLE OF sflight. SELECT *        FROM sflight        INTO TABLE itab        WHERE fldate IN range_tab. Before ABAP release 7.40, SP02, multiple NOT operators could be placed consecutively in a WHERE condition. An even or odd number of consecutive NOT operators is the same as no NOT or a single NOT, which means that surplus NOT operators can now no longer be specified. Example From ABAP release 7.40 SP02, syntax errors for: SELECT SINGLE *        FROM spfli        INTO wa WHERE        NOT NOT carrid = 'LH'. Before ABAP release 7.40 SP02, it was possible to read the client column when using alias names defined with AS or joins in ON and WHERE conditions, without disabling implicit client handling using CLIENT SPECIFIED. In this case, the result set is empty whenever the explicitly specified client is not the current client. From ABAP release 7.40 SP02, this situation produces a syntax check warning. Example From ABAP release 7.40 SP02, syntax warnings for: SELECT *        FROM scarr AS carriers        INTO TABLE itab        WHERE carriers~mandt = '...'. and SELECT *        FROM scarr        INNER JOIN spfli        on scarr~mandt = spfli~mandt        INTO CORRESPONDING FIELDS OF TABLE itab        WHERE scarr~mandt = '...'. The addition GROUP BY cannot be specified for pooled tables and cluster tables. Before ABAP release 7.40 SP02, it was possible to specify a column dynamically after GROUP BY; however this always raised an exception. From ABAP release 7.40 SP02, a dynamically specified GROUP BY clause in pooled tables and cluster tables produces a syntax warning; this warning will become a syntax error in a future SP. Example From ABAP release 7.40 SP02, a syntax warning or error for: SELECT id object langu typ        FROM doktl        INTO TABLE itab        GROUP BY (`ID OBJECT LANGU TYP`). Corrections for Dynamic Tokens Before ABAP release 7.40 SP02, a single period (.) could be specified in the dynamic tokens of any ABAP SQL statements. This period was ignored when the token was evaluated at runtime. From ABAP release 7.40 SP02, a period like this raises an exception of the class CX_SY_DYNAMIC_OSQL_SYNTAX. Example From ABAP release 7.40 SP02, exception for: SELECT *        FROM (`SPFLI .`)        INTO TABLE itab        WHERE (`. CARRID = 'LH'`). Before ABAP release 7.40, SP02, an alias name could be given more than once in cases where columns were specified dynamically in the SELECT list of the columns after SELECT using column_syntax, even though this is not allowed statically. From ABAP release 7.40 SP02, this raises an exception of the class CX_SY_DYNAMIC_OSQL_SEMANTICS. Example From ABAP release 7.40 SP02, exception for: SELECT SINGLE ('carrid AS col carrname AS col')        FROM scarr        INTO CORRESPONDING FIELDS OF wa        WHERE carrid = 'LH'. Before ABAP release 7.40, SP02 the statically compulsory addition DISTINCT could be omitted when the aggregate function COUNT( DISTINCT col ) was specified dynamically and all rows of the result set were counted. From ABAP release7.40 SP02, the omission of DISTINCT raises an exception of the class CX_SY_DYNAMIC_OSQL_SYNTAX. Example From ABAP release 7.40 SP02, exception for: SELECT ('COUNT( carrid )')        FROM spfli        INTO count. ENDSELECT. In previous releases (before 7.40 SP02), a NOT could be mistakenly written directly in front of a comparison operator in a dynamic WHERE condition (which is not possible in the static case). With ABAP release 7.40 SP02 and higher, this raises an exception of class CX_SY_DYNAMIC_OSQL_SYNTAX. Example From ABAP release 7.40 SP02, exception for: SELECT SINGLE *        FROM spfli        INTO wa        WHERE (`carrid NOT = 'LH'`). Before ABAP release 7.40, SP02 it was possible to use (incorrectly) a dynamic FROM clause combined with the addition ORDER BY PRIMARY KEY to access DDIC projection views containing the same number of key fields and view fields, which is not possible in static cases. From ABAP release 7.40, SP02, this raises the exception CX_SY_DYNAMIC_OSQL_SYNTAX. Example From ABAP release 7.40, SP02, an exception is raised when projection_view has the same number of key fields and view fields. DATA itab TYPE TABLE OF projection_view. SELECT *        FROM ('KELLERH_VIEW')        INTO TABLE itab        ORDER BY PRIMARY KEY. SAP-internal hint For DDIC database views the exception is raised only in the strict modes from 7.40, SP05 on. Corrections for the aggregate function count( * ) As in all aggregate functions, the target field must be chosen appropriately in the case of count( * ) or count(*) and no values must be lost when the result is assigned. This was not checked before ABAP release 7.40 SP02, and assignments were made in accordance with the conversions rules. This did not always raise an exception when values were lost. From ABAP release 7.40 SP02, the target field must be numeric and a loss of values always produces an exception. Example From ABAP release 7.40 SP02, a syntax warning and exception (if the value does not fit in the target field) for: DATA cnt TYPE c LENGTH 1. SELECT COUNT(*)        FROM scarr        INTO cnt. When individual columns or aggregate expressions are specified in the SELECT list, an explicit work area must usually be specified and the obsolete short form is not possible. The only exception here is when count( * ) is used to specify nothing, if no alias name and no GROUP BY clause was specified. Before ABAP release 7.40 SP02, the short form using count( * ), specified together with an alias name or a GROUP BY clause, produced a runtime error. From ABAP release 7.40 SP02, this also produces a syntax error if known statically. Example From ABAP release 7.40 SP02, syntax errors for: TABLES scarr. SELECT COUNT( * ) AS cnt        FROM scarr. SELECT count( * )        FROM scarr        GROUP BY carrid.    ... ENDSELECT. Corrections when using the built-in types LCHR and LRAW from ABAP Dictionary. Columns of the types LCHR and LRAW cannot be used in relational expressions of the SQL conditions. Before ABAP release 7.40 SP02, this produced a runtime error. From ABAP release 7.40 SP02, this also produces a syntax error if known statically. Example From ABAP release 7.40 SP02, syntax errors for: SELECT SINGLE *        FROM  indx        INTO wa        WHERE clustd = '...'. Columns of the types LCHR and LRAW cannot be read using SELECT if the addition DISTINCT is specified. Before ABAP release 7.40 SP02, this produced a runtime error. From ABAP release 7.40 SP02, this also produces a syntax error if known statically. Example From ABAP release 7.40 SP02, syntax errors for: SELECT DISTINCT *        FROM  indx        INTO TABLE itab. Columns of the types LCHR and LRAW can be read using SELECT only if they are read together with the associated length fields. Before ABAP release 7.40 SP02, columns of this type read without length fields produced a syntax warning. From ABAP release 7.40 SP02, this situation always produces a runtime error. Example From ABAP release 7.40 SP02, runtime errors for: SELECT clustd        FROM  indx        INTO TABLE itab. Corrections for FOR ALL ENTRIES If FOR ALL ENTRIES is used in front of a WHERE condition of a SELECT statement, a column of the internal table must be specified in at least one comparison (the comparison can also be specified in a subquery). Before ABAP release 7.40 SP02, the subquery was not checked. From ABAP release 7.40 SP02, the comparison must be specified (statically or dynamically) even if a subquery is specified. Example From ABAP release 7.40 SP02, syntax errors for: SELECT carrid connid fldate        FROM sflight        INTO CORRESPONDING FIELDS OF TABLE rtab        FOR ALL ENTRIES IN itab        WHERE EXISTS ( SELECT * FROM sflight ). When FOR ALL ENTRIES is used in front of a WHERE condition of a SELECT statement, no database fields of the built-in types STRING and RAWSTRING plus LCHR and LRAW can occur in the SELECT list, since the implicit addition DISTINCT cannot be passed to the database system in this case. From ABAP release 7.40, SP02, a syntax warning occurs in the extended program check. This warning can be hidden by a pragma. Example From ABAP release 7.40 SP02, pragma required for: SELECT *       FROM snwd_bpa       INTO TABLE bupas       FOR ALL ENTRIES IN orders       WHERE node_key = orders-buyer_guid       ##select_fae_with_lob[web_address]. If FOR ALL ENTRIES is used in front of a WHERE condition of a SELECT statement, no LOB handles can be created in the target area, since this produces an undefined result. Before ABAP release 7.40 SP02, this was not identified correctly for locators, either statically or at runtime. From ABAP release 7.40 SP02, this produces a syntax error or raises an exception. Example From ABAP release 7.40 SP02, syntax errors for: SELECT picture        FROM demo_blob_table        INTO wa-picture        FOR ALL ENTRIES IN name_tab        WHERE name = name_tab-table_line. ENDSELECT. The addition FOR ALL ENTRIES should not be used with the addition GROUP BY. The addition GROUP BY is ignored if used together with FOR ALL ENTRIES. From ABAP release 7.40 SP02, this situation produces a syntax check warning. Example From ABAP release 7.40 SP02, syntax warning for: SELECT COUNT( * )        FROM spfli        INTO cnt        FOR ALL ENTRIES IN carriers        WHERE carrid = carriers-table_line        GROUP BY carrid. Corrections for ORDER BY Before ABAP release 7.40 SP02, it was possible to specify any text between a dynamically specified column after ORDER BY and the closing period of a SELECT statement and this text was ignored when the statement was executed. Before ABAP release 7.40 SP02, this text produced a syntax warning; from ABAP release 7.40 SP02, it produces a syntax error. Example From ABAP release 7.40 SP02, syntax warning for: SELECT *        FROM scarr        INTO TABLE itab        ORDER BY (`CARRID`) carrname and so on. If the addition ORDER BY is specified together with FOR ALL ENTRIES, all columns of the primary key must be read; if not, the result is undefined. From ABAP release 7.40 SP02, a syntax warning is produced in this case if known statically; at runtime, an exception is always raised. Example From ABAP release 7.40 SP02, a syntax warning or exception for: SELECT carrid connid        FROM sflight        INTO CORRESPONDING FIELDS OF TABLE rtab        FOR ALL ENTRIES IN itab        WHERE carrid = itab-carrid AND              connid = itab-connid        ORDER BY PRIMARY KEY. If aggregate functions are specified after SELECT, all columns that are specified after ORDER BY and that do not have an alias name for an aggregation function must also be specified after SELECT and after GROUP BY. Before ABAP release 7.40 SP02, the checks on this situation at runtime were not strict enough and the behavior was platform-dependent. From ABAP release 7.40 SP02, a violation of this rule always raises an exception of the class CX_SY_DYNAMIC_OSQL_SEMANTICS. Example From ABAP release 7.40 SP02, an exception from the class CX_SY_DYNAMIC_OSQL_SEMANTICS for: SELECT COUNT( * )        FROM spfli        INTO (cnt)        GROUP BY ('CARRID')        ORDER BY ('CARRID').   ... ENDSELECT. An alias name in the SELECT list cannot be the name of a column to which no alias name is assigned. Before ABAP release 7.40 SP02, the use of a name of this type after ORDER BY raised an exception. From ABAP release 7.40 SP02, this also produces a syntax error if known statically. Example From ABAP release 7.40 SP02, syntax errors for: SELECT carrid connid AS carrid        FROM spfli        INTO TABLE itab        ORDER BY carrid. </td>
        </tr>
<tr>
            <td>Access to SAP HANA Views</td>
            <td>DDIC external views in ABAP Dictionary make SAP HANA views known in ABAP programs. They also allow type references and access using ABAP SQL. </td>
        </tr>
<tr>
            <td>SQLScript Call</td>
            <td>The new statement CALL DATABASE PROCEDURE enables SQLScript to be called from ABAP programs. </td>
        </tr>
<tr>
            <td rowspan="1">DATE_TIME_PROCESSING</td>
            <td>Weaker Type Check for Time Zones</td>
            <td>A character-like data object is now sufficient in all operand positions where a time zone could previously only be specified with exactly the type TZNZONE or as a text field of the length 6. This concerns the statements CONVERT [INTO] TIME STAMP and formatting of string templates and WRITE. </td>
        </tr>
<tr>
            <td rowspan="5">EXPRESSIONS</td>
            <td>Inline Declarations</td>
            <td>The new declaration operators DATA and FIELD-SYMBOL make inline declarations possible in declaration expressions in declaration positions. </td>
        </tr>
<tr>
            <td>Constructor Expressions</td>
            <td>A new type of expression, constructor expressions, uses constructor operators in general expression positions to construct results of specified types. NEW creates objects VALUE creates values REF gets references EXACT performs a lossless assignment or calculation CONV converts values CAST performs an upcast or downcast COND and SWITCH enable conditional expressions </td>
        </tr>
<tr>
            <td>Table Expressions</td>
            <td>The new table expressions itab[ ... ] allow reads to be performed on internal tables in operand positions. </td>
        </tr>
<tr>
            <td>New Built-In Function ipow</td>
            <td>The new integer power function ipow enables fields of any numeric type and with integer values to be raised to a power. The calculation type is determined by the base. </td>
        </tr>
<tr>
            <td>Writable Expressions</td>
            <td>Some expressions are writable expressions that can be specified in result positions. </td>
        </tr>
<tr>
            <td rowspan="7">ITAB</td>
            <td>Table Expressions</td>
            <td>The new table expressions allow reads to be performed on internal tables in operand positions. </td>
        </tr>
<tr>
            <td>Built-In Functions for Internal Tables</td>
            <td>The following built-in functions were introduced: The function line_index can be used to identify a line number in an index of an internal table and use it in operand positions. The predicate function line_exists can be used to check the existence of table lines; the resulting truth value can then be used directly. </td>
        </tr>
<tr>
            <td>Explicit Definition of an Empty Key</td>
            <td>The new addition EMPTY KEY of the statements TYPES, DATA, and so on can be used to define an empty table key explicitly for standard tables. Without this addition, empty table keys occur only if the standard key of a standard table does not contain any components suitable as key fields. </td>
        </tr>
<tr>
            <td>Table Sharing for Boxed Components</td>
            <td>Until now there was no table sharing if the line types contained boxed components. This restriction has now been lifted. </td>
        </tr>
<tr>
            <td>References in Dynamically Specified Components</td>
            <td>Object component selectors can now be specified when components are specified dynamically (this was not previously the case). However, those specifications can be made that are statically possible. For example, when using ASSIGN attributes cannot be accessed that are not known statically. This is the case, for example, when using superclass references to access subclass components. </td>
        </tr>
<tr>
            <td>Optimization of the WHERE Condition</td>
            <td>The rules described under Optimization of the WHERE Condition have been modified as follows: Except in comparisons for equality, optimizations are now also performed for the predicate expression IS INITIAL. This enables a simple check to be made on the initial value, in particular for columns typed as reference variables. A static WHERE condition cannot contain any duplicate or overlapping specified keys, if the prerequisites for optimizations are met. This means that this change is occasionally incompatible: Syntax errors are now produced in those WHERE conditions in which a key column is checked using both a comparison for equality with one value and using IS INITIAL. Syntax warnings are no longer produced by mistake for non-optimizable type combinations in comparisons, even if the comparison does not contain any key columns. Non-optimizable type combinations now produce syntax warnings only if they actually modify key fields of a hash key or of the initial part of a sorted key. The syntax warnings have been improved so that the non-optimizable combination is mentioned in the text. The following type combinations were not previously detected as non-optimizable when secondary keys were used in comparisons: string with n, i (b, s), f, decfloat16, and decfloat34 xstring with c and string There was no syntax error or exception in these cases. Instead, all lines of the internal table were checked linearly when the statement was executed or reads performed using the primary key. Optimized reads are guaranteed when using secondary keys, which is why the combinations above now produce syntax errors or raise exceptions. This represents an incompatible change. </td>
        </tr>
<tr>
            <td>Expression for Dynamic Sorts</td>
            <td>In the statement SORT itab, the table (otab) can now be specified for dynamic sorts as the result of an expression or functional method call. </td>
        </tr>
<tr>
            <td rowspan="4">JSON</td>
            <td>JSON-XML</td>
            <td>JSON-XML is a special XML format that enables JSON data to be described using an XML representation. A new format, IF_SXML=&gt;CO_XT_JSON, has been added to the sXML Library, which enables JSON data to be processed using JSON-XML. </td>
        </tr>
<tr>
            <td>asJSON</td>
            <td>The canonical JSON representation asJSON defines a mapping between ABAP types and JSON. This is used in serializations and deserializations using the identity transformation ID. </td>
        </tr>
<tr>
            <td>JSON and CALL TRANSFORMATION</td>
            <td>JSON data can be specified in various forms as an XML source in the statement CALL TRANSFORMATION and a JSON writer can be specified as target. The identity transformation ID supports JSON by using asJSON. More information is available in Transformations for JSON. </td>
        </tr>
<tr>
            <td>JSON and escape</td>
            <td>The escape function escape supports the new format E_JSON_STRING for replacing special characters in JSON. </td>
        </tr>
<tr>
            <td rowspan="18">MISC</td>
            <td>ABAP Doc</td>
            <td>Special ABAP Doc comments can be entered in front of declaration statements. These comments are prefixed by "!. Tools of an ABAP development environment that support ABAP Doc, such as ABAP development tools for Eclipse, analyze the content of ABAP Doc comments, converts it to HTML and display the content in an appropriate way. </td>
        </tr>
<tr>
            <td>Enhancement to RTTI</td>
            <td>The attribute METHODS of the object description class CL_ABAP_OBJECTDESCR has a new component, IS_RAISING_EXCPS, which can be used to determine whether the exception of a method is class-based. Also, the tabular component EXCEPTIONS the new component IS_RESUMABLE, which can be used to determine whether a class-based exception can be propagated as a resumable exception. </td>
        </tr>
<tr>
            <td>Authorization Check in CALL TRANSACTION</td>
            <td>The new additions WITH AUTHORITY-CHECK and WITHOUT AUTHORITY-CHECK make it possible to perform or skip an authorization check when executing the statement CALL TRANSACTION. This makes the statement CALL TRANSACTION obsolete if neither of these additions are specified. </td>
        </tr>
<tr>
            <td>Documentation Moved</td>
            <td>The documentation describing shared objects object services iXML Library sXML Library logical databases has been integrated into the ABAP keyword documentation. </td>
        </tr>
<tr>
            <td>Operand Position in WRITE and WRITE TO</td>
            <td>Until now, the statements WRITE and WRITE TO could only be used to produce or assign a single data object dobj. Now the following can also be specified instead of dobj: A built-in function, a functional method call or method chaining, or a constructor expression If the return value/result meets the conditions for dobj. A string expression. Arithmetic expressions and bit expressions cannot be specified directly, but can be specified as embedded expressions in string templates. </td>
        </tr>
<tr>
            <td>Declaration Positions</td>
            <td>The new inline declarations can be made in the new declaration positions. </td>
        </tr>
<tr>
            <td>Operand Position in CALL FUNCTION ... EXPORTING</td>
            <td>Actual parameters specified after CALL FUNCTION ... EXPORTING have now become a general expression position. </td>
        </tr>
<tr>
            <td>Operand Position After CASE</td>
            <td>The operand position after CASE was changed to a general expression position. </td>
        </tr>
<tr>
            <td>Operand Position in Dynamic ASSIGN</td>
            <td>The operand comp in the statement ASSIGN COMPONENT comp OF STRUCTURE is now a character-like or numeric expression position. </td>
        </tr>
<tr>
            <td>Operand Positions in Statements for Internal Tables</td>
            <td>The following changes have been made: The operands after the addition WITH TABLE KEY of the statements READ TABLE itab and DELETE TABLE itab are now general expression positions. The work area wa of the following statements was changed from functional operand positions to general expression positions: APPEND wa TO ... INSERT wa INTO ... MODIFY ... FROM wa ... The internal table itab specified in the statements READ TABLE itab ... and LOOP AT itab ... is now a functional operand position. In the statement SORT itab, the internal table (otab) can now be specified for dynamic sorts as the result of an expression or functional method call. </td>
        </tr>
<tr>
            <td>Operand Positions for Events in ABAP Objects</td>
            <td>The following changes have been made: The operand positions for formal parameters of the statement RAISE EVENT are now general expression positions. Functions and expressions can now also be passed to event handlers as actual parameters. oref specified after SET HANDLER ... FOR is now a functional operand position. </td>
        </tr>
<tr>
            <td>Operand Positions After MESSAGE</td>
            <td>The following operand positions of the statement MESSAGE were changed: oref is now a functional operand position. text is now a character-like expression position. dobj through dobj4 after WITH are now character-like expression positions or functional operand positions. </td>
        </tr>
<tr>
            <td>Case Sensitivity of Name in ASSIGN COMPONENT</td>
            <td>A component name specified as a character string in ASSIGN COMPONENT no longer has to be in uppercase letters. </td>
        </tr>
<tr>
            <td>ABAP Messaging Channels</td>
            <td>From ABAP release 7.40, SP02, ABAP Messaging Channels (AMC) enable a new type of communication between AS ABAP programs, which goes beyond the limits of an AS instance. </td>
        </tr>
<tr>
            <td>New Variants of WAIT UNTIL</td>
            <td>The new variant WAIT FOR MESSAGING CHANNELS waits for AMC messages in ABAP Messaging Channels (AMC). A further new variant, WAIT FOR ASYNCHRONOUS TASKS, waits for the callback routines of an aRFC and replaces the previous statement WAIT UNTIL (which now only exists in an obsolete short form). </td>
        </tr>
<tr>
            <td>Replacement of the System Class</td>
            <td>The abstract system class CL_ABAP_EXPIMP for data clusters and its subclasses have been replaced by the new system class CL_ABAP_EXPIMP_UTILITIES and its methods. The class CL_ABAP_EXPIMP should no longer be used. </td>
        </tr>
<tr>
            <td>Exception Handling</td>
            <td>The following improvements were made: If an SQL error occurs in the statements EXPORT, IMPORT, or DELETE FROM for data clusters in database tables, the new exception class CX_SY_EXPIMP_DB_SQL_ERROR can be used to handle the error. If the string or internal table is empty in the statements IMPORT FROM DATA BUFFER or IMPORT FROM INTERNAL TABLE, the dedicated runtime errors IMPORT_FROM_DATA_BUFFER_EMPTY or IMPORT_FROM_INTTABLE_EMPTY now occur. </td>
        </tr>
<tr>
            <td>Cleanup in Updates</td>
            <td>When statements are executed that would produce a database commit or database rollback or would disrupt the update controller, the associated system behavior has been cleaned up as described under Forbidden Statements in Updates. The behavior in the case of local and regular updates has been synchronized and all forbidden statements now produce a runtime error directly, which makes troubleshooting easier. The following statements now also raise the exception POSTING_ILLEGAL_STATEMENT directly during the update, since they disrupt the update controller: LEAVE LIST-PROCESSING LEAVE PROGRAM LEAVE SCREEN LEAVE TO LIST-PROCESSING SET SCREEN Before ABAP release 7.40, only the statements CALL DIALOG, CALL SCREEN, CALL SELECTION SCREEN, CALL TRANSACTION, LEAVE TO TRANSACTION, SUBMIT, plus COMMIT WORK and ROLLBACK WORK raised an exception directly. The behavior of messages in updates was not modified. SAP-internal hint Temporarily the NGAP behavior was introduced for messages with SP02 but rolled back with a kernel patch. </td>
        </tr>
<tr>
            <td rowspan="2">NATIVE_SQL</td>
            <td>Bulk Access in ADBC</td>
            <td>The new method SET_PARAM_TABLE of the class CL_SQL_STATEMENT enables bulk access in ADBC (see the executable example). </td>
        </tr>
<tr>
            <td>Recommended Use</td>
            <td>New developments in Native SQL are now only possible in ADBC, which means that ADBC is now recommended in new programs instead of the static embedding of Native SQL. </td>
        </tr>
<tr>
            <td rowspan="4">STRINGS</td>
            <td>New Formatting Option ALPHA</td>
            <td>The new formatting option ALPHA for embedded expressions in string templates inserts leading zeros in front of strings of digits or removes these zeros. </td>
        </tr>
<tr>
            <td>New Formatting Option XSD</td>
            <td>The new formatting option XSD for embedded expressions in string templates creates their asXML format for elementary data types. </td>
        </tr>
<tr>
            <td>Comparisons of String Expressions</td>
            <td>Comparisons of string expressions are now possible when the comparison operator BETWEEN. This was not previously the case. </td>
        </tr>
<tr>
            <td>Assignments of String Expressions</td>
            <td>Exceptions raised in conversions can now be handled when making assignments of string expressions to target fields. This was not previously the case. Exceptions raised (for example, in assignments of non-numeric values to numeric data types or in overflows) can now be handled in the regular way. </td>
        </tr>
<tr>
            <td rowspan="2">TOOLS</td>
            <td>Security Tests in the Extended Program Check</td>
            <td>The new security checks in the extended program check scan ABAP programs for security issues. The security checks perform a static analysis of the ABAP source code and report any potential security risks. The security checks can be executed as follows: Directly from the extended program check in ABAP Workbench From Code Inspector From ABAP Test Cockpit (ATC) If used from ATC, the checks are integrated into the transport release functions and can be executed from the ABAP development tools for Eclipse (ADT). The security checks must be purchased as a separate product. Additional licensing costs are incurred if the security checks are used in customer systems. The program RSLIN_SEC_LICENSE_SETUP can be used to provide the security check option in a system. This requires the authorization modify global check variants in ATC. </td>
        </tr>
<tr>
            <td>Documentation of Security Risks</td>
            <td>Potential security risks in ABAP programs are documented in the ABAP security notes. </td>
        </tr>
<tr>
            <td rowspan="3">XML</td>
            <td>New ST Statement tt:read-write</td>
            <td>The new ST statement tt:read-write is a short form for the statements tt:read and tt:write specified one after the other. </td>
        </tr>
<tr>
            <td>Mapping Rules for tt:value</td>
            <td>In the ST statement tt:value the attribute option="...,..." can be used to specify options for certain data types that override the default mapping of elementary data objects to the asXML format. </td>
        </tr>
<tr>
            <td>Support for JSON</td>
            <td>The format IF_SXML=&gt;CO_XT_JSON has been added to the formats supported by sXML Library. XML readers and XML writers created in this format can be used to process JSON data. A special JSON-XML is used as an intermediate format here. The statement CALL TRANSFORMATION can access these JSON readers and JSON writers and use them as XML sources or XML targets. Furthermore, JSON data can also be specified as an XML source in strings and internal tables. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 710</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="10">MISC</td>
            <td>Lossless Assignments</td>
            <td>The new addition EXACT of the statements MOVE and MOVE-CORRESPONDING enables lossless assignments; if data is lost or values are invalid, an exception is raised. </td>
        </tr>
<tr>
            <td>New RTTC Methods</td>
            <td>The new GET method of classes CL_ABAP_STRUCTDESCR, CL_ABAP_TABLEDESCR, CL_ABAP_REFDESCR, plus GET_BY_NAME of class CL_ABAP_REFDESCR return the type description object specified by the input parameters. A new type description object is created or an existing one is used again. </td>
        </tr>
<tr>
            <td>New API for Creating UUIDs</td>
            <td>The methods of the class CL_SYSTEM_UUID create UUIDs in different formats, such as 16-digit byte-like UUIDs, 22-digit character-like UUIDs with uppercase and lowercase letters, and 32-digit character-like UUIDs in hexadecimal. They also make it possible to convert the UUIDs from one type to another. </td>
        </tr>
<tr>
            <td>New Time Formats</td>
            <td>While number and date formats for output have always been formatted according to the formatting setting of the language environment, until now only the 24-hour format has been available for time output. From SP14 in ABAP release 7.0, four additional 12 hour formats have been introduced. These formats can be selected in the fixed values in the user master record or stored specifically for each country in a new column TIMEFM in the T005X database table. If enough space is available, a time is displayed on the dynpro in 12-hour format when the relevant settings are made in the user master record. In ABAP, new formats can be used by means of the new ENVIRONMENT TIME FORMAT addition of the WRITE TO and WRITE statements as well as by using the environment and COUNTRY formatting options for embedded expressions in string templates. The new class CL_ABAP_TIMEFM contains methods for converting between external and internal times, as well as some auxiliary methods. The predefined time formats of the statements WRITE TO and WRITE cannot be affected automatically due to downward compatibility. Hint The new formats are supported on dynpros and in Screen Painter from EhP2. </td>
        </tr>
<tr>
            <td>String Constants in PXA</td>
            <td>From ABAP release 7.0, EhP2, strings declared as constants are saved globally in PXA in the same way as all constants. This can significantly reduce the amount of memory required. </td>
        </tr>
<tr>
            <td>Class for Generation Limits</td>
            <td>From ABAP release 7.0, EhP2, the static method GET_VALUES of class CL_ABAP_GEN_LIMITS returns the generation limits for an ABAP program. Hint This change was previously introduced in ABAP release 7.0, SP16. </td>
        </tr>
<tr>
            <td>Splitter Control on Dynpros</td>
            <td>From ABAP release 7.0, EhP2, splitter controls can be used on classic dynpros. A splitter control enables two subscreens to be arranged above and below or beside one another in a way that allows the border between the subscreen areas to be adjusted. </td>
        </tr>
<tr>
            <td>Extended Jump Distance</td>
            <td>Prior to ABAP release 7.0, EhP2, errors could occur in ABAP programs in which jump distances were too large. This could also occur, for example, if processing blocks were exited with RETURN, since this statement jumps to the end of the block. From ABAP release 7.0, EhP2, the jump distance is essentially unlimited. Hint The unlimited jump distance should not be intentionally exploited. It enables errors to prevented in large procedures or control blocks, but the maximum recommended number of statements to be used in a procedure must always be adhered to as outlined in the programming guidelines. </td>
        </tr>
<tr>
            <td>Dynamic Programming</td>
            <td>The documented class CL_ABAP_DYN_PRG provides a set of static methods that support error-free, secure dynamic programming. </td>
        </tr>
<tr>
            <td>File Interface</td>
            <td>The documented class CL_FS_PATH provides a set of static methods that support the error-free, secure handling of file names. </td>
        </tr>
<tr>
            <td rowspan="8">TOOLS</td>
            <td>ABAP Editor</td>
            <td>Code Completion has been added to the new ABAP front-end editor. This tool proposes appropriate ABAP words and operands for the current code location. By default, Code Completion is called using Ctrl+Space. The following units can be completed: Data objects including structures and components Data types and classes Components of classes Function modules Subroutines Formal parameters Database tables ABAP words The proposals can be accepted as they are or as patterns with prefilled parameters. </td>
        </tr>
<tr>
            <td>Class Builder</td>
            <td>The &lt;ZK&gt;Source Code-Based Editor&lt;/&gt; setting is used to expand the internal include programs of a class pool. This displays the entire source code of a global class like a program in ABAP Editor and it can then be edited. When saved, the modified source code is then split among the associated include programs. </td>
        </tr>
<tr>
            <td>ABAP Debugger</td>
            <td>The following functions have been added to the two-process debugger: Variable display: A new tab page, Auto, displays the last return values of methods. Long data objects can now be modified in the detail view. Table tool: Support for secondary keys (the key used can be toggled in the debugger). Tables can be filled with data that was previously downloaded onto the presentation server. Substructures of structured line types can be expanded in a line. Columns can be hidden and made visible again. Display settings are retained for specific tables. Breakpoint tool: Special breakpoints for Web Dynpro ABAP (specification of Web Dynpro component, Web Dynpro controller, or a Web Dynpro method). Special breakpoints for Simple Transformations (debugging of Simple Transformations). Special breakpoints for stack changes (breakpoint when switching program or procedure). Special breakpoint for imprecise Decfloat calculation (stops if an imprecise statement or calculation with decimal floating point numbers takes place). Dynpro tool: New tool for analyzing screens (control, properties, layout). ST debugging: New tool for debugging Simple Transformations (step-by-step execution, variables display, breakpoints). Debugger scripting: ABAP debugger scripting can be used to analyze the current ABAP context. Called using a breakpoint or an automated single step. The context of the debuggee can be stopped, traced, or changed within a script. An integrated script tool scripts to be written, saved, loaded, and executed. Transaction SAS allows the independent editing of debugger scripts. Statement debugging: Multiple statements in one line can be debugged step by step. Logical expressions can also be debugged step by step. Layer debugging: The user can define the software layer (package, program, procedure, or interface) that should be debugged. A layer can be executed as a single step in debugging. Stack tool: New setting for displaying the call stack of the internal session of a caller Exception tool: New tool for displaying and analyzing the last and previous class-based exceptions. Console tool: This new tool displays the growing data of serializations from ABAP to XML for CALL TRANSFORMATION and for classic list output. ABAP Enhancement Framework: Visualization of enhancements in the stack. Possibility of skipping or executing enhancement implementations. ABAP byte code: In the ABAP byte code or ABAP byte code (debug macro) context menus, it is possible to select what a replacement for a proper macro debugging run can depict. Functions taken from the old debugger: Displays the classic list currently being created in the ABAP session of the debuggee. Possibility of restarting the entire application. Settings (block tRFC sending, ESI debugging, shared objects: debug automatic area build, always create exception object, Control Framework: automation controller always processes requests synchronously, check sorting before READ BINARY SEARCH). </td>
        </tr>
<tr>
            <td>Coverage Analyzer</td>
            <td>Coverage Analyzer has been modified to include the following: Code coverage is now measured on the statement level. The coverage of individual conditions of logical expressions is measured. The coverage of empty branches is measured. The coverage of the executed and non-executed statement blocks in control structures is measured (branch coverage). Code coverage is visualized in different colors in the new ABAP front-end editor. To measure the coverage of test runs, Coverage Analyzer has been integrated into ABAP Unit. </td>
        </tr>
<tr>
            <td>ABAP Unit</td>
            <td>In ABAP release 7.0, EhP2, the additions introduced for the statement CLASS ... FOR TESTING, namely RISK LEVEL and DURATION, have replaced the pseudo comments introduced in ABAP release 7.0, namely "#AU Risk_Level ... and "#AU Duration ..., used for defining the test properties of test classes. An ABAP Unit Browser integrated into Object Navigator in ABAP Workbench allows: A structured overview of existing unit tests to be displayed. Multiple test runs to be started at the same time. Unit tests to be organized in favorites. For the latter item, test coverage can be measured and displayed using Coverage Analyzer. Class CL_ABAP_UNIT_ASSERT replaces class CL_AUNIT_ASSERT. New methods are now assigned to the new class only. Existing unit tests do not have to be converted to the new class. It is recommended, however, that the new class is used only in new tests. </td>
        </tr>
<tr>
            <td>Runtime Analysis</td>
            <td>The runtime analysis tool has been switched to the new transaction SAT (the previous transactions were SE30 and ATRA). The new runtime analysis tool has a new user interface for analyzing measurements. Like ABAP Debugger, it consists of adjustable desktops where various tools can be organized. contains additional and more flexible analysis tools than the previous version. saves the measurement data to the database, which makes it independent of the operating system of the host computer of the AS instance. enables the cross-system comparison of measurement data. </td>
        </tr>
<tr>
            <td>ABAP Test Cockpit</td>
            <td>ABAP Test Cockpit (ATC) is a framework that is integrated into ABAP Workbench to execute various tests for development objects and to show the results of these. The following are some of the tests that are currently integrated: Extended program checks Performance tests Unit tests Usability tests Package checks The external release takes place in support package 12. </td>
        </tr>
<tr>
            <td>Checkpoint Groups</td>
            <td>A time limit now applies to activation settings for checkpoint groups. When an activation setting is saved in transaction SAAB, either a validity period (valid for the current day or week) or a validity end date must be specified. The default value (valid for the current day) is generally suitable for most applications. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 71</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_OBJECTS</td>
            <td>Declaring Instance Constructors</td>
            <td>From ABAP release 7.0, EhP2, the statement METHODS can be used to declare an instance constructor in all visibility sections, whose instantiability is more general than or equal to that specified in the addition CREATE of the statement CLASS DEFINITION. Until now, this was only possible with the public visibility section (this modification was also transported to ABAP release 7.0). </td>
        </tr>
<tr>
            <td>C Destructor</td>
            <td>From ABAP release 7.0, EhP2, more than one attribute can be passed in the C destructor when SYSTEM-CALL is used. </td>
        </tr>
<tr>
            <td>Constants in Class Pools</td>
            <td>From ABAP release 7.0, EhP2, local program constants can be declared in a class pool. The private components and methods of the global class can access these constants. </td>
        </tr>
<tr>
            <td rowspan="8">ABAP_SQL</td>
            <td>UPDATE for Tables with Strings</td>
            <td>When the statement UPDATE is used for a column of type STRING or RAWSTRING with the addition SET, it is no longer necessary to specify the primary key in full in the WHERE condition. </td>
        </tr>
<tr>
            <td>Table Buffering with Single Record Access</td>
            <td>Access to a table with single record buffering that an equality condition is set for in the WHERE clause for all key fields of the primary key uses table buffering, even if the addition SINGLE is not specified for SELECT. Before ABAP release 7.0 EhP2, table buffering was bypassed if SINGLE was missing. </td>
        </tr>
<tr>
            <td>Short Strings as Key Fields of Database Tables</td>
            <td>From ABAP release 7.0, EhP2, short strings can be used as key fields of the type SSTRING in database tables. This can produce significant memory and performance gains in comparison with using long fields of type CHAR. </td>
        </tr>
<tr>
            <td>Maximum Length of Short Strings in ABAP Dictionary</td>
            <td>The maximum length of short strings of type SSTRING has been increased from 255 to 1333. </td>
        </tr>
<tr>
            <td>ABAP Database Connectivity (ADBC)</td>
            <td>The class-based framework ABAP Database Connectivity (ADBC), which has existed since ABAP release 6.10 for dynamic access to the Native SQL interface, is now also documented in the ABAP keyword documentation under ADBC. </td>
        </tr>
<tr>
            <td>Streaming and Locators in ABAP SQL</td>
            <td>From ABAP release 7.0, EhP2, ABAP SQL supports processing of LOBs using data streams and locators. A new set of classes and interfaces was introduced. See ABAP SQL - Streaming and Locators In reading and changing ABAP SQL statements, data streams and locators can be linked to LOBs in database tables by using special reference variables. See SELECT - LOB Handles and INSERT, UPDATE, MODIFY dbtab|view - LOB Handles Using the statements TYPES, DATA, and CLASS-DATA, it is possible to derive special LOB handle structures for use in ABAP SQL. See TYPES ... FOR ... COLUMNS ... The data streams for ABAP SQL are embedded in a broader streaming framework. Further data streams and filter streams will be added to this in future releases. See Streaming. </td>
        </tr>
<tr>
            <td>Specifying Sorting Columns in ABAP SQL</td>
            <td>The restriction that after ORDER BY in SELECT only columns can be specified that are also specified after SELECT does not apply. This also applies to the addition PRIMARY KEY. Previously, all columns of the primary key had to be in the SELECT list, but this is no longer the case. </td>
        </tr>
<tr>
            <td>Overflow Handling for sy-dbcnt</td>
            <td>If an overflow occurs in the system field sy-dbcnt, meaning the number of statements processed by SQL access is greater than 2,147,483,647, the value of sy-dbcnt is set to -1. The value was previously undefined. </td>
        </tr>
<tr>
            <td rowspan="9">DOCU</td>
            <td>ABAP - Examples</td>
            <td>The ABAP example library has been integrated into the ABAP keyword documentation. Executable example programs that until now have been called using transaction ABAPDOCU and are still valid, are now in the relevant topic of the tree structure of the ABAP keyword documentation and can be executed, debugged, or displayed in the corresponding editor from there. An alphabetical list of all the example programs can be found under ABAP examples. The example programs are taken into account for index searches of the ABAP keyword documentation. The ABAPDOCU transaction now displays the ABAP keyword documentation. The ABAP keyword documentation search options now include an option to search within source code in example programs. </td>
        </tr>
<tr>
            <td>Searching in Documentation</td>
            <td>The access to the search options in ABAP keyword documentation has been improved: An input window is permanently shown in the documentation display. This window replaces the toolbar functions used previously. If Quick Search is chosen (or Enter), an index search is performed first, followed a full text search for the term entered. If Extended Search is chosen, a dialog box appears where a search mode can be selected. The source code of ABAP examples can now also be searched, as well as to using the existing index and full-text searches to search documentation text. The display window of the search hit list now contains a function for opening the dialog box. This dialog box allows you to start a new search in the hit list, without needing to return to the previous window. </td>
        </tr>
<tr>
            <td>Syntax Diagrams</td>
            <td>The syntax diagrams for the ABAP keywords are now displayed in a completely new way. Selecting a language element in a syntax diagram displays the corresponding documentation. If a syntax diagram can be displayed for a keyword, a selectable icon is displayed in the title. Hint Support for interactive syntax diagrams is stopped in ABAP release 7.65. </td>
        </tr>
<tr>
            <td>Downloading the Documentation</td>
            <td>The existing option for creating an offline version of the ABAP keyword documentation has been expanded as of SP02 to include the option of downloading nodes with subnodes. With the previous offline version, all the documents of the ABAP keyword documentation are exported to individual HTML files that are managed by a separate tree display. When you download nodes with subnodes, the currently selected node, including subnodes, is exported into a single HTML file. Additional files are created only for possible displays. All the links work internally. If you have more than one node, an index is inserted at the beginning. By selecting the uppermost node, it is possible to download the entire ABAP keyword documentation into a file. With both options, you can select whether you wish to include the interactive examples in downloading. When downloading to individual HTML files, you can select whether the new syntax diagrams should be taken into account or not. At selection, offline versions of the syntax diagrams are loaded into a subdirectory. These can then be selected using an icon as in the online version. </td>
        </tr>
<tr>
            <td>Standalone HTML Versions of the Documentation</td>
            <td>From ABAP release 7.0, EhP2, AS ABAP provides the following services to allow the ABAP keyword documentation to be used independently of SAP GUI: The remote enabled function module ABAP_DOCU_GET_HTML returns documents of the ABAP keyword documentation as HTML files. You can either pass search terms to the parameter QUERY or the names of documents to the parameter OBJECT; passing * to OBJECT leads to the sending of the tree-like documentation structure. Example: Refer to the program ABAP_DOCU_REMOTE_CALL. The loaded files are displayed here using the CL_ABAP_BROWSER class. Under the node /sap/public/bc/abap/docu in transaction SICF, a service is provided to call the ABAP keyword documentation from the Web. On the initial screen, the tree structure of the documentation can be displayed or searches performed. The services /sap/public/bc/abap/docu and /sap/bc/abap/docu must be activated in transaction SICF. This version can also be used as a provider for OpenSearch. Example: Refer to the program ABAP_DOCU_HTTP_CALL. </td>
        </tr>
<tr>
            <td>Hit List of F1 Help in the ABAP Editor</td>
            <td>When the F1 help for keywords is called, the hit list that uses the new ABAP Front-End Editor is more specific than the one that uses the old ABAP Editor. After selecting an operand, the search in the new ABAP Editor evaluates the adjacent ABAP words. As of ABAP release 7.0, EhP2, the selection of all operands produces a hit list. Previously, selecting =, +, and so on, displayed a dialog box with an input field. The latter also applies to the old ABAP Editor. </td>
        </tr>
<tr>
            <td>Printing Documentation</td>
            <td>As of ABAP release 7.0, EhP2, all documents for printing are formatted in HTML. The individual executable example programs from ABAP examples and the ABAP glossary can now also be printed. In addition, tabular displays are now aligned correctly on printouts. </td>
        </tr>
<tr>
            <td>Display Languages</td>
            <td>From ABAP release 7.0, EhP2, the language in which the documentation is displayed (English or German) can be configured by user, independently of the logon language. Any German developers developing in English, for example, can now display the most up-to-date documentation in its original language, German. This user-specific setting is made when the documentation is displayed using the Settings for Display Window function, or in the initial window of the transaction ABAPHELP; the system remembers this setting for each user. Until now, the language setting in the initial screen of ABAPHELP only applied to what you were currently viewing. </td>
        </tr>
<tr>
            <td>Internal Performance Improvement</td>
            <td>As of ABAP release 7.0, EhP2, large and unchangeable datasets of the ABAP keyword documentation, such as the ABAP Index or the ABAP Glossary, are downloaded into the shared memory. Here they are available to all users of the documentation. This leads to improvements in performance with regard to memory use and also improvements in downloading the documentation. </td>
        </tr>
<tr>
            <td rowspan="3">EXCEPTIONS</td>
            <td>Resumable Exceptions</td>
            <td>Before ABAP release 7.0, EhP2, the context in which a class-based exception was raised was always emptied completely. All procedures called between the raising of the exception and any handler, and their local data, were deleted before the handler was executed. The program could only resume after the TRY control structure of the handler. From ABAP release 7.0, EhP2, class-based exceptions can be raised and propagated as resumable exceptions. The new addition RESUMABLE can be used in the statement RAISE EXCEPTION the RAISING addition for declaring exceptions in [CLASS-]METHODS, FUNCTION, and FORM. The new statement RESUME is used to resume the execution of a program after the exception-raising statement, while the resumable exception is being handled. </td>
        </tr>
<tr>
            <td>Preserving the Context of an Exception</td>
            <td>The context of the exception must be retained while it is being handled so that the program can resume afterwards. To enable this, the new addition BEFORE UNWIND of the statement CATCH has been implemented. This addition can also be used independently of resumable exceptions. </td>
        </tr>
<tr>
            <td>Retrying the TRY Block</td>
            <td>The new statement RETRY enables an exception-raising TRY block to be retried. </td>
        </tr>
<tr>
            <td rowspan="7">EXPRESSIONS</td>
            <td>Using Functions and Calculation Expressions</td>
            <td>The usability of functions and expressions in operand positions has been substantially improved in ABAP release 7.0, EhP2: Calculation expressions, built-in functions, and functional methods can be used in general expression positions. Numeric expressions can be used in numeric expression positions. String expressions can be used in character-like expression positions. Functional methods can be used in functional operand positions. From ABAP release 7.0, EhP2, the following general expression positions are available: Operands of relational expressions Example: a + b &lt; oref-&gt;meth( ) Actual parameters for input parameters of methods Example: oref1-&gt;meth1( oref2-&gt;meth2( ... ) ) Before ABAP release 7.0, EhP2, operand positions on the right side of assignments with = were the only general expression positions. Hint From EhP1, the character-like arguments of description functions are (with one exception) character-like expression positions. Before EhP1, they were character-like functional operand positions. </td>
        </tr>
<tr>
            <td>Method Chainings</td>
            <td>As well as the previous chained names, operand positions that accept functional methods now also accept method chainings, that is chained method calls and chained attributes accesses. </td>
        </tr>
<tr>
            <td>Access to Components of Structured Return Values</td>
            <td>If the result of a functional method or method chaining is structured, the call can (like the name of a structure) be written directly in front of the structure component selector to access a component of the result. </td>
        </tr>
<tr>
            <td>Built-In Functions with Multiple Parameters</td>
            <td>ABAP release 7.0, EhP2 introduces new built-in functions that can now accept multiple arguments. A built-in function of this type is called in the same way as a functional method with multiple input parameters. ... func( p1 = arg1 p2 = arg2 ... ) ... The new functions are include: Rounding functions Search, similarity, and processing functions for character strings Logical functions </td>
        </tr>
<tr>
            <td>Boolean Functions</td>
            <td>The new built-in Boolean functions boolc and boolx accept logical expressions as arguments and return their truth values in the form of character strings or byte strings. </td>
        </tr>
<tr>
            <td>Predicate Functions</td>
            <td>The new predicate functions return truth values and can be used like relational expressions. Predicate functions for character-like arguments were introduced in ABAP release 7.0, EhP2. </td>
        </tr>
<tr>
            <td>New Boolean Operator EQUIV</td>
            <td>The new Boolean operator EQUIV enables an equivalence relationship between two logical expressions. </td>
        </tr>
<tr>
            <td rowspan="5">ITAB</td>
            <td>Dynamic WHERE Condition</td>
            <td>From ABAP release 7.0, EhP2, the statements LOOP AT itab, MODIFY itab, and DELETE itab make it possible to specify the WHERE condition in a cond_syntax data object dynamically. </td>
        </tr>
<tr>
            <td>Definition of Secondary Table Keys</td>
            <td>Previously, each internal table had just one table key. Any search key could be entered when accessing internal tables, but this was not very efficient. Also, standard tables were always searched linearly during key access. To be able to efficiently access an internal table with different keys, and to also allow efficient key access to standard tables, secondary table keys were introduced. From ABAP release 7.0, EhP2, secondary table keys can be defined for internal tables with TYPES and DATA as well as in ABAP Dictionary. An internal table can have up to 15 secondary table keys with different names. At the same time, what was previously the table key became the primary table key, and a predefined name for it, primary_key, was introduced. This can be replaced with an alias name in the enhanced definition of the primary table key in ABAP release 7.0, EhP2. Secondary table keys can be hash keys or sorted keys. A secondary table index is created for each sorted secondary key of an internal table. The previous table index, which exists only for index tables, becomes the primary table index. Secondary table keys are part of the technical type properties of an internal table. Secondary keys can be specified generically for standalone table types. </td>
        </tr>
<tr>
            <td>Using Secondary Keys</td>
            <td>The following additions have been introduced for statements that access lines of internal tables: WITH [TABLE] KEY keyname COMPONENTS ... USING KEY keyname keyname can be used to specify the name of the key to be used statically or dynamically. At the same time, statements that previously only accessed the primary key have been enhanced so that access to secondary keys is also possible. The table index used can now also be specified explicitly using a table key when indexes are specified. The system field sy-tabix is now filled with reference to the table index used. It is set to 0 for access using a hash key. The enhanced statements are: READ TABLE itab The lines to be read can be specified using a secondary key. LOOP AT itab The processing sequence and conditions can be controlled using a secondary table key. INSERT itab Only a secondary key for the source table can be specified here, from which multiple lines are copied. The position they are inserted at is determined solely using the primary key and the primary index. APPEND Only a secondary key for the source table can be specified here, onto which multiple lines are appended. MODIFY itab The lines to be modified can be specified using a secondary key. DELETE itab The lines to be deleted can be specified using a secondary key. In statements where these additions have not been introduced, such as SORT, COLLECT, or PROVIDE, secondary keys are not explicitly supported. </td>
        </tr>
<tr>
            <td>Updating Secondary Keys</td>
            <td>In all statements that change the content or structure of an internal table, the internal administration of the secondary table key (hash administration or secondary table index) is updated automatically as follows: In inserting operations, such as INSERT or APPEND, and in change operations using MODIFY, the secondary table key administration of unique keys is updated immediately (direct update), while for non-unique table keys it is updated upon the next explicit use of the secondary table key (lazy update). If an update infringes the uniqueness of a secondary key, an exception is raised immediately. For block operations, such as statements between internal tables, or when internal tables are filled using SELECT, the behavior is the same as with inserting operations. When individual lines are modified using field symbols or data references that point to table lines, the secondary key administration of unique keys is updated upon the next access to the internal table (delayed update), and the secondary key administration of non-unique keys is updated upon the next explicit use of the secondary table key (lazy update). A uniqueness check is also run when the update is made. An internal table might therefore be in an inconsistent state with respect to the secondary key after individual lines are modified. An exception is not raised until the table is next used. Class CL_ABAP_ITAB_UTILITIES contains methods that can be used to update single secondary keys or all secondary keys for an internal table in exceptional situations. </td>
        </tr>
<tr>
            <td>Streaming for Internal Tables</td>
            <td>The new streaming concept supports internal tables. </td>
        </tr>
<tr>
            <td rowspan="1">LISTS</td>
            <td>Support for the New 12 Hour Format</td>
            <td>The new addition ENVIRONMENT TIME FORMAT to the WRITE TO and WRITE statements supports the new 12 hour format. The predefined time formats of the statements WRITE TO and WRITE cannot be affected automatically due to downward compatibility. Hint This modification was also added to ABAP release 7.0 in SP14. </td>
        </tr>
<tr>
            <td rowspan="12">MISC</td>
            <td>Structured Types with Static Boxes</td>
            <td>Within a structure definition using TYPES BEGIN OF, the addition BOXED in TYPES can be used to create a substructure as a static box. </td>
        </tr>
<tr>
            <td>Attributes of Classes as Static Boxes</td>
            <td>Within a class declaration, the BOXED addition of DATA can be used to create a structured attribute as a static box. </td>
        </tr>
<tr>
            <td>Enhancements to RTTS for Static Boxes</td>
            <td>The class CL_ABAP_TYPEDESCR contains the new constant TYPEKIND_BREF for static boxes. The value of these constants is specified as the type of a static box in the component table COMPONENTS of the class CL_ABAP_STRUCTDESCR. The return value of the method GET_COMPONENTS of the class CL_ABAP_STRUCTDESCR contains boxed components and reference variables as type description objects of the class CL_ABAP_REFDESCR. The method GET_REFERENCED_TYPE of this class gets a type description object for the substructure. A type description object of the class CL_ABAP_REFDESCR, which describes a boxed component, cannot be used in the statements CREATE DATA or ASSIGN CASTING. </td>
        </tr>
<tr>
            <td>New Built-In ABAP Types decfloat16 and decfloat34</td>
            <td>From ABAP release 7.0, EhP2, ABAP includes the new built-in numeric ABAP types decfloat16 and decfloat34 for decimal floating point numbers. The corresponding data objects are eight bytes or 16 bytes long and the data objects are aligned in their lengths. The value range is determined by mantissas of the length -383 and +384 or -6143 and +6144. The new generic ABAP type decfloat covers both new types, decfloat16 and decfloat34. The following changes have been caused by the new types: New conversion and comparison rules have been introduced for the new types. If a decimal floating point number appears in an arithmetic expression, the calculation type is decfloat34. The floating point functions exp, log, log10, and sqrt now no longer work just with arguments of type f; they now also work with arguments of type decfloat16 and decfloat34. The type of the argument determines the type of the return value. Decimal floating point numbers cannot be used as arguments for the remaining floating point functions at the moment. </td>
        </tr>
<tr>
            <td>New Built-In Types in ABAP Dictionary</td>
            <td>The following new types have been introduced in ABAP Dictionary for the new ABAP types decfloat16 and decfloat34: DF16_DEC and DF34_DEC are used by database fields in which decimal floating point numbers, such as packed numbers with type DEC are stored. Database writes can cause roundings and overflows. DF16_RAW and DF34_RAW are used by database fields in which decimal floating point numbers are stored in their internal representation. DF16_SCL and DF34_SCL are used by database fields in which decimal floating point numbers are stored with their scaling. In this kind of field, the decimal floating point numbers are stored as in DF16_RAW or DF34_RAW. The scaling must be specified in a direct successor database field with type INT2. When a decimal floating point number is written, this field is filled automatically with the scaling and the scaling is taken from it in reads. These data types are now obsolete and their use is strongly discouraged. Data elements with these types can be used to declare fields in database tables and in dynpros. An output style can be specified in a domain or directly in a structure component created with one of these types. For dynpro fields whose data type is not defined in ABAP Dictionary, the output style can be defined in Screen Painter. </td>
        </tr>
<tr>
            <td>New Built-In Functions round and rescale</td>
            <td>The new rounding functions round and rescale enable decimal floating point numbers to be rounded or their scaling to be changed. New constants have been added to the class CL_ABAP_MATH. These constants can be used to specify the rounding rule. </td>
        </tr>
<tr>
            <td>Lossless Calculations</td>
            <td>The new addition EXACT of the statement COMPUTE can be used to force a lossless calculation for decimal floating point numbers, under certain prerequisites. No roundings are allowed in a lossless calculation; they raise the exception CX_SY_CONVERSION_ROUNDING. </td>
        </tr>
<tr>
            <td>Methods for Decimal Floating Point Numbers</td>
            <td>Methods for operations with floating point numbers have been added to the class CL_ABAP_MATH. The method GET_SCALE gets the scaling of a decimal floating point number. The method GET_NUMBER_OF_DIGITS gets the precision of a decimal floating point number. The method NORMALIZE gets a normalized floating point number. This means that the scaling and precision of an input value are changed so that the mantissa has no trailing zeros. The method GET_MAX_DB_VALUE returns the maximum value of a number of the type DF16_DEC or DF34_DEC on the database (from EhP1). The method GET_DB_LENGTH_DECS returns the length and number of decimal places of a number of the type DF16_DEC or DF34_DEC on the database (from EhP1). The new class CL_ABAP_DECFLOAT contains special methods for decimal floating point numbers. The methods READ_DECFLOAT34 and READ_DECFLOAT16 convert character-like data objects to decimal floating point numbers and, unlike regular assignments, enable enhanced exception handling. </td>
        </tr>
<tr>
            <td>Formatting of Decimal Floating Point Numbers Using WRITE</td>
            <td>Predefined formats have been defined for the new types decfloat16 and decfloat34 for the statements WRITE ... TO for formatted assignments and WRITE for list output. The predefined output lengths for list output are 24 and 46. The new addition STYLE has been added to the WRITE statements for the formatting of decimal floating point numbers with different formats. </td>
        </tr>
<tr>
            <td>Exception Handling in CALL BADI</td>
            <td>Until now, the CX_SY_DYN_CALL_ILLEGAL_METHOD exception was raised when the implementation of a method in CALL BADI was missing. The exception is now caught internally and the call is executed as if the method were present with an empty implementation. Actual parameters that are bound to EXPORTING or RETURNING parameters passed by value are initialized. All other actual parameters remain unchanged. Hint This change has also been downported to ABAP release 7.0. </td>
        </tr>
<tr>
            <td>Dynamic GET BADI and CALL BADI</td>
            <td>Dynamic variants have been added to the GET BADI and CALL BADI statements, which allow the BAdI or BAdI method to be specified dynamically. </td>
        </tr>
<tr>
            <td>Nested Source Code Enhancements</td>
            <td>From ABAP release 7.0 EhP2, it is possible to enhance a source code plug-in defined between ENHANCEMENT - ENDENHANCEMENT with additional source code plug-ins. This means the ENHANCEMENT-POINT and ENHANCEMENT-SECTION statements can be used in a source code plug-in. In addition, implicit enhancement options are now available before the first line and after the last line of a source code plug-in (after ENHANCEMENT and before ENDENHANCEMENT). </td>
        </tr>
<tr>
            <td rowspan="2">PRAGMAS</td>
            <td>Introduction of Pragmas</td>
            <td>From ABAP release 7.0, EhP2, pragmas can be used to hide warnings from the ABAP Compiler syntax check and other check tools. </td>
        </tr>
<tr>
            <td>Switching from Pseudo Comments</td>
            <td>The pseudo comments "#EC ..., which until now could be used to hide warnings from enhanced program checks, were to be replaced by the ##... pragmas. The statement SET EXTENDED CHECK must no longer be used in programs in which a warning from the extended program check is hidden by a pragma. If used, the statement produces a warning that cannot be hidden. </td>
        </tr>
<tr>
            <td rowspan="3">PROGRAM_LOAD</td>
            <td>Statement TYPE-POOLS is Obsolete</td>
            <td>The statement TYPE-POOLS is no longer required for the use of a data type, a constant, or a macro from a type pool. The elements of a type pool can now be addressed in the same way as all other objects in ABAP Dictionary without previously loading the type pool. TYPE-POOLS statements are ignored by ABAP Compiler from ABAP release 7.0, EhP2, and can be deleted. In list processing in particular, the include programs &lt;LIST&gt;, &lt;SYMBOL&gt;, &lt;ICON&gt;, &lt;LINE&gt;, and &lt;COLOR&gt; are no longer needed, since they only contain TYPE-POOLS statements. </td>
        </tr>
<tr>
            <td>Addition LOAD for CLASS and INTERFACE is Obsolete</td>
            <td>The now rarely used statements CLASS ... DEFINITION LOAD. INTERFACE ... LOAD. are no longer required From ABAP release 7.0, EhP2 these statements are ignored by ABAP Compiler and can be deleted. </td>
        </tr>
<tr>
            <td>Addition PUBLIC for CLASS, INTERFACE DEFINITION DEFERRED is Obsolete</td>
            <td>The statements CLASS ... DEFERRED PUBLIC. INTERFACE ... DEFERRED PUBLIC. are no longer required </td>
        </tr>
<tr>
            <td rowspan="6">RFC</td>
            <td>Background RFC</td>
            <td>With the new Background RFC (bgRFC) Remote Function Calls can be carried out safely in a transaction and in the call sequence. The calling application and the called application are connected asynchronously. bgRFC replaces the functions previously provided by tRFC and qRFC (but not qRFC no-Send). Hint This modification was also added to ABAP release 7.0 in SP14. </td>
        </tr>
<tr>
            <td>Local Data Queue</td>
            <td>The Local Data Queue (LDQ) is a persistence layer into which data can be placed. This data can only be read in the order that it is recorded in. This involves separation of data recording from data retrieval by the receiver like in an inbox. Since the access order is defined according to the First-In First-Out (FIFO) principle, queues are used as the organizational element for the individual receivers. The LDQ replaces the functions previously provided by the qRFC No-Send scenario. </td>
        </tr>
<tr>
            <td>SAP RFC SDK</td>
            <td>The new SAP RFC SDK replaces the classic RFC SDK. It supports a revised API that can be used like the SAP Java Connector. </td>
        </tr>
<tr>
            <td>RFC Connectivity in SAP Java Enterprise Edition (Java EE)</td>
            <td>For the SAP Java Enterprise Edition (Java EE), the SAP Java Resource Adapter (SAP JRA) for SAP JCo is provided. It provides connections to resources in accordance with JCA 1.5 SAP JCo. </td>
        </tr>
<tr>
            <td>basXML as New RFC Protocol</td>
            <td>The basXML format is supported as the new standard RFC protocol and replaces the previous formats. </td>
        </tr>
<tr>
            <td>Identification of Trusted Systems</td>
            <td>In transaction SMT1 for editing RFC trust relationships, the installation number is now also used when creating trusted systems by entering the system ID (name of the AS ABAP). This applies to all trusted systems from ABAP releases 7.02, EhP2 and 7.2 (including 7.1). When creating a trusted relationship from ABAP release 7.0, EhP2, the trusting system checks the installation number of the calling system, if it is specified in transaction SMT1. For trust relationships of this kind, the relevant authorizations need to be modified so that field RFC_INFO in authorization object S_RFCACL contains the installation number of the calling system. </td>
        </tr>
<tr>
            <td rowspan="5">SHARED_OBJECTS</td>
            <td>Handling of Memory Bottlenecks</td>
            <td>From ABAP release 7.0, EhP2, memory bottlenecks in the shared objects memory raise catchable exceptions of the class CX_SHM_OUT_OF_MEMORY. Previously, uncatchable runtime errors were raised. </td>
        </tr>
<tr>
            <td>Data References in the Shared Objects Memory</td>
            <td>From ABAP release 7.0, EhP2, anonymous data objects in the shared objects memory can also be created with direct reference to data elements and table types of ABAP Dictionary using the addition AREA HANDLE of the statement CREATE DATA. </td>
        </tr>
<tr>
            <td>Area Binding</td>
            <td>From ABAP release 7.0, EhP2, the lifetime and visibility of area instances can be bound not only to the current AS instance but also to the following: User sessions Top level transactions </td>
        </tr>
<tr>
            <td>Methods PROPAGATE_AREA and PROPAGATE_INSTANCE Replaced</td>
            <td>The methods PROPAGATE_AREA and PROPAGATE_INSTANCE of an area class for transactional areas may no longer be used. Instead, the parameter AFFECT_SERVER of the methods FREE_AREA, FREE_INSTANCE, INVALIDATE_AREA, and INVALIDATE_INSTANCE can be used for areas of this type with the Application Server area binding. </td>
        </tr>
<tr>
            <td>Service Class CL_SHM_UTILITIES</td>
            <td>The class CL_SHM_UTILITIES is used to query the attributes of area instances in programs. </td>
        </tr>
<tr>
            <td rowspan="9">STRINGS</td>
            <td>String Expressions</td>
            <td>The new string expressions enhance the previous calculation expressions and enable character string processing at specific operand positions. A string expression is either a string template or a concatenation with operator &amp;&amp;. </td>
        </tr>
<tr>
            <td>String Templates</td>
            <td>String templates have been introduced as a new way of creating character strings. A string template is enclosed between two | characters and creates a character string in a string expression from a literal text, embedded expressions, and control characters. String templates replace the WRITE TO statement to a large extent. </td>
        </tr>
<tr>
            <td>Concatenation Operator</td>
            <td>The concatenation operator &amp;&amp; is a new string operator that concatenates together two character-like operands as one operand in a string expression. The concatenation operator replaces the CONCATENATE statement to a large extent. </td>
        </tr>
<tr>
            <td>String Functions</td>
            <td>String functions have been introduced as a new type of embedded function. There are Search functions a similarity function Processing functions The Boolean function boolc can also be considered a string function. The string functions enable many character string processing tasks to be performed in operand positions where separate statements and helper variables were required previously. </td>
        </tr>
<tr>
            <td>Bit Functions</td>
            <td>Bit functions have been introduced as a new type of embedded function. A bit function (bit-set) currently exists for setting bits. The Boolean function boolx can also be considered a bit function. </td>
        </tr>
<tr>
            <td>Management of Short Strings</td>
            <td>The internal management of short stings has been optimized to reduce the memory overhead that accumulates when short strings are managed for the relevant string header. For string lengths of less than 30 characters or 60 bytes, the string header now only requires between 10 and 40 bytes. For longer strings, this remains at approximately 50 bytes. Before ABAP release 7.0, EhP2, the overhead of the string header was not related to the length of the string and was approximately 60 bytes for each string. Strings are recommended instead of data objects for all character string and byte string operations where a fixed length is not important. Hint This change is also available before EhP2 if a kernel greater than ABAP release 7.0 is used. </td>
        </tr>
<tr>
            <td>Streaming for Strings</td>
            <td>The new streaming concept supports strings. </td>
        </tr>
<tr>
            <td>Any Start Values for Data Objects of Type xstring</td>
            <td>From ABAP release 7.0, EhP2, any suitable start value val can be specified for xstring after the addition VALUE for statements DATA, CONSTANTS and so on. Previously, only IS INITIAL was possible. </td>
        </tr>
<tr>
            <td>Maximum Length of Data Objects of Types c, n, and x</td>
            <td>The maximum length of data objects of types c and n has been increased from 65535 to 262143. The maximum length of data objects of type x has been increased from 65535 to 524287. </td>
        </tr>
<tr>
            <td rowspan="9">XML</td>
            <td>Access to ABAP Objects from Simple Transformations</td>
            <td>The following new ST statements can be used to call methods of global classes and create objects of these classes in ST programs: tt:call-method calls static methods or instance methods. tt:create-object creates an instance of a global class. For addressing objects, a new addition tt:ref-type of the statements tt:parameter and tt:variable has been introduced that makes it possible to create parameters and variables from ST programs explicitly as reference variables. Another new statement tt:cast also enables a downcast of reference variables in ST programs. See the ST Statement Overview. </td>
        </tr>
<tr>
            <td>Mapping of XML Schema Data Types</td>
            <td>Previously, asXML allowed only mappings of the elementary built-in ABAP types to XML schema data types and back. But this did not cover all XML schema data types. Special DDIC domains named XSD... have been introduced that allow mapping of further XML schema data types. Hints This change was introduced in ABAP release 7.0, SP14. The domain XSDQNAME is available from ABAP release 7.0, EhP1. </td>
        </tr>
<tr>
            <td>Lengths Specified in Simple Transformations</td>
            <td>In the ST statements tt:value, tt:write and tt:read can be used to make length specifications in order to provide a minimum length for serialization of character and type strings and a maximum length for deserialization. </td>
        </tr>
<tr>
            <td>Validation of Values in Simple Transformations</td>
            <td>An XML schema type and further restrictions can now be specified in the ST statements tt:value, tt:write, and tt:read, to validate a value with respect to a value range. </td>
        </tr>
<tr>
            <td>New Values for Transformation Option value_handling</td>
            <td>The new values accept_decimal_loss and reject_illegal_characters can be selected for the transformation option value_handling of the statement CALL TRANSFORMATION. </td>
        </tr>
<tr>
            <td>New Values for Transformation Option data_refs</td>
            <td>The new values heap-or-error and heap-or-create can be selected for the transformation option data_refs of the statement CALL TRANSFORMATION, to control the handling of stack references. Hint This change was transported back as far as ABAP release 6.20 (in ABAP release 7.0, from SP6). </td>
        </tr>
<tr>
            <td>New Value for Transformation Option initial_components</td>
            <td>The new value suppress_boxed can be selected for the transformation option initial_components of the statement CALL TRANSFORMATION, to control the handling of boxed components. The new value is also the new default setting. </td>
        </tr>
<tr>
            <td>New Transformation Option technical_types</td>
            <td>error or ignore can be selected for the new transformation option technical_types of the statement CALL TRANSFORMATION, to control the handling of data reference variables with unknown dynamic types. Hint This change was transported back as far as ABAP release 6.20 (in ABAP release 7.0, from SP6). </td>
        </tr>
<tr>
            <td>New Transformation Option clear</td>
            <td>all, supplied, or none can be selected for the new transformation option clear of the statement CALL TRANSFORMATION, to control the initialization of the ABAP target fields in deserializations. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 703</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="1">ABAP_OBJECTS</td>
            <td>Chained Method Call</td>
            <td>From ABAP release 7.31, the chained method call cannot only be specified in functions but also executed in operations. </td>
        </tr>
<tr>
            <td rowspan="1">MISC</td>
            <td>ST Documentation Moved</td>
            <td>The documentation about Simple Transformations was integrated into the ABAP keyword documentation. </td>
        </tr>
<tr>
            <td rowspan="2">SELECTION_SCREENS</td>
            <td>Extended Field Lengths on Selection Screens</td>
            <td>The maximum length of input fields on selection screens has been increased from 132 to 255 for parameters and from 45 to 255 for selection criteria. To allow values to be passed using SUBMIT WITH SELECTION TABLE, a new data type RSPARAMSL_255, which supports this maximum length, has been added to ABAP Dictionary. </td>
        </tr>
<tr>
            <td>Handling Spool Lists</td>
            <td>If a spool list level stacked in a preceding spool list is closed using NEW-PAGE PRINT OFF, the preceding spool list is now always resumed. Previously, a new spool list was opened in this case if the spool parameters were different and the existing spool list resumed only if the spool parameters were identical. When the system returns to the spool list, the system field sy-spono is switched to the associated number of the spool request directly when the statement NEW-PAGE PRINT OFF is executed. Previously, sy-spono was not modified directly but only in the next output statement sent to a spool list. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 700</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="7">MISC</td>
            <td>Class for System States</td>
            <td>The static methods of the class CL_ABAP_SYST return important system states and replace the evaluation of the corresponding system fields if it is necessary to ensure that a system field was not incorrectly overwritten in a program. </td>
        </tr>
<tr>
            <td>Number of ABAP Sessions per User Session</td>
            <td>From ABAP release 7.0, up to 16 ABAP sessions can be opened per user session, compared to the previous maximum of 6. The actual number is controlled by the system parameter rdisp/max_alt_modes. The default value for this parameter is 6 as before. To enable more than single-digit numbers in the system field sy-modno, its data type has been changed (incompatibly ) from c of length 1 to i. </td>
        </tr>
<tr>
            <td>Storage of Interface Constants</td>
            <td>From ABAP release 7.0, an interface constant is created exactly once in the internal session and GET REFERENCE returns the same reference regardless of the name. Before ABAP release 7.0, a new interface constant was created for each implementation and different formulations like i1=&gt;const, c1=&gt;i1~const, or c2=&gt;i1~const produced different references for a constant const with GET REFERENCE. </td>
        </tr>
<tr>
            <td>Exception Handling for Data Clusters</td>
            <td>In IMPORT, the exception IMPORT_FORMAT_ERROR has been made catchable by assigning it to the new class CX_SY_IMPORT_FORMAT_ERROR. The exception CONNE_IMPORT_CONVERSION_ERROR has been made catchable by assigning it to the existing class CX_SY_CONVERSION_CODEPAGE (change was also transported back to ABAP release 6.40) The addition IGNORING CONVERSION ERRORS now also suppresses the exception raised by the fact that the number of bytes of an imported character-like component increases when it is converted to another code page, and therefore no longer fits into the target object. Up to now, only superfluous blanks were cut off without raising an exception (change was also transported back to ABAP release 6.40). </td>
        </tr>
<tr>
            <td>Authorization Check for Users</td>
            <td>From ABAP release 7.0, the statement AUTHORITY-CHECK now has the addition FOR USER, which can be used to check the authorizations of any user. </td>
        </tr>
<tr>
            <td>Strings on Selection Screens</td>
            <td>In the statement PARAMETERS, it is now also possible to specify the data type string after the addition TYPE. </td>
        </tr>
<tr>
            <td>Format of Lists when Sending</td>
            <td>The internal format into which an ABAP list is packed when the send function is called, has been changed. The previous format can still be read. From ABAP release 7.0, to send lists to systems with older releases, the new format must be converted to the previous format using the function module LIST_CONVERT_NEW_TO_OLD_FORMAT. </td>
        </tr>
<tr>
            <td rowspan="2">OBJECT_SERVICES</td>
            <td>Loading Multiple Persistent Objects</td>
            <td>From ABAP release 7.0, the interface IF_OS_CA_PERSISTENCY of the persistence service contains the methods GET_PERSISTENT_BY_OID_TAB, GET_PERSISTENT_BY_KEY_TAB, and GET_PERSISTENT_BY_QUERY. This makes it possible to retrieve more than one persistent object at once from the database and to create the appropriate instances in the ABAP program. </td>
        </tr>
<tr>
            <td>Query Service</td>
            <td>From ABAP release 7.0, the object services offer a query service to find and load persistent objects from the database. </td>
        </tr>
<tr>
            <td rowspan="4">TOOLS</td>
            <td>ABAP Debugger</td>
            <td>The new two-process debugger has been further developed as follows: Alignment to the functions of the previous debugger: Lines can be inserted and deleted in the display of internal tables. Watchpoints can be created. External programs (RFC, update function modules, BSP, ...) can be debugged. Integration of memory analysis and Memory Inspector. New tools: New Editor Control used for source code, which displays the content of variables as a tooltip. Display of differences between complex data objects. Display of data object graphs. Display of global data objects of all loaded programs. Structured display of the current screens and container controls. Enhancements: Variable fast display split into global and local data. Display of current parameter interface with local data. Complete screen and ABAP stack in the stack display. Pausing of a running process at a particular point by setting a session breakpoint in a parallel ABAP session. Hint From ABAP release 7.0, the default setting is the use of the two-process debugger. This setting can be changed in ABAP Editor by choosing Utilities → Settings → ABAP Editor → Debugging. </td>
        </tr>
<tr>
            <td>Memory Inspector</td>
            <td>The following improvements were made: Memory Inspector can now be called in every transaction by choosing System → Utilities → Memory Analysis → Compare Memory Snapshots. Memory snapshots can now be created in every transaction by choosing System → Utilities → Memory Analysis → Create Memory Snapshot. </td>
        </tr>
<tr>
            <td>ABAP Unit</td>
            <td>The following improvements were made: The pseudo comments "#AU Risk_Level ... "#AU Duration ... can be used to define test properties when creating test classes using the statement CLASS ... FOR TESTING. To enable the reuse of extensive test preparations, global test classes can be defined in Class Builder. Global test classes should always be abstract, can only be used in local test classes, and, like these, are not generated in production systems. </td>
        </tr>
<tr>
            <td>Logpoints</td>
            <td>The new LOG-POINT statement enables unconditional log entries to the log also used by ASSERT. Logpoints are activatable checkpoints, whose activation must be controlled using checkpoint groups or activation variants. Logpoints replace the incorrect usage of ASSERT for logging-only purposes. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 70</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">DATASET</td>
            <td>Handling of the Byte Order Mark</td>
            <td>When a UTF-8 text file is opened, the handling of the byte order mark (BOM) at the start of the file can be controlled using the new addition SKIPPING|WITH BYTE-ORDER MARK. </td>
        </tr>
<tr>
            <td>Definition of the Line End Marker</td>
            <td>When any text file is opened, the line end marker can be defined using the new addition WITH NATIVE|SMART|UNIX|WINDOWS LINEFEED. The structure of file properties used in the statements GET DATASET and SET DATASET has been enhanced by the addition of the components linefeed and linefeed_mode for the line end marker, so that these can be selected and set for an open file. </td>
        </tr>
<tr>
            <td>Help Class for the File Interface</td>
            <td>The static methods of the new class CL_ABAP_FILE_UTILITIES provide information about files on the host computer of the current AS instance. </td>
        </tr>
<tr>
            <td>Trailing Blanks in EBCDIC Files</td>
            <td>For text files handled as legacy EBCDIC files using the addition LEGACY TEXT MODE, the following criteria have been defined: As is also the case with other code pages, trailing blanks are now cut off when writing a legacy EBCDIC text file using TRANSFER. Before ABAP release 7.0, the hexadecimal EBCDIC code 40 was written to the file. When writing to a legacy EBCDIC text file using TRANSFER, it is now padded with the hexadecimal EBCDIC code 40 for blanks if the specified length is greater than that of the data object. Before ABAP release 7.0, it was padded with the ASCII code for blanks. If, when reading a legacy EBCDIC text file using READ DATASET, the target object must be padded with blank characters, it is now padded with the blanks of the current system code page. Before ABAP release 7.0, it was padded with hexadecimal 80. </td>
        </tr>
<tr>
            <td rowspan="6">DOCU</td>
            <td>Full Text Search</td>
            <td>Considerable improvements have been made to the performance of the full text search in the ABAP keyword documentation. The occurrences found by the full text search are now marked in the display. As before, the full text search can be called from the display window of the documentation. It is also available on the initial screens for the documentation (ABAP Editor, transaction ABAPHELP) as an alternative to the index search. </td>
        </tr>
<tr>
            <td>Subject Search</td>
            <td>An alphabetical subject directory has been added to the ABAP keyword documentation. The index search now evaluates this directory in addition to the ABAP index and ABAP glossary. </td>
        </tr>
<tr>
            <td>Diagrams</td>
            <td>Using access to the MIME repository, diagrams can now be displayed in the ABAP keyword documentation. Example: Sessions and Memory Areas </td>
        </tr>
<tr>
            <td>Language</td>
            <td>It is now possible to select the language in which the documentation is displayed on the initial screen of the transaction ABAPHELP. The user can choose between the logon language, English, and German. The logon language is the standard setting. When the ABAP keyword documentation is called from ABAP Editor, the logon language is always used. </td>
        </tr>
<tr>
            <td>Terminology Change for Narrowing and Widening Casts</td>
            <td>The use of the terms narrowing cast and widening cast, which refers to the view that a reference variable offers to an object, has been modified to reflect more general usage, which refers to the value set covered by a variable. Now the unambiguous terms upcast and downcast are now used almost everywhere. </td>
        </tr>
<tr>
            <td>Adjustment of Font Size</td>
            <td>If the font size is changed when adjusting the local layout in the SAP GUI, this also applies to the display of the ABAP keyword documentation. Previously, only changes in color affected the ABAP keyword documentation. Within a user session, the GUI font size dependency can be deactivated by setting the standard font size. </td>
        </tr>
<tr>
            <td rowspan="3">ITAB</td>
            <td>Addition CASTING After ASSIGNING</td>
            <td>In all statements for processing internal tables in which the addition ASSIGNING &lt;fs&gt; can be specified for the output behavior, it is now also possible to use the addition CASTING (familiar from the statement ASSIGN) to cast a table line to the type of the field symbol. </td>
        </tr>
<tr>
            <td>Sort Key Specified Dynamically in SORT</td>
            <td>In the statement SORT itab, the sort key, sort direction, and the text sorting can be specified in a new variant of the addition BY in a special internal table otab of type ABAP_SORTORDER_TAB. </td>
        </tr>
<tr>
            <td>Optimization when Specifying a WHERE Condition</td>
            <td>Until now, the optimized table reads performed when specifying an initial segment of the table key (using equality queries joined using AND) in cases where a WHERE condition was specified in the statements LOOP, DELETE, and MODIFY were only optimized for sorted tables. From ABAP release 7.0, this is done for hashed tables too. In hashed tables, however, the entire table key must be specified for the optimization to take place. </td>
        </tr>
<tr>
            <td rowspan="2">MISC</td>
            <td>Source Code Enhancements</td>
            <td>For source code enhancements using source code plug-ins, the following new statements have been introduced: ENHANCEMENT-POINT ENHANCEMENT-SECTION ENHANCEMENT Implicit enhancement points are also available in ABAP source code or parameter interfaces of procedures. </td>
        </tr>
<tr>
            <td>Enhancements Using BAdIs</td>
            <td>For enhancements using BAdIs, the following new statements have been introduced: GET BADI CALL BADI </td>
        </tr>
<tr>
            <td rowspan="5">REGEX</td>
            <td>Search for Regular Expressions in Character Strings</td>
            <td>From ABAP release 7.0, it is possible to search for regular expressions in the statements FIND and REPLACE. This replaces the search for patterns using the statement SEARCH. </td>
        </tr>
<tr>
            <td>Search for Multiple Occurrences Using FIND</td>
            <td>From ABAP release 7.0, the addition ALL OCCURRENCES can be used in the statement FIND. The previous behavior is expressed using the addition FIRST OCCURRENCE. In addition, the statements FIND and REPLACE for pattern-based searches have been mostly standardized. </td>
        </tr>
<tr>
            <td>Number of Occurrences in Unsuccessful Searches</td>
            <td>From ABAP release 7.0, the addition REPLACEMENT COUNT of the statement REPLACE sets the operand rcnt to 0 if no replacement has been made. This is the same as the statement FIND, in which mcnt is also set to 0 by the addition MATCH COUNT for unsuccessful searches. Before ABAP release 7.0, rcnt retained its own previous value if no replacement was made. </td>
        </tr>
<tr>
            <td>Find and Replace in Internal Tables</td>
            <td>From ABAP release 7.0, internal tables can be searched using the statement FIND IN TABLE and modified using REPLACE IN TABLE. This replaces the search in internal tables using the statement SEARCH. </td>
        </tr>
<tr>
            <td>New Additions in CONCATENATE</td>
            <td>From ABAP release 7.0, the addition LINES OF in the statement CONCATENATE can be used to concatenate lines of an internal table. The new addition RESPECTING BLANKS enables trailing blanks to be respected data objects of fixed length. This can also be used to assign text fields to strings when respecting the trailing blanks. </td>
        </tr>
<tr>
            <td rowspan="1">RFC</td>
            <td>New Transactional RFC</td>
            <td>A new interface has been developed for tRFC and qRFC, which will exist in parallel with the old interface from ABAP release 7.0. The statement for calling a transactional RFC from the new interface is CALL FUNCTION IN BACKGROUND UNIT. Hint Complete shipment from ABAP release 7.0, EhP2 only. </td>
        </tr>
<tr>
            <td rowspan="2">SFW</td>
            <td>Binding of Dynpros to Switch Framework</td>
            <td>The new addition SWITCH of the statement MODULE in the dynpro flow logic can be used to make the call of a dialog module dependent on the state of a switch. The statement FIELD and the activation of functions in the GUI status are controlled by binding switches to dynpro fields and status elements in Screen Painter or Menu Painter. </td>
        </tr>
<tr>
            <td>Binding the Syntax Check to Switch Framework</td>
            <td>In every implicit or explicit compilation of an ABAP program, the syntax check uses the switch configuration of Switch Framework that is provided when the statement is executed. When the statement GENERATE SUBROUTINE POOL is executed, the syntax check uses the switch configuration that was available at the time the current transaction was called. As a standard setting, a syntax check performed using the statement SYNTAX-CHECK uses the switch configuration available at the time the statement is executed. To achieve the same behavior as with the statement GENERATE SUBROUTINE POOL, the addition WITH CURRENT SWITCHSTATES has been introduced. </td>
        </tr>
<tr>
            <td rowspan="2">SHARED_OBJECTS</td>
            <td>Data References in the Shared Objects Memory</td>
            <td>From ABAP release 7.0, data references that point to data objects of the same area instance version can be stored permanently in the shared objects memory. An instance of a shared memory-enabled class can contain data reference variables as attributes that point to data objects within a closed area instance version. An exception is raised only if the used data type is unknown to the method DETACH_COMMIT in the shared objects memory when the method is executed. This is the case if the types of the referenced data objects were created dynamically. Due to technical reasons, no direct reference to data elements and table types in ABAP Dictionary is possible either. The opportunity to store data references in closed area instance versions makes it possible (from ABAP release 7.0) to store not only instances of classes, but also anonymous data objects using the addition AREA HANDLE of the statement CREATE DATA as shared objects. </td>
        </tr>
<tr>
            <td>Waiting Time for Change Locks</td>
            <td>From ABAP release 7.0, a waiting period can be passed to the parameter WAIT_TIME for the methods ATTACH_FOR_WRITE and ATTACH_FOR_UPDATE of the area class and for MULTI_ATTACH in CL_SHM_AREA. </td>
        </tr>
<tr>
            <td rowspan="2">XML</td>
            <td>Enhancements to ST</td>
            <td>The following changes have been made to the syntax for Simple Transformations: New statements tt:type, tt:node, and tt:front for the definition of types in the ST program. New attribute type for tt:root for the typing of data roots. In type definitions and typings, string and xstring can now be also be entered as elementary ABAP types, and lengths can be specified for c and x. New statement tt:extensible for controlling the extensibility of literal XML elements. Generalization of the term pattern for conditional transformations. See the ST Statement Overview. </td>
        </tr>
<tr>
            <td>Changes to CALL TRANSFORMATION</td>
            <td>In the statement CALL TRANSFORMATION, the dynamic specification of parameters after the addition PARAMETERS has been enhanced by the option of entering the following table types: abap_trans_obj_bind_tab for the specification of data references abap_trans_objbind_tab for the specification of object references. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 640</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="3">ABAP_SQL</td>
            <td>Dynamic Checks on Ranges Tables</td>
            <td>From ABAP release 6.40, it is possible to analyze a ranges table in a dynamic WHERE condition </td>
        </tr>
<tr>
            <td>Dynamic Checks on Internal Tables</td>
            <td>From ABAP release 6.40, it is possible to analyze an internal table specified in FOR ALL ENTRIES in a dynamic WHERE condition. The syntax comparison with a column in the internal table can be made statically or dynamically. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>Upper Limit for Values in Conditions Increased</td>
            <td>From ABAP release 6.40, the upper limit for the space required for all comparison values in WHERE, HAVING, or ON conditions as well as the values in the addition SET of the statement UPDATE is 64 MB. Previously, this limit was 64 KB. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td rowspan="3">CREATE</td>
            <td>Reference to a Type Description Object in CREATE DATA</td>
            <td>The new addition HANDLE of the statement CREATE DATA makes it possible to reference RTTS type description objects when data objects are created. From ABAP release 6.40, the RTTS classes contain methods for creating type description objects independently of existing types (refer to Runtime Type Services for ABAP release 6.40). Together with the addition HANDLE, this allows the dynamic construction of any type of data objects for the program runtime. </td>
        </tr>
<tr>
            <td>Any Reference Type in CREATE DATA</td>
            <td>In the statement CREATE DATA dref TYPE REF TO (name). it is now also possible to specify a data type in name. Previously, it was only possible to specify classes and interfaces. Hint This change was also transported to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>Reference to a Generic Table Type in CREATE DATA</td>
            <td>Previously, only non-generic table types could be specified in CREATE DATA - TYPE. As of ABAP release 6.40, table types with generic keys can also be specified. In this case, a new linked table type with a standard key is created and used. </td>
        </tr>
<tr>
            <td rowspan="3">DATASET</td>
            <td>New Statement TRUNCATE DATASET</td>
            <td>The new statement TRUNCATE DATASET sets the end of the file to a specified item, which truncates or expands the file. </td>
        </tr>
<tr>
            <td>New Addition NO END OF LINE in TRANSFER</td>
            <td>The new addition NO END OF LINE of the statement TRANSFER prevents an end of file marker from being appended to the transferred data in a text file or legacy text file. </td>
        </tr>
<tr>
            <td>Change to the Addition POSITION END-OF-FILE</td>
            <td>In the statement SET DATASET, the name of the addition POSITION END-OF-FILE has been changed to POSITION END OF FILE for consistency. The END-OF-FILE spelling is still supported for compatibility reasons, but is no longer documented and no longer recommended. </td>
        </tr>
<tr>
            <td rowspan="1">FIELD_SYMBOLS</td>
            <td>Reference to a Type Description Object in ASSIGN</td>
            <td>The new addition HANDLE of the statement ASSIGN can be used to refer to the RTTS type description objects when casting field symbols. From ABAP release 6.40, the RTTS classes contain methods for creating type description objects independently of existing types (refer to Runtime Type Services for ABAP release 6.40). Together with the addition HANDLE, this makes it possible to cast to any existing or dynamically created types at program runtime. </td>
        </tr>
<tr>
            <td rowspan="1">LISTS</td>
            <td>Lists and Unicode</td>
            <td>In Unicode systems, each character in the list buffer has a position but can occupy more than one column in the list, which is of particular relevance for Asian characters. However, since the list only contains the same number of columns as there are positions in the list buffer, this means the list can only display fewer characters than are stored in the list buffer. The list output is therefore shortened accordingly and the page conforms to the alignment. The horizontal position of the list cursor is only the same in the non-Unicode systems as the output column of displayed or spooled lists. In Unicode systems, this is only guaranteed for the top and bottom output limits. The following changes were made in ABAP to comply with the behavior of lists in Unicode systems: If list output in a Unicode system is reduced, this is indicated by an indicator &gt; or &lt;. The complete content can then be displayed by choosing System → List → Unicode Display. So that unnecessary reductions are not made in the Unicode systems, the new specifications * and ** were introduced for the output length in the WRITE statement. In Unicode systems, predefined output formats or predefined output lengths that are different to non-Unicode systems are valid. Hint These changes were also downported to ABAP release 6.20. </td>
        </tr>
<tr>
            <td rowspan="16">MISC</td>
            <td>VALUE Addition for All DATA Statements</td>
            <td>The addition VALUE can now be used in all variants of the statement DATA and in all related statements, like CONSTANTS. Previously, the addition could not be used when creating bound table types. It is now also possible to create constants with bound table types. </td>
        </tr>
<tr>
            <td>LEAVE TO CURRENT TRANSACTION</td>
            <td>If the new addition CURRENT TRANSACTION is specified after LEAVE TO TRANSACTION, the current transaction is called using the transaction code used to call CALL TRANSACTION or LEAVE TO TRANSACTION. Except in the case of a parameter transaction or variant transactions, this is the same transaction code as in the sy-tcode system field. For parameter or variant transactions it is their transaction code, while sy-tcode contains the name of the implicitly called dialog transaction. The transaction code for the current transaction can also be obtained using the new static method GET_CURRENT_TRANSACTION of class CL_DYNPRO. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>INTO Addition for the CLEANUP Statement</td>
            <td>The new addition INTO of the statement CLEANUP is used to make a reference to the exception object in the specified reference variable. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>IF FOUND Addition for the INCLUDE Statement</td>
            <td>The new addition IF FOUND can be used to prevent syntax errors in the statement INCLUDE if the specified program cannot be found. </td>
        </tr>
<tr>
            <td>Data Compression with GZIP</td>
            <td>System classes with methods for compressing character chains and byte chains with GZIP were introduced (see classes for compressing data). Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>Random Number Generator</td>
            <td>System classes for creating pseudo random numbers were introduced (see classes for mathematical operations). </td>
        </tr>
<tr>
            <td>New Method in CL_SYSTEM_TRANSACTION_STATE</td>
            <td>In class CL_SYSTEM_TRANSACTION_STATE there is a new method GET_SAP_LUW_KEY for determining the update key. </td>
        </tr>
<tr>
            <td>Variant MESSAGE oref for Messages</td>
            <td>If the variant MESSAGE oref is used, only one object reference variable can be specified whose dynamic type implements the system interface IF_T100_MESSAGE for oref. The attributes of the interface identify the message to be sent in table T100. The use of classes (introduced in ABAP release 6.20) that only implement the interface IF_MESSAGE is obsolete and only allowed for compatibility reasons. </td>
        </tr>
<tr>
            <td>Behavior of Error Messages in LOAD-OF-PROGRAM</td>
            <td>From ABAP release 6.40, a program is terminated with the runtime error SYSTEM_LOAD_OF_PROGRAM_FAILED if the event block for LOAD-OF-PROGRAM contains an error message. Before ABAP release 6.40, the system behaved according to the context where the program was loaded. </td>
        </tr>
<tr>
            <td>Revision of ABAP Keyword Documentation</td>
            <td>The ABAP keyword documentation has been completely revised. An alphabetical quick reference, an ABAP glossary, and an alphabetic ABAP index have been added. F1 Help in ABAP Editor and Screen Painter analyzes the index and the glossary. The color code in the hyperlinks has been changed as follows: Link in the keyword documentation Link to a glossary entry Link to a program in the current AS ABAP. </td>
        </tr>
<tr>
            <td>New System Class CL_ABAP_EXPIMP_CONV</td>
            <td>The methods of the class CL_ABAP_EXPIMP_CONV convert the release-dependent internal format of data clusters. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>Conversion Additions in IMPORT</td>
            <td>The conversion additions ACCEPTING PADDING, ACCEPTING TRUNCATION, and IGNORING STRUCTURE BOUNDARIES are now also possible in IMPORT FROM SHARED BUFFER. </td>
        </tr>
<tr>
            <td>Stricter Structure Check in IMPORT</td>
            <td>The rule that structures and substructures (in the addition ACCEPTING PADDING) in a target structure in the statement IMPORT can have more components than the source structure can lead to problems in references to the structures defined in ABAP Dictionary, if the structure is indicated there as extensible. From ABAP release 6.40, this situation can therefore produce a warning from the extended program check. </td>
        </tr>
<tr>
            <td>New Callback Routine in Asynchronous RFC</td>
            <td>In asynchronous RFC, the addition CALLING addition was introduced to also specify methods as callback routines. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>Simple Transformation Calls</td>
            <td>CALL TRANSFORMATION can now be used to call Simple Transformations, as well as XSLT programs. </td>
        </tr>
<tr>
            <td>Control Options for Transformations</td>
            <td>In CALL TRANSFORMATION, the addition OPTIONS can be used to specify control options for the called transformations. </td>
        </tr>
<tr>
            <td rowspan="4">OBJECTS</td>
            <td>Data Types and Constants in the Visibility Section of Global Classes</td>
            <td>From ABAP release 6.40, data types and constants can be created in the public visibility section of global classes and interfaces, which was not previously possible. Classes and interfaces therefore make the use of type pools superfluous, whereas for types and constants names that are longer than the names in the type pools are possible. </td>
        </tr>
<tr>
            <td>Access to Static Components of Object Types</td>
            <td>The statements CLASS cl DEFINITION LOAD and INTERFACE in LOAD are now only necessary if source code contains recursive accesses to global classes or interfaces. Until now, these statements always had to be specified if static components of global classes or interfaces were being accessed for the first time. Transaction SYNT, to which a trace for these object types has been added, can be used to detect recursive class and interface definitions. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>Use of Alias Names</td>
            <td>From ABAP release 6.40, it is possible to specify the alias names of the methods defined using ALIASES in the implementation of methods using the statement METHOD and in the redefinition of methods using the statement METHODS ...REDEFINITION. At the same time, however, from ABAP release 6.40 there will be a warning about the syntax check if identical components are accessed within a class declaration or a method with different names. </td>
        </tr>
<tr>
            <td>Unused Private Components</td>
            <td>From ABAP release 6.40, all unused private components of a class produce a warning in the extended program check. Private methods must be called and private events must be both raised and handled. </td>
        </tr>
<tr>
            <td rowspan="7">RTTI</td>
            <td>Methods for Creating Data Types</td>
            <td>The type description classes of RTTI were enhanced using RTTC methods. These methods create type description objects independently of existing types. This means what was previously RTTI becomes the RTTS. Together with the addition HANDLE of the statements CREATE DATA and ASSIGN, the RTTC-specific methods of the RTTS allow the construction of data objects of any types for the program runtime and the casting of dynamic types. The most important new methods for dynamically defining data types are: GET_C, GET_D, GET_F, and so on of class CL_ABAP_ELEMDESCR for creating type description objects for elementary data types. CREATE of class CL_ABAP_STRUCTDESCR for creating type description objects for structured data types based on a component table. CREATE of class CL_ABAP_TABLEDESCR for creating type description objects for table types, where the properties of the table are passed to the input parameters. CREATE of class CL_ABAP_REFDESCR for creating type description objects for reference types, where the static type is passed to an input parameter. </td>
        </tr>
<tr>
            <td>New Methods for Types from ABAP Dictionary</td>
            <td>If the type of a type description object comes from ABAP Dictionary, the dictionary properties of the type can be identified using the following methods: For any data types CL_ABAP_TYPEDESCR=&gt;IS_DDIC_TYPE returns abap_true when the object describes a type from ABAP Dictionary CL_ABAP_TYPEDESCR=&gt;ABSOLUTE_NAME returns the absolute name of a type (also for program-defined types) CL_ABAP_TYPEDESCR=&gt;GET_DDIC_HEADER returns the nametab header of a type from ABAP Dictionary CL_ABAP_TYPEDESCR=&gt;GET_DDIC_OBJECT returns the nametab field descriptions of a type from ABAP Dictionary For elementary data types CL_ABAP_ELEMDESCR=&gt;GET_DDIC_FIELD returns the properties of the data element and its texts (replaces function module DDIF_FIELDINFO_GET) CL_ABAP_ELEMDESCR=&gt;GET_DDIC_FIXED_VALUES returns the fixed values of the data element For structures and database tables CL_ABAP_STRUCTDESCR=&gt;GET_DDIC_FIELD_LIST returns the features of the structure and its texts (replaces function module DDIF_FIELDINFO_GET) </td>
        </tr>
<tr>
            <td>Enhancement of the Method GET_COMPONENT_TYPE</td>
            <td>Previously, the method GET_COMPONENT_TYPE of class CL_ABAP_STRUCTDESCR for parameter P_NAME only accepted actual parameters of types string and c. Although the formal parameter is type any, during the call, a type check was executed that caused a serious error for non-text-like actual parameters. Actual parameters of type csequence and numeric are now accepted. A text-like parameter is interpreted as a component name, a numeric parameter as the position of the component in the structure. An actual parameter that does not apply to csequence or numeric raises the non-class-based exception UNSUPPORTED_INPUT_TYPE. </td>
        </tr>
<tr>
            <td>Enhancement of the Method GET_PROPERTY</td>
            <td>A new public constant CL_ABAP_TYPEDESCR=&gt;TYPEPROPKIND_HASCLIENT has been introduced. If this constant of method GET_PROPERTY is passed to class CL_ABAP_TYPEDESCR, the system checks whether the type of the type description object has a client field. A reference to a data object of type abap_bool is always returned. If the type has a client field, the referenced data object has the value of the constant abap_true. If the type does not have a client field or if it does not affect a structure, the referenced data object has the value of the constant abap_false. </td>
        </tr>
<tr>
            <td>New Method HAS_PROPERTY</td>
            <td>A new public method HAS_PROPERTY has been introduced in class CL_ABAP_TYPEDESCR. Unlike GET_PROPERTY, this method only identifies whether a type has a property and only returns the values from abap_true or abap_false. </td>
        </tr>
<tr>
            <td>New Method GET_CLASS_NAME</td>
            <td>A new public method GET_CLASS_NAME has been introduced in class CL_ABAP_CLASSDESCR. This method returns the name of the class of an object. </td>
        </tr>
<tr>
            <td>New Method GET_DATA_TYPE_KIND</td>
            <td>A new public method GET_DATA_TYPE_KIND has been introduced in class CL_ABAP_DATADESCR. This method returns the same values as the statement DESCRIBE FIELD with the addition TYPE. These values are also defined as constants with the prefix TYPE_KIND_ of class CL_ABAP_DATADESCR. </td>
        </tr>
<tr>
            <td rowspan="1">SELECTION_SCREENS</td>
            <td>Arrangement of Radio Buttons</td>
            <td>From ABAP release 6.40, the addition RADIOBUTTON of the statement PARAMETERS displays defined radio buttons in the first position of the selection screen and the associated text on the right. Before ABAP release 6.40, radio buttons were displayed on the right of the text. </td>
        </tr>
<tr>
            <td rowspan="7">TOOLS</td>
            <td>Assertions and Activatable Breakpoints</td>
            <td>The new statement ASSERT can be used to define assertions. Assertions help verify particular assumptions about the state of a program in a particular location, and they guarantee that these assumptions hold. Assertions can be activated from outside the program using a checkpoint group and the addition ID. The same addition was also introduced for the statement BREAK-POINT to activate breakpoints using checkpoint groups. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>New Two-Process Debugger</td>
            <td>The new two-process debugger is a completely new development, with particular emphasis on the development of a more modern user interface. The main differences between this and the previous ABAP debugger are that the new debugger is executed in a separate ABAP session and the object that is being analyzed (the debuggee) is now the entire ABAP session instead of an internal session. The new ABAP debugger provides the user with a flexible and freely configurable interface with over eight desktops, on which between one and four tools, such as source code display or structure display, can be placed and arranged. In ABAP release 6.40, there is now a choice of the previous debugger and the new debugger; it is now also possible to switch between the two at any time during a debugger session. </td>
        </tr>
<tr>
            <td>Enhancements to the Previous Debugger</td>
            <td>The following improvements were made: When internal tables are displayed, offsets and lengths can be specified for the character-like components in the column headers. When internal tables are displayed, the corresponding icons next to the table names can be selected to list the names of all internal tables displayed in the previous debugger session and double-clicked to display them. The display starts from the line number and the component with which the selected table was last displayed. If the program flow is stuck on a CASE statement, the system branches in a single step to the affected WHEN block. In ABAP Editor, external debugging can be activated and deactivated under Utilities → Settings. Depending on the settings and whether a regular ABAP program or an ABAP Script for a BSP is being edited, a prompt may appear (when creating breakpoints in ABAP Editor) asking whether these breakpoints are to be HTTP breakpoints or session breakpoints. HTTP breakpoints are persisted in the database for different logons; session breakpoints are regular breakpoints that refer to the current logon. </td>
        </tr>
<tr>
            <td>Memory Inspector</td>
            <td>The Memory Inspector tool was introduced for the analysis of memory snapshots. Hint This change was also transported back to ABAP release 6.20. </td>
        </tr>
<tr>
            <td>Code Inspector</td>
            <td>The Code Inspector tool was introduced to check repository objects for performance, security, syntax, and the adherence to naming conventions. </td>
        </tr>
<tr>
            <td>ABAP Unit</td>
            <td>The ABAP Unit tool was integrated in the ABAP runtime framework to test individual program sections. ABAP Unit is based on the execution of test methods in test classes. </td>
        </tr>
<tr>
            <td>Runtime Analysis</td>
            <td>When measuring BSP applications, it is now also possible to specify a different variant to the standard variant for restrictions (in the runtime analysis transaction SE30). The class CL_ABAP_RUNTIME provides methods for creating objects. The method GET_RUNTIME can be used to execute multiple runtime measurements with different resolutions and parallel measurements (see Class for Runtime Measurements). </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 620</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="4">CLASSES</td>
            <td>Class for Calculating with Time Stamps</td>
            <td>Class CL_ABAP_TSTMP has been introduced for calculating with UTC time stamps in packed numbers. </td>
        </tr>
<tr>
            <td>Class for Formatting Lists</td>
            <td>Class CL_ABAP_LIST_UTILITIES has been introduced to calculate output lengths, convert values from the list buffer, and define field limits. With the return values for their methods, a correct column alignment for ABAP lists can be programmed (even when Eastern Asian scripts are displayed). The objects in a list can be displayed in different output lengths by specifying the required length in the menu under System → List → Unicode Display. This is of particular use with screen lists where the output is cut off, which is displayed by the characters (&gt; or &lt;). </td>
        </tr>
<tr>
            <td>Error Handling of Exceptions</td>
            <td>Interfaces IF_MESSAGE and IF_SERIALIZABLE_OBJECT have been added to class CX_ROOT, the global superclass for all exceptions. Texts from classes that implement these interfaces are edited as message texts. The method GET_LONGTEXT is used to provide the long text for an exception text. </td>
        </tr>
<tr>
            <td>Methods for Determining Type Properties</td>
            <td>Two new methods have been introduced in class CL_ABAP_DATADESCR for determining type properties at runtime (RTTI): The method IS_READ_ONLY determines whether a reference data object is write-protected. The method APPLIES_TO_DATA_REF executes a type check between the description and the data object. This makes it possible to determine the compatibility of data objects. </td>
        </tr>
<tr>
            <td rowspan="13">EXCEPTIONS</td>
            <td>Catchable Exceptions in Method Calls</td>
            <td>The following exceptions can now be handled in method calls, having been assigned to existing exception classes: CALL_METHOD_NOT_IMPLEMENTED CALL_METHOD_NOT_ACCESSIBLE CALL_METHOD_CONFLICT_TAB_TYPE CALL_METHOD_CONFLICT_GEN_TYPE CALL_METHOD_CONFLICT_TYPE </td>
        </tr>
<tr>
            <td>Catchable Exceptions in Function Module Calls</td>
            <td>In CALL FUNCTION, the following exceptions can now be handled, having been assigned to existing exception classes. The new exception class CX_SY_DYN_CALL_ILLEGAL_FUNC was introduced only for the first two exceptions in the following list: CALL_FUNCTION_NOT_ACTIVE CALL_FUNCTION_NOT_FOUND CALL_FUNCTION_PARM_MISSING CALL_FUNCTION_PARM_UNKNOWN CALL_FUNCTION_CONFLICT_GEN_TYP CALL_FUNCTION_CONFLICT_LENG CALL_FUNCTION_CONFLICT_TYPE CALL_FUNCTION_NO_VB CALL_FUNCTION_WRONG_ALIGNMENT </td>
        </tr>
<tr>
            <td>Catchable Exceptions in Downcasts</td>
            <td>In downcasts, the following exceptions can now be handled, having been assigned to an existing exception class: MOVE_CAST_ERROR_DYN MOVE_CAST_REF_ONLY </td>
        </tr>
<tr>
            <td>Catchable Exceptions in ABAP SQL</td>
            <td>In ABAP SQL, the following exceptions can now be handled, having been assigned to an existing exception class. ESCAPE_WITH_POOLTABLE SAPSQL_CONNECTION_ILL_TABTYPE </td>
        </tr>
<tr>
            <td>Catchable Exceptions in CALL TRANSFORMATION</td>
            <td>The exception classes CX_SY_CONVERSION_NO_RAW and CX_SY_CONVERSION_NO_DATE_TIME have been introduced for the statement CALL TRANSFORMATION. </td>
        </tr>
<tr>
            <td>Catchable Exceptions in GENERATE REPORT | SUBROUTINE POOL</td>
            <td>The exception class CX_SY_GEN_SOURCE_TOO_WIDE has been introduced for the statement GENERATE REPORT|SUBROUTINE POOL. </td>
        </tr>
<tr>
            <td>Catchable Exceptions in SCAN</td>
            <td>The exception class CX_SY_SCAN_SOURCE_TOO_WIDE has been introduced for the statement SCAN. </td>
        </tr>
<tr>
            <td>Catchable Exceptions in EXPORT TO SHARED MEMORY</td>
            <td>The exception CX_SY_EXPORT_NO_SHARED_MEMORY has been introduced for EXPORT TO SHARED MEMORY. </td>
        </tr>
<tr>
            <td>Catchable Exceptions in PROVIDE</td>
            <td>The exception classes CX_SY_PROVIDE_INTERVAL_OVERLAP and CX_SY_PROVIDE_TABLE_NOT_SORTED have been introduced for the new variant of the statement PROVIDE. These exceptions are not raised by the short form of PROVIDE. </td>
        </tr>
<tr>
            <td>Displaying Work Areas</td>
            <td>The system now only displays processed work areas, and the data area directory no longer exists. Instead, only the directory of application tables is displayed, including their administration information. </td>
        </tr>
<tr>
            <td>Selected Variables</td>
            <td>The display of selected variables has been improved. The data for all call levels is now displayed, whereas previously only the uppermost call level was displayed. A display of all active calls can be viewed under application calls. </td>
        </tr>
<tr>
            <td>SNAP Variables</td>
            <td>The SNAP variable list contains the data for the short dump in compact form, without explanatory text. This data can be stored locally on the front end. Due to the shorter representation, this format is suitable for sending, for example as an attachment in a customer problem message. </td>
        </tr>
<tr>
            <td>Display Languages</td>
            <td>When short dumps are displayed, the system now also respects the second language, stored in the profile parameter zcsa/second_language. If no text is found, a note can be created and referred to in the standard text. The search for the short dump text is performed in the following order with respect to language version: Search for text in logon language Search for text in second language Search for text in English Search for standard text including note created Search for standard text As soon as a text is found, the system stops the search and displays the text. </td>
        </tr>
<tr>
            <td rowspan="19">MISC</td>
            <td>Parameter INITIAL SIZE for Internal Tables</td>
            <td>An internal table created as a data type in ABAP Dictionary can be assigned an initial memory requirement. This specification in the dictionary has the same effect as the INITIAL SIZE addition in the ABAP source code. Therefore, all the information that describes a table, such as table category, key, line number, and type, are also available in the dictionary. </td>
        </tr>
<tr>
            <td>Relevant Language Key in Dictionary Structures</td>
            <td>For error-free conversion of data that is swapped between Unicode and non-Unicode systems, you can specify the structure components responsible for the relevant language key in ABAP Dictionary. If necessary, this information can be queried in the remote function call. </td>
        </tr>
<tr>
            <td>New Functions in the Extended Program Check</td>
            <td>The following changes have been made to the extended program check: Errors in included type pools are no longer displayed. An INSERT itab outside a loop causes a warning. All characters in the ABAP source code, especially special characters that do not comply with the naming conventions, raise a warning. All structure enhancements whose ABAP source code indicate potential runtime errors or changed program behavior raise a warning. </td>
        </tr>
<tr>
            <td>Handling IMPORTING Parameters with CREATE</td>
            <td>Formal parameters of methods that were passed to the method using IMPORTING and referenced there using CREATE can no longer be overwritten. This check, which was previously not executed till runtime, is now caught by the syntax check. </td>
        </tr>
<tr>
            <td>Secondary Language Access for Text Elements</td>
            <td>Secondary language access is now available for report titles, list headers, selection texts, text symbols, and texts specified dynamically of the form ASSIGN ('TEXT-nnn') TO &lt;fs&gt;. If the text pool does not exist in the logon language, the system loads the text pool that matches the entry in the profile parameter zcsa/second_language. Until now, text elements that were not available in the text pool for the language lg remained unaltered by the statement SET LANGUAGE lg. In the context of this change, these texts are now reset to their initial value. </td>
        </tr>
<tr>
            <td>System Field sy-toccu Obsolete</td>
            <td>In the statements READ TABLE, LOOP AT itab, and DESCRIBE TABLE, the system field sy-toccu is no longer filled. </td>
        </tr>
<tr>
            <td>Compatibility in Structure Typing</td>
            <td>If structures are passed to formal parameters and function modules typed using STRUCTURE, or assigned to similarly typed field symbols, closing alignment gaps are now also respected by the type check. </td>
        </tr>
<tr>
            <td>Lengths Specified in the Statement WRITE</td>
            <td>As of ABAP release 6.20, the output length can be specified using the variants WRITE AT (*) and WRITE AT (**), as well as using numeric data objects. </td>
        </tr>
<tr>
            <td>XML Serialization and XML Deserialization</td>
            <td>Objects and data references can now also be serialized and deserialized in XML serialization and XML deserialization. Objects are only serialized if the class implements the interface IF_SERIALIZABLE_OBJECT. Data references can only be serialized if the referenced type is not anonymous. </td>
        </tr>
<tr>
            <td>Static Method Call from XSLT</td>
            <td>It is now also possible to call public static methods from XSLT programs. </td>
        </tr>
<tr>
            <td>Addition USER-COMMAND After PARAMETERS ... AS LISTBOX</td>
            <td>The addition USER-COMMAND can be specified for the statement PARAMETERS together with the addition AS LISTBOX. </td>
        </tr>
<tr>
            <td>Wider Frame on Selection Screens</td>
            <td>The standard maximum width of a frame around a block created using SELECTION-SCREEN is now 120 columns. It was previously 83 columns. </td>
        </tr>
<tr>
            <td>DAYLIGHT SAVING TIME in CONVERT TIME STAMP</td>
            <td>The addition DAYLIGHT SAVING TIME dst has been added to the statement CONVERT TIME STAMP tst. This provides a flag in data object dst that indicates whether the time tst is within the daylight saving time. </td>
        </tr>
<tr>
            <td>AS PERSON TABLE in Infotypes</td>
            <td>This addition creates an internal table of personal data when infotype nnnn is declared. The internal table is given the name PPnnnn, unless a name was specified explicitly using the addition NAME. </td>
        </tr>
<tr>
            <td>TO|FROM INTERNAL TABLE in EXPORT|IMPORT</td>
            <td>The variant EXPORT ... TO INTERNAL TABLE itab makes it possible to store cluster data in the internal table itab. The variant IMPORT ... FROM INTERNAL TABLE itab can be used to read this repository again. </td>
        </tr>
<tr>
            <td>CONNECTION in ABAP SQL</td>
            <td>The addition CONNECTION enables all ABAP SQL statements to use databases other than the standard database. The alternative database systems must be supported by SAP. </td>
        </tr>
<tr>
            <td>SOURCE in CALL TRANSFORMATION</td>
            <td>The addition SOURCE enables ABAP data and objects to be serialized in a canonical XML format, where the SAP XSLT processor has a direct interface to the serializer. The result of this XSLT transformation can be produced as XML data or be converted back into ABAP data and objects. </td>
        </tr>
<tr>
            <td>VISIBLE LENGTH in SELECTION-SCREEN PUSHBUTTON</td>
            <td>This addition can be used to change the visible length of pushbuttons for the statement SELECTION-SCREEN PUSHBUTTON. </td>
        </tr>
<tr>
            <td>DISPLAY|MEMORY OFFSET in GET|SET CURSOR|LINE</td>
            <td>When OFFSET is specified with the additions DISPLAY or MEMORY in list processing, the statements GET CURSOR { FIELD f | LINE l } and SET CURSOR { FIELD f | LINE l} can be used to specify whether the column in the displayed list or the position in the list buffer is intended. The addition DISPLAY is the standard and can be omitted. </td>
        </tr>
<tr>
            <td rowspan="6">OBJECTS</td>
            <td>New Variant PROVIDE FIELDS</td>
            <td>The statement PROVIDE FIELDS makes it possible to process internal tables without headers. The functions of the statement PROVIDE are therefore also available under ABAP Objects. </td>
        </tr>
<tr>
            <td>Creating Objects from the SAP XSLT Processor</td>
            <td>It is possible to create objects and call class methods from the SAP XSLT processor. Now only public methods are allowed, whereas previously private and protected methods could also be called. Parameter passing now supports all ABAP elementary types, any object references, and the generic types csequence and xsequence. Previously, the types c, d, f, i, n, string, and t were supported. The transfer has been tightened, so that for the data types d and i only valid specifications are allowed. Value losses for type p are now caught. The values of types x and xstring are converted into the XML standard format base64. </td>
        </tr>
<tr>
            <td>Polymorphism and Object Services</td>
            <td>The uniqueness of object keys is now checked not only by class, but across the whole inheritance hierarchy. The behavior of the following methods has also been changed: The methods RELEASE and REFRESH_PERSISTENT behave polymorphically, so that objects of subclasses can also be handled. The methods GET_PERSISTENT and DELETE_PERSISTENT now access a table of the root class first, to optimize the type determination. Because the type is now stored over multiple transactions, it can no longer be changed. By searching in the tables of the root class, the methods CREATE_PERSISTENT and CREATE_TRANSIENT now check whether the object key exists in another class of the inheritance hierarchy. </td>
        </tr>
<tr>
            <td>Accessing Data References and Static Class Components in JavaScript</td>
            <td>Binding JavaScript objects to ABAP objects has been expanded to enable access to data reference variables and static components (attributes and methods) of classes. In bound internal tables, you can delete lines using the JS method deleteLines and the JS method append has been renamed appendLine. Hint Support for the connection of JavaScript to ABAP will be discontinued without replacement in a release after 7.1. </td>
        </tr>
<tr>
            <td>Kernel Methods</td>
            <td>The addition BY KERNEL MODULE of the statement METHOD makes it possible to implement methods as kernel methods. </td>
        </tr>
<tr>
            <td>Recursions of RAISE EVENT</td>
            <td>The number of possible recursions of the statement RAISE EVENT has been raised from 63 to 1023. </td>
        </tr>
<tr>
            <td rowspan="4">TOOLS</td>
            <td>Code Inspector</td>
            <td>Code Inspector is a tool that enables ABAP programs, function modules, classes, interfaces, and dictionary objects to be checked statically for errors, performance, security, and reliability. The development of these repository objects is supported by simple search and check functions. The check results are available in the form of a tree hierarchy Code Inspector is called for a single object from ABAP Editor by choosing Program → Check → Code Inspector, from Function Builder by choosing Function Module → Code Inspector, or from Class Builder by choosing Object Type → Check → Code Inspector. If more than one repository object needs to be checked, for example all development objects in a package, these can be grouped together in an object set. It is also possible to define check variants with individual checks. A check of all objects in an object set using a specified check variant is called an inspection. The Code Inspector executes purely static checks that only return hints and clues for an object. The actual runtime behavior can be ascertained using Runtime Analysis or SQL Trace. </td>
        </tr>
<tr>
            <td>Runtime Monitor</td>
            <td>The Runtime Monitor is a framework that supports the recording of ABAP program information at runtime. This information can come from tests that are fixed in the ABAP kernel. ABAP programmers can also query and log specific program conditions at runtime. With Test &gt; Create ABAP Test, the Runtime Monitor creates a class, which can be called to record data in the source code. The data is first compressed in the main memory and periodically transferred to the database in a background job. The tests can be individually activated and deactivated for different servers. Compressing and saving the data hardly affects the AS instance, which means that the Runtime Monitor can be used at any time, even in a production operation. </td>
        </tr>
<tr>
            <td>Debugger</td>
            <td>The following improvements were made: The display of the memory consumption of dynamic objects has been divided into an overview of the total memory consumption and ranking lists for individual objects. The ranking lists can be compiled according to specific criteria by choosing the function Change Settings. Memory consumption can now be displayed by choosing Goto → Status Display → Memory Usage. Breakpoints are defined according to whether they take effect in a HTTP session or in a standard session. HTTP debugging is activated in the editor by choosing Utilities → Settings → HTTP Debugging. Depending on the setting, the system then displays either the HTTP or standard breakpoints in the Editor. If, under Settings, the function Check Sorting Before READ BINARY SEARCH is selected, the system checks, before every execution of this statement, whether the internal table is sorted. If the table is not sorted, a runtime error occurs. This setting should only be activated shortly before reaching a relevant point in the source code, because there can be a significant loss in performance, depending on the table size. If the Check Sorting Before PROVIDE function was selected under settings, the system checks all of the relevant tables - and not just the area specified with extlim1 and extlim2 - for sorting and overlapping intervals when the long form of the PROVIDE statement is executed. When displaying exception objects, the system only displayed the key itself in the field display for the TEXTID attribute that contains the OTR key of the text description assigned to the exception. Because this key is generated automatically and is nothing more than a sequence of numbers, assigning the corresponding text to the exception was difficult. The reason for this is that the displayed value had to be compared with the values of all constants generated for the exception. To simplify the assignment, the name of the constant generated for the key is now displayed as a tooltip. For example, in the case of an exception of the type CX_SY_FILE_IO for the TEXTID attribute, the system displays READ_ERROR or WRITE_ERROR as a tooltip, depending on whether the exception was raised while reading or writing. The actual value of the attribute is the OTR key of the corresponding text description. By choosing Debugging → Session, breakpoints and the settings System Debugging and Always Create Exception Object can now be saved persistently and reloaded later. The session can be saved by entering a name and expiration date for it. It is then available to other users and sessions, with the selected settings. </td>
        </tr>
<tr>
            <td>Runtime Analysis</td>
            <td>In Runtime Analysis, it is no longer possible to create temporary variants. Instead, a separate standard variant can be created, which is automatically assigned the user name by the system. Other variants can also be created with the name of the user or the names of other users, as long as a master record exists for them. When runtime analysis is first started, the system always displays the standard variant of the user. If it does not exist, the system displays the SAP standard variant. If runtime analysis is called again, the system always displays the last used variant. Additionally, the create, delete, and copy functions are again included in the measurement restrictions block on the initial screen. While create and copy can only be executed as single functions, the F4 key can be used to delete multiple variants. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 610</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="13">ABAP_SQL</td>
            <td>COMMIT WORK in Local Updates</td>
            <td>As in the asynchronous update, the system now also sets the system field SY-ONCOM = V in the local update. Therefore the system recognizes a COMMIT WORK in the function module that makes the local update, and this raises an uncatchable exception (a runtime error) </td>
        </tr>
<tr>
            <td>Additional Syntax Checks</td>
            <td>Additional syntax checks were introduced for the following constructs: Subqueries in WHERE conditions are only allowed if the source table is transparent. The addition GROUP BY is now also only possible with transparent tables. INTO CORRESPONDING is now only possible when moving data to structured work areas or internal tables. ESCAPE in WHERE conditions for SELECT, DELETE, and UPDATE of pooled tables </td>
        </tr>
<tr>
            <td>Type Check in the Addition VERSION</td>
            <td>In the case of the statements MODIFY, DELETE, READ TABLE, and LOOP AT only fields of types C, N, D, T, and flat structures of the same types are allowed for the addition VERSION. </td>
        </tr>
<tr>
            <td>Dynamic SQL</td>
            <td>Dynamic specifications are now possible for the following language constructs: ABAP variables in WHERE, HAVING, FROM, and SET. Lowercase spelling when specifying table names dynamically. Dynamic WHERE in DELETE and UPDATE Dynamic SET in UPDATE. Dynamic aggregate expressions in the HAVING clause Dynamic FROM in SELECT ... FROM for joins, ON conditions, and table aliases Strings, tables of strings, and C fields for all operand positions for which statements could previously be specified dynamically in an internal table. </td>
        </tr>
<tr>
            <td>Implicit Client Handling</td>
            <td>The client is now specified in the database interface. Previously the system changed the client field in an internal table or work area, as soon as the user carried out an INSERT or UPDATE on the database table. Now the system enters the current client in the database table, while the client in the work area or internal table remains unchanged. </td>
        </tr>
<tr>
            <td>Use of DISTINCT in Pooled Tables and Cluster Tables</td>
            <td>If DISTINCT is used in the SELECT clause with pooled tables and cluster tables, this always produces (statically) a syntax error if individual columns are accessed. This combination was not detected dynamically and the addition DISTINCT was simply not executed. In dynamic cases, the catchable exception CX_SY_DYNAMIC_OSQL_SEMANTICS is now raised. </td>
        </tr>
<tr>
            <td>Database Changes Using Read-Only Views</td>
            <td>Previously, an attempt to change database tables using read-only views simply caused a runtime error. If this is known statically, a syntax error is now produced. </td>
        </tr>
<tr>
            <td>Catchable Exceptions in ABAP SQL and Native SQL</td>
            <td>In ABAP SQL, exceptions that occur when the source code is being parsed can now be handled using TRY ... ENDTRY. As before, various runtime errors were assigned exception classes. In statically embedded Native SQL, all runtime errors of the database interface are now assigned exception classes, so that the exceptions between TRY ... ENDTRY can be handled. In addition, the opening of too many cursors and connections can be caught. </td>
        </tr>
<tr>
            <td>INSERT, UPDATE, and MODIFY of the System Table TRDIR</td>
            <td>Previously, INSERT TRDIR and UPDATE TRDIR were valid ABAP SQL statements. In the case of UPDATE TRDIR, program properties could be changed by the runtime synchronization without registration. Both statements now cause a syntax error. Only MODIFY TRDIR is still allowed, because there the runtime synchronization is switched on. The statement is for internal usage only and not documented. </td>
        </tr>
<tr>
            <td>Selection Table and Target Table in FOR ALL ENTRIES</td>
            <td>The table of selection conditions for FOR ALL ENTRIES and the target table of the SELECT statement can be the same. This makes the form SELECT ... INTO itab FOR ALL ENTRIES IN itab possible. </td>
        </tr>
<tr>
            <td>New Exception for Pooled Tables and Cluster Tables</td>
            <td>If pooled tables or cluster tables are used in subqueries, joins, aggregate functions, or with GROUP BY, this raises the exception CX_SY_DYNAMIC_OSQL_SEMANTICS. </td>
        </tr>
<tr>
            <td>Strings on the Database</td>
            <td>It is now possible to process character strings and binary data saved to database columns as strings in ABAP SQL. </td>
        </tr>
<tr>
            <td>WHERE Condition Optional in DELETE Statement</td>
            <td>In the statement DELETE FROM dbtab, the WHERE condition is no longer required. A DELETE statement without a WHERE condition deletes all rows of a table. </td>
        </tr>
<tr>
            <td rowspan="9">COMPILER</td>
            <td>Error Handling Using the Statement SYNTAX-CHECK</td>
            <td>The statement SYNTAX-CHECK has been enhanced so that the syntax check also continues after error messages occur, as long as the parameter ID = ERR is set. Any error messages are written to an internal table, which can be specified using TABLE. The collected errors also provide the return code sy-subrc = 4. </td>
        </tr>
<tr>
            <td>Static Checks in Specified Offsets/Lengths</td>
            <td>Previously, in offsets/lengths specified as field+offset(length) the values were not checked for convertibility to numbers of the types I, F, or P until runtime. This is now checked as part of the syntax check. </td>
        </tr>
<tr>
            <td>Data Types for Source Code in SCAN ABAP-SOURCE</td>
            <td>The statement SCAN ABAP-SOURCE has been enhanced so that fields of type C or string are allowed in the scanned source code, instead of an internal table. This means that an internal table no longer needs to be edited. </td>
        </tr>
<tr>
            <td>Implementation in TOP Includes</td>
            <td>If implementations of the type FORM, FUNCTION, or CLASS ... IMPLEMENTATION are made in a top include, a syntax warning is now produced. The program can still be activated however. </td>
        </tr>
<tr>
            <td>Faster Generation of Program Segments</td>
            <td>Program segments can be generated much more quickly, now that pre-compiled headers have been introduced. Pre-compiled headers allow the system to import and reuse previously processed data declarations of global classes, interfaces, and type pools more quickly, both during the syntax check and when generating the program. This means it is no longer necessary to import program segments repeatedly. If the compiler does not find a pre-compiled header for the required program segment, it generates one and stores it in the program buffer (PXA). The addition WITH PRECOMPILED HEADERS of the statement GENERATE REPORT can now be used to speed up generation from within an ABAP program. </td>
        </tr>
<tr>
            <td>Generating Data and Types in System Includes</td>
            <td>Data and types in includes of the form &lt;...&gt; were previously viewed as system objects and packed into a separate name space; however, this was ignored when the program was generated. For this reason, objects of this type could be hidden by global program definitions, so that it was unclear at runtime which definition was used. Now both generation and runtime handle system objects in the same way. Therefore in future, the following example will always produce the value 5. include &lt;SCREEN&gt;. data SCREEN type I value 5. write / SCREEN. </td>
        </tr>
<tr>
            <td>Converting C Literals with a Negative Maximum Value</td>
            <td>Previously, if an attempt was made to assign the highest negative integer value to a type I variable, in the form of the C literal '-2147483648', the system raised a catchable exception, CX_SY_CONVERSION_OVERFLOW. On the other hand, an assignment in the form of a numeric literal was accepted. Now both forms are supported, as the following example shows: DATA int TYPE i. int = '-2147483648'. int =  -2147483648. WRITE int. </td>
        </tr>
<tr>
            <td>Changing Read-Only DDIC Database Views</td>
            <td>In the editing status of the dictionary, it is possible to set a DDIC database view to read only, so that ABAP programs cannot make any changes. Previously the editing status was only checked at runtime. Now a syntax error is produced when this situation is known statically. </td>
        </tr>
<tr>
            <td>Passing the Source Code in GENERATE SUBROUTINE POOL</td>
            <td>In the statement GENERATE SUBROUTINE POOL, source code can now also be passed in an internal table of line type string. </td>
        </tr>
<tr>
            <td rowspan="5">DATASET</td>
            <td>Adapting to Unicode</td>
            <td>Extensive changes to the file interface have been made as a result of the switch to Unicode. </td>
        </tr>
<tr>
            <td>Reading and Changing File Properties at Runtime</td>
            <td>The statement GET DATASET ... returns both the current reading or write positions and the file properties. In turn, a required file position can be set at runtime using the statement SET DATASET .... It is also possible to change the properties of the file, but the system only applies values compatible with the opening mode. </td>
        </tr>
<tr>
            <td>New Classes for Converting Files</td>
            <td>Three classes have been implemented for file conversion: CL_ABAP_CONV_IN_CE: Instances of this class enable the conversion of external binary data to valid ABAP data objects CL_ABAP_CONV_OUT_CE: Instances of this class convert ABAP data objects to binary data. CL_ABAP_CONV_X2X_CE: Instances of this class enable the text data of various character sets and the numeric data of various number formats to be transformed. </td>
        </tr>
<tr>
            <td>Support for Files Greater than 2 Gigabytes</td>
            <td>Files greater than 2 gigabytes can now be read and written on all platforms that support large files. Only the platforms OS/390 and IBM System i (previously AS/400) are currently not included in this change. Additionally, the statement OPEN DATASET has been enhanced in such a way that, when opening a file, the file pointer also addresses positions greater than 2 gigabytes. </td>
        </tr>
<tr>
            <td>Limiting Data Range when Reading Files</td>
            <td>The amount of data to be read can be limited using the addition MAXIMUM LENGTH maxlen. Depending on the mode in which the file was opened, maxlen defines the maximum number of characters or bytes that are to be read from the file. </td>
        </tr>
<tr>
            <td rowspan="11">DEBUGGER</td>
            <td>Displaying Program Properties</td>
            <td>If Goto → Further Information → Program Properties is chosen in the menu, the system displays the properties Fixed Point Arithmetic, System Program, and Unicode Check. </td>
        </tr>
<tr>
            <td>Displaying Data References</td>
            <td>If a data reference dref is double-clicked in the Field Names column, information about the technical properties of the reference are displayed. If the Field Content column is double-clicked, the content of the data reference is displayed. The referenced object is displayed when the data reference dref-&gt;* is double-clicked in the Field Names column. </td>
        </tr>
<tr>
            <td>Special Display of Deep Data Objects</td>
            <td>The headers of strings, internal tables as well as data and object references are displayed if you place an asterisk at the beginning of the name. For example, to display the header of the internal table itab in hexadecimal form, enter *itab in the Field Name column. The field contains zeros if a table does not have a header. If the data object name is prefixed with an ampersand, the relevant reference of the strings, internal tables, data references, or object references is displayed in hexadecimal form. For example, specifying &amp;str displays the reference to the string str. </td>
        </tr>
<tr>
            <td>Setting Breakpoints at Methods</td>
            <td>A breakpoint can be set for a method by choosing Breakpoint → Breakpoint At → Method. </td>
        </tr>
<tr>
            <td>Support for Class-Based Exceptions</td>
            <td>The following functions are available when an exception is raised: If the addition INTO ref is missing from the statement CATCH, an exception object can be created in the debugger under Settings. If Display Exception Object is chosen, the attributes and interfaces of the exception class are displayed together with their content. If Statement that Caused the Exception is chosen, the line of the source code containing the statement that raised the exception is displayed. The statement is marked explicitly in the source code. </td>
        </tr>
<tr>
            <td>Debugging Mode for Business Server Pages with ABAP Scripting</td>
            <td>Business Server Pages (BSP) can now be run in debugging mode. It is also possible to display and set breakpoints there. Business Server Pages can be displayed in Object Navigator by selecting an appropriate application under BSP Applications. </td>
        </tr>
<tr>
            <td>Transferring Breakpoints to HTTP and Update Sessions</td>
            <td>If an HTTP session or update session is called from a logical unit of work (LUW), new work processes are started for these sessions. Any breakpoints set in this calling LUW are inherited by the new sessions and can be displayed by choosing Breakpoints. For example, if the update module func is called using CALL FUNCTION func IN UPDATE TASK, the new work process is displayed in a new GUI window, provided update debugging is selected under Settings in debugging mode. Here, all the breakpoints set in the calling LUW can be processed. </td>
        </tr>
<tr>
            <td>Displaying Statements for the Database Interface</td>
            <td>The most recent statements passed to the database now can be checked. Choose Goto → System → System Areas after the debugger is started and enter Area = OSQL in the input field. </td>
        </tr>
<tr>
            <td>Navigating Forwards and Backwards in the Source Code</td>
            <td>If Debugging - &gt; Goto Statement is chosen from the menu, the line containing the cursor is displayed. This allows simple forward and backward navigation within the displayed source code. </td>
        </tr>
<tr>
            <td>Displaying Memory Use</td>
            <td>If Settings → Memory Display On/Off is chosen from the menu, the memory used by objects, tables, references, or individual fields is displayed. It distinguishes between allocated and referenced memory and also displays the memory actually used by every data object. A hit list of the 25 data objects with the highest memory consumption can be displayed by choosing Goto → System → System Areas. The following values can be entered in the Area field: ITAB-TOP25 to display internal tables OBJ-TOP25 to display objects REF-TOP25 to display references ALL-TOP25 to display all data objects </td>
        </tr>
<tr>
            <td>Displaying the Structure Fragment View</td>
            <td>Choosing Goto → Display Data Object → System Information in the menu displays the structure fragment view. In this view, structures, internal tables, strings, data references, and objects are broken down into alignment gaps, character-like and byte-like areas, and all remaining types (such as P, I, F, strings, references, or internal tables). Adjacent, character-like components of a structure (with the exception of strings) are organized internally into the same group, but only if there are no alignment gaps between these components. All possible alignment requirements for characters are respected here. In the same way, adjacent byte-like components are also grouped together. </td>
        </tr>
<tr>
            <td rowspan="12">ITAB</td>
            <td>Creating a Table Object</td>
            <td>Any table objects can be created at runtime by using the statement CREATE DATA. If the table type is known in full, this is respected by the syntax check. </td>
        </tr>
<tr>
            <td>Checking the Convertibility of Key Fields</td>
            <td>In the case of the statements READ TABLE itab WITH KEY k1 = v1 .... kn = vn and DELETE TABLE itab WITH KEY = v1 .... kn = vn, the system already checks whether the key values are convertible to the component type in the syntax check. This convertibility check is also effective for the newer variant READ ... WITH TABLE KEY .... Previously, the runtime error MOVE_NOT_SUPPORTED was raised if the conversion was not possible. </td>
        </tr>
<tr>
            <td>Duplicate or Overlapping Key Specifications</td>
            <td>In the case of duplicate or overlapping key specifications, the statement READ TABLE itab WITH KEY k1 = v1 .... kn = vn now raises a syntax error instead of a syntax warning. If specified dynamically, it was previously the case that the last key specification was used; now the runtime error DYN_KEY_DUPLICATE is raised. </td>
        </tr>
<tr>
            <td>Support for Accessing Attributes</td>
            <td>The addition ... COMPARING o-&gt;attr now makes it possible to access the attributes of objects that are components of internal tables, similar to when reading, changing, deleting, or sorting internal tables. </td>
        </tr>
<tr>
            <td>Table Categories for Text Pools and IN Conditions</td>
            <td>Alongside standard tables, the tables categories HASHED and SORTED are supported for the statements READ, DELETE, and INSERT TEXTPOOL .... The IN conditions for SELECT ... WHERE and LOOP ... WHERE are also no longer dependent on the table category. </td>
        </tr>
<tr>
            <td>Handling Overflows in COLLECT and SUM</td>
            <td>Previously, fields of type f were only checked for possible overflows in the case of arithmetic additions. When using COLLECT and SUM, the addition was terminated in accordance with IEEE standards and a runtime error was raised as soon as the overflow limit (Infinity) was reached. Now this raises the exception CX_SY_ARITHMETIC_OVERFLOW, which can be caught using TRY. </td>
        </tr>
<tr>
            <td>Additions ASSIGNING and REFERENCE INTO</td>
            <td>The additions ASSIGNING and REFERENCE INTO are now available for the statements READ, LOOP, INSERT, APPEND, MODIFY, and COLLECT. Previously, only the addition ASSIGNING for the keywords LOOP and READ was available. </td>
        </tr>
<tr>
            <td>Uppercase and Lowercase Spelling in Dynamic Components</td>
            <td>Previously, the field content of a dynamic component always had to written in capitals, for example SORT ... BY ('COMP'). Now lowercase spelling is valid in specifications for all internal table statements. </td>
        </tr>
<tr>
            <td>Combined Key Specifications with table_line</td>
            <td>Now there are additional key specifications for the pseudo component table_line. Previously, it was not possible to have a key specified in the form READ ... WITH KEY table_line-&gt;attr = ... table_line = ... BINARY SEARCH, for example, where both the value of the attribute attr and the value of the reference itself were the key. </td>
        </tr>
<tr>
            <td>Establishing the Number of Lines in an Internal Table</td>
            <td>The number of lines in an internal table can be determined using the function LINES. For consistency, this function is intended to replace the statement DESCRIBE TABLE in the long term, since not all ABAP types can be described using DESCRIBE. </td>
        </tr>
<tr>
            <td>Optimization when Accessing Internal Tables</td>
            <td>For ABAP release 6.10, optimized access to tables of the types SORTED and HASHED has been introduced. For more details, see the section on Optimized Key Operations. In addition, in the case of assignments between internal tables of the same type, data is now only copied if changes were made to these tables. This table sharing has a positive effect on runtime and the memory required. Until now, these optimizations had to be carried out explicitly using field symbols. Now it is possible to work normally with a work area, because no table is copied when reads are performed. </td>
        </tr>
<tr>
            <td>Offset and Length Declarations for the Table Definition</td>
            <td>Previously, components of the table key could be specified with offset and length declarations, which resulted in undefined runtime behavior. This is no longer possible and a syntax error is raised instead. </td>
        </tr>
<tr>
            <td rowspan="8">KERNEL</td>
            <td>Restrictions on the Statement STOP</td>
            <td>The statement STOP can no longer be used outside of reports or in dynpros called using CALL SCREEN. Such actions previously produced a runtime error or, in rarer cases, a non-traceable program flow. </td>
        </tr>
<tr>
            <td>Addition ON ROLLBACK for Subroutines</td>
            <td>The addition ON ROLLBACK is now available for subroutines, similar to PERFORM ... ON COMMIT. This stops FORM routines of this type from being executed until a ROLLBACK WORK or a MESSAGE of type A is raised. </td>
        </tr>
<tr>
            <td>Overwriting of Untyped Field Symbols</td>
            <td>Untyped field symbols no longer lose their ready-only status when being read for the first time. Also, the fields %_DUMMY and %_SPACE are now constants. </td>
        </tr>
<tr>
            <td>Number of Global Segments</td>
            <td>The number of global segments in the program memory is now unrestricted. Previously, a maximum of 2**15 global segments was allowed. </td>
        </tr>
<tr>
            <td>Passing of c Literals to Numeric Parameters</td>
            <td>When passing c literals to parameters of type p, it was previously possible for a number to have more decimal places than the formal parameter. Due to rounding and the associated loss of information loss, the passing of c literals to parameters of types I, INT1, and INT2 also caused problems. For example, the literal '1245.6789' could be passed to a formal parameter of type p DECIMALS 2 or type i, and the rounding was performed in accordance with the associated conversion rule. Now the system ensures that the formal parameter has as large as a fractional portion as required by the literal. In the case of programs without fixed point arithmetic, the size of the fractional portion must be an exact match. </td>
        </tr>
<tr>
            <td>c Literals as Default Values for Parameters of Type p</td>
            <td>If, in the case of generic parameters of type p, a c literal is specified as the default value (for example in the form '12.345'), a field of type p with Decimals 0 was created and the literal value converted to this field. This could also result in information loss due to rounding. Now a field of type p is created with as many decimal places as specified in the literal. For example, for a c literal with the value '12.345', a P field with Decimals 3 is created. </td>
        </tr>
<tr>
            <td>Paging Out of the Structure SCREEN</td>
            <td>Until now the structure of the data object SCREEN was generated automatically in every program. In ABAP release 6.10 this structure description was paged out into the type pool SYSCR. Instead of data NAME type %_CX_SCREEN-Name the code should now be data NAME type SYSCR_SCREEN-NAME </td>
        </tr>
<tr>
            <td>Syntax Revisions in LOOP AT SCREEN and MODIFY SCREEN</td>
            <td>The following changes were made to the statements LOOP AT SCREEN and MODIFY SCREEN. To ensure independence from global definitions, the additional variants LOOP AT SCREEN INTO wa and MODIFY SCREEN FROM wa were introduced. LOOP AT SCREEN with an internal table SCREEN defined locally in the program is no longer possible. The following program, previously not terminated until runtime, now produces a syntax error: PROGRAM loop_test. PERFORM test. FORM test.   DATA screen TYPE STANDARD TABLE OF spfli.   LOOP AT SCREEN.   ENDLOOP. ENDFORM. </td>
        </tr>
<tr>
            <td rowspan="3">LISTS</td>
            <td>Standard Spool Dialog Box Simplified</td>
            <td>The spool dialog box has been reduced to the most important spool parameters. Further parameters can be selected on the following Spool Request Attributes screen. The user can apply options from this screen to the initial screen by double-clicking a parameter. Further spool options are available for printers with an active list driver on the Printed Page tab. </td>
        </tr>
<tr>
            <td>New Addition FRAMES OFF of the Statement FORMAT</td>
            <td>The addition FRAMES OFF prevents the minus (-) and vertical bar (|) characters from being transformed to parts of frames, if adjacent. In archiving, this addition also ensures that a line consisting of minus signs is saved unchanged and in ASCII format. </td>
        </tr>
<tr>
            <td>Extended Options for Configuration of the Spool Dialog Box</td>
            <td>The program RSPRIPARADMIN can be used to make spool settings that apply to all users. </td>
        </tr>
<tr>
            <td rowspan="19">MISC</td>
            <td>Dynamic Pass by Parameter in CALL FUNCTION</td>
            <td>The addition PARAMETER-TABLE itab can be used to fill the interface of a function module with parameters dynamically. The addition EXCEPTION-TABLE itab is used to pass a table of exceptions to the called function module. At the same time, the number value to be assigned to the system field sy-subrc after the exception is raised is passed to the component VALUE. </td>
        </tr>
<tr>
            <td>New Predicate Expression IS BOUND</td>
            <td>The predicate expression IS BOUND indicates whether a reference variable contains a valid reference. </td>
        </tr>
<tr>
            <td>Negated Relational Operators</td>
            <td>A new word order has been introduced for the negation of the following relational operators. Instead of IF NOT (f IS INITIAL), the spelling IF f IS NOT INITIAL is now possible. This word order is available for the following relational operators: [NOT] BETWEEN [NOT] IN IS [NOT] INITIAL IS [NOT] ASSIGNED IS [NOT] REQUESTED IS [NOT] SUPPLIED </td>
        </tr>
<tr>
            <td>Unconditional Exit from a Processing Block</td>
            <td>The new statement RETURN can be used to exit a processing block explicitly. </td>
        </tr>
<tr>
            <td>Extended Search in the Keyword Documentation</td>
            <td>The keyword documentation has been enhanced in such a way that more general terms like RADIOBUTTON, INNER JOIN, or FOR ALL ENTRIES are now also processed. Until now the system only searched for keywords like DATA or CLASS. If a number of documents contain the same term, the search can be limited in the hit list. A further improvement is that the system displays the found document at the relevant point. </td>
        </tr>
<tr>
            <td>Runtime Analysis</td>
            <td>From now on, the runtime analysis can also be called within a program unit by calling the methods ON or OFF from the class CL_ABAP_TRACE_SWITCH. Additionally, the measurement restrictions now have an input help for program types and the Tips and Tricks interface has been completely redesigned and now includes a tree structure and a text editor. </td>
        </tr>
<tr>
            <td>Calling Predefined Arithmetic Functions</td>
            <td>In expressions, the arithmetic functions can be used at the same positions as functional methods. The following cases are supported: Source fields of the MOVE statement Arithmetic expressions of the COMPUTE statement Logical expressions The CASE statement of the CASE control structure The WHEN statement of the CASE control structure The WHERE condition in the LOOP AT statement </td>
        </tr>
<tr>
            <td>Transformation of XML Data into ABAP Variables</td>
            <td>Another new addition is the CALL TRANSFORMATION statement, which enables the transformation of XML data into ABAP variable content. The following transformations are possible: XML to ABAP ABAP to XML ABAP to ABAP XML to XML </td>
        </tr>
<tr>
            <td>Displaying the Generation Limits in the Program Check</td>
            <td>When choosing Program → Check → Generation Limits in the ABAP Editor menu, the system displays the load size and the available kernel resources for the selected program. </td>
        </tr>
<tr>
            <td>Increase in Line Length in ABAP Editor</td>
            <td>From now on, ABAP programs support a line length of 255 characters instead of 72 characters. Literals or statement lists that exceed a line with 72 characters now produce a syntax error. However, this only affects programs that are modified in an editor with a line length of more than 72 characters. </td>
        </tr>
<tr>
            <td>Enhancement of the MESSAGE Statement</td>
            <td>The form MESSAGE msg TYPE t is a new in the statement MESSAGE. It produces direct output of character strings or other strings. This option is designed for exception classes that have exception texts of data type string. The new addition DISPLAY LIKE can also be used to modify the icons in the display of the message. </td>
        </tr>
<tr>
            <td>Displaying Exceptions for Runtime Errors</td>
            <td>The display of runtime errors now includes the columns Name of Runtime Error and Exception. The same fields are also displayed and explained in the short dump, immediately after an uncaught runtime error. </td>
        </tr>
<tr>
            <td>Separate Logical Units of Work for Application and Generation</td>
            <td>If a program was not modified by its own logical unit of work (LUW), generation is started in a separate work process when the program is used. The program is generated in the same work process only if no other work process is free. If, for example, modified programs were used in a background process, these were only accessible to other users after the background process was finished. After the program was generated, the system could not trigger a COMMIT WORK, since this may have caused inconsistencies in the background processing data. The separation of application and generation now ensures that a program is available to other applications immediately after it is generated. </td>
        </tr>
<tr>
            <td>Minimum and Maximum Values for Elementary Data Types</td>
            <td>The class CL_ABAP_EXCEPTIONAL_VALUES provides the methods GET_MAX_VALUE and GET_MIN_VALUE, which are used to establish the value ranges of the elementary data types. </td>
        </tr>
<tr>
            <td>Name for a Component in the Structure Buffer</td>
            <td>Previously, if you used &lt;ASSIGN COMPONENT to access components of a Dictionary structure, then up to now the statement DESCRIBE FIELD ... HELP-ID returned the name of the corresponding data element. The name of the component of the dictionary structure is now returned instead. Example The following code snippet now produces the value TRDIR-NAME instead of PROGRAMM. DATA: mydir     TYPE trdir,       hlpid(61) type c. FIELD-SYMBOLS &lt;fs&gt; TYPE ANY. ASSIGN mydir TO &lt;fs&gt;. ASSIGN COMPONENT 1 OF STRUCTURE &lt;fs&gt; TO &lt;fs&gt;. DESCRIBE FIELD &lt;fs&gt; HELP-ID hlpid. WRITE / hlpid. </td>
        </tr>
<tr>
            <td>Reading a Program Title</td>
            <td>Previously, the program title was read from the ABAP text elements using the statement READ TEXTPOOL. For performance and buffering reasons, the program title is now read from the table TRDIRT. </td>
        </tr>
<tr>
            <td>New Medium for Data Clusters</td>
            <td>The medium SHARED MEMORY has been added to the statements EXPORT and IMPORT. Unlike the variant SHARED BUFFER, whose memory is automatically modified after a displacement, memories must be managed explicitly if this type of repository is used. To do this, the statement DELETE FROM SHARED MEMORY or a method of the class CL_ABAP_EXPIMP_SHMEM can be used. </td>
        </tr>
<tr>
            <td>Selecting and Deleting Data Clusters</td>
            <td>There are new methods in the following classes for selecting and deleting data clusters: CL_ABAP_EXPIMP_MEM CL_ABAP_EXPIMP_SHMEM CL_ABAP_EXPIMP_SHBUF CL_ABAP_EXPIMP_DB </td>
        </tr>
<tr>
            <td>New Additions for INSERT REPORT</td>
            <td>The following new additions are available for the statement INSERT REPORT: ... KEEPING DIRECTORY ENTRY ... DIRECTORY ENTRY trdir ... UNICODE ENABLING uc ... FIXED-POINT ARITHMETIC fp ... PROGRAM TYPE pt ... MAXIMUM WIDTH INTO w </td>
        </tr>
<tr>
            <td rowspan="10">OBJECTS</td>
            <td>Dynamic Access: Dynamic Access to Object Attributes</td>
            <td>The following variants of the statement ASSIGN make it possible to access the attributes of classes dynamically: assign oref-&gt;(f) to &lt;fs&gt;. assign iref-&gt;(f) to &lt;fs&gt;. assign (f1)=&gt;(f2) to &lt;fs&gt;. assign c=&gt;(f) to &lt;fs&gt;. assign (f)=&gt;f to &lt;fs&gt;. The attribute search is first carried out for the static type. If the search was unsuccessful or the attributes are not visible in the context, the system performs a search for the dynamic type. </td>
        </tr>
<tr>
            <td>CALL METHOD Optional when Calling Methods</td>
            <td>In static method calls of the form CALL METHOD meth( ), the CALL METHOD expression is optional. It is sufficient to simply write meth( ). </td>
        </tr>
<tr>
            <td>Friends - Friendship Between Classes</td>
            <td>The Friends concept means that a class can offer friendship to other classes or interfaces (GLOBAL FRIENDS and LOCAL FRIENDS are possible here). These friends can then access all the components of the provider class, and can always instantiate the class. The PROTECTED and PRIVATE components always become PUBLIC for friends. Classes offer friendship to other classes or interfaces using the addition FRIENDS of the statement CLASS ... DEFINITION. The new concept makes the previous language construction DATA TYPE REF TO class %_friend obsolete. </td>
        </tr>
<tr>
            <td>New Additions for Implementing Interfaces</td>
            <td>The statement INTERFACES has the new additions ABSTRACT | FINAL METHODS and ALL METHODS ABSTRACT | FINAL, which make it possible to make methods in the implementing class abstract or final. DATA VALUES is another new addition. It assigns start values to interface attributes when implemented in a class. </td>
        </tr>
<tr>
            <td>Pass by Parameter in Dynamic Instantiations</td>
            <td>Parameters can now be passed and classic exceptions handled in the dynamic variant CREATE OBJECT (class). </td>
        </tr>
<tr>
            <td>Object Transactions</td>
            <td>In Transaction Maintenance, you can classify a transaction code as an object transaction (OO transaction). The transaction code is then linked with a global or local class of a program, either using the transaction service of ABAP - Object Services for persistent objects, or using any method. When this type of transaction is called, the system loads the program linked with the class, creates an object for instance methods, and executes the method. </td>
        </tr>
<tr>
            <td>Enhanced Syntax for Interfaces</td>
            <td>All the component interfaces of a compound interface are handled in the same way in the interface and in the implementing class. This means that the components of inner interfaces can be accessed directly using interface references. Previously, this was only possible using class reference variables. INTERFACE i1.   DATA a1. ENDINTERFACE. INTERFACE i2.   INTERFACES i1.   DATA a2 LIKE i1~a1. ENDINTERFACE. DATA iref TYPE REF TO i2. WRITE iref-&gt;i1~a1. In previous releases, this example would have caused a syntax error with the expressions containing i1~a1 and it would have been necessary to use aliases. In ABAP release 6.10, however, the interface component selector (~) should only be used outside classes or interfaces in exceptional circumstances. </td>
        </tr>
<tr>
            <td>Enhanced Syntax for Event Handling</td>
            <td>In the statement SET HANDLER, the system now checks the exact type of the reference variable that points to a triggering object. The type must be of the same class or subclass as the one specified in the declaration of event handlers as METHODS after FOR EVENT evt OF. This means objects of superclasses cannot be registered, even if the event was inherited from a superclass. If an interface has been declared after FOR EVENT evt OF, the type of the reference variable must either be the interface itself, a class or subclass that implements the interface, or another interface that contains the interface as a component. From ABAP release 6.10 onwards, the type of the implicit event parameter sender (which can be imported by a handler method and passed using RAISE EVENT) is determined using the class or interface specified in the declaration of the event handler (after the addition FOR EVENT OF of the statement METHODS). In previous releases, the type was determined by the class or interface where the event was declared using EVENTS. </td>
        </tr>
<tr>
            <td>Enhanced Syntax in the Instance Constructor of Subclasses</td>
            <td>In the instance constructor of subclasses, the constructor of the superclass must always be called using [CALL METHOD] super-&gt;constructor, even if this superclass was not explicitly defined there. Direct subclasses of the root class OBJECT are the only exception. This means instance constructors can be implemented in superclasses retroactively, without invalidating the subclasses. If the superclass constructor is not called, the following program produces a syntax warning in ABAP release 6.10 and higher. Example Calling the superclass constructor. CLASS c1 DEFINITION INHERITING FROM object.   PUBLIC SECTION.     ... ENDCLASS. CLASS c1 IMPLEMENTATION.   ... ENDCLASS. CLASS c2 DEFINITION INHERITING FROM c1.   PUBLIC SECTION.     METHODS constructor. ENDCLASS. CLASS c2 IMPLEMENTATION.   METHOD constructor.     ...     super-&gt;constructor( )     ...   ENDMETHOD. ENDCLASS. </td>
        </tr>
<tr>
            <td>Dynamic Access to Interface Constants</td>
            <td>Interface constants can now be accessed dynamically using intf=&gt;const. To make this possible, the logic for dynamic ASSIGN, dynamic access, and dynamic invoke was changed so that global classes are now hidden by local types or interfaces. In previous releases, if constants were accessed using class=&gt;attr, the system only searched for class names. </td>
        </tr>
<tr>
            <td rowspan="8">REFERENCES</td>
            <td>Typing Using TYPE DATA</td>
            <td>TYPE DATA was previously handled like TYPE REF TO DATA and were therefore completely typed. Now TYPE DATA can only be used for formal parameters and field symbols; otherwise a syntax error occurs. A non-generic type can now be specified after REF TO. </td>
        </tr>
<tr>
            <td>Specifying Types for CREATE</td>
            <td>A type no longer needs to be specified for the statement CREATE DATA ... if the reference is completely typed. In this case, the new data object is given the type of the reference. </td>
        </tr>
<tr>
            <td>Uppercase and Lowercase in Dynamically Specified Types</td>
            <td>In the statement CREATE DATA ..., previously only uppercase letters could be used for the field content of dynamically specified types. In ABAP release 6.10, lowercase letters can also be used. Initially the system searches using the specified field content; if this search fails, the system searches again using uppercase letters. If this search also fails, a runtime error occurs. </td>
        </tr>
<tr>
            <td>Defining Typed Data References</td>
            <td>In the case of the statements TYPES and DATA, a fixed type can now be specified for the addition REF TO. </td>
        </tr>
<tr>
            <td>Casting Data References</td>
            <td>The introduction of typed data references enables downcasts in assignments between data reference variables. Downcasts must be expressed using a special assignment operator, ?=. Example Downcasts for data references. DATA:   d1 TYPE REF TO data,  "Generic   d2 TYPE REF TO i.     "Typed d1  = d2. d2 ?= d1. </td>
        </tr>
<tr>
            <td>Additional Type Information for ASSIGN dref-&gt;*</td>
            <td>If a data reference has fixed typing, it passes on its additional attributes if it is assigned to an untyped data reference. DATA:   dataobj   TYPE dtel_1,   dataref_1 TYPE REF TO dtel_2,   dataref_2 TYPE REF TO data. FIELD-SYMBOLS &lt;fs&gt; TYPE ANY. GET REFERENCE OF dataobj TO dataref_1. dataref_2 = dataref_1. ASSIGN dataref_2-&gt;* TO &lt;fs&gt;. In this case, dataref_1-&gt;*, dataref_2-&gt;*, and &lt;F&gt; inherit the attributes of the dictionary data element DTEL_2. </td>
        </tr>
<tr>
            <td>Any Typing for CREATE DATA</td>
            <td>New types, such as data references and internal tables, can be constructed when data objects are created using the statement CREATE DATA. Previously, only references to existing types were possible. </td>
        </tr>
<tr>
            <td>Dereferencing in Any Operand Positions</td>
            <td>If a data reference variable is completely typed, it can be dereferenced in any operand position, using the dereferencing operator -&gt;*. Example Dereferencing data references. DATA dref TYPE REF TO i. ... dref-&gt;* = dref-&gt;* + 1. </td>
        </tr>
<tr>
            <td rowspan="6">STRINGS</td>
            <td>Identify the Length and Number of a Character</td>
            <td>New functions: The function charlen provides the length of the first character of a string or of a character-like field. numofchar can be used to obtain the number of characters in a string or a character-like field. dbmaxlen provides the maximum length of the string as stored in ABAP Dictionary. </td>
        </tr>
<tr>
            <td>New Statements FIND and REPLACE</td>
            <td>There is a new statement, FIND, for searching in character strings. This replaces the SEARCH statement. For replacing characters in character strings, the statement REPLACE has been expanded to include position-based replacements. </td>
        </tr>
<tr>
            <td>Faster Access to Strings</td>
            <td>Offset/length access is now the fastest way to process a string character by character. This technique is also faster than searching in a field of type C that is assigned to a field symbol. </td>
        </tr>
<tr>
            <td>Support for Strings in the Database</td>
            <td>From ABAP release 6.10, character strings and binary data can be stored in database columns of types STRING or RAWSTRING. The system distinguishes short strings from long strings: Short strings consist of a maximum of 256 characters, do not have trailing blanks, and can be compared on the database. Long strings can be of any length and do have trailing blanks; however they cannot be compared on the database. When working with strings, some restrictions have to be observed. Further details are available here. </td>
        </tr>
<tr>
            <td>Definition of String Constants</td>
            <td>Strings can now also be defined as constants and can be given an initial value using the keyword VALUE. CONSTANTS str1 TYPE string VALUE 'ABC'. DATA      str2 TYPE string VALUE 'XYZ'. str2 = str1. str1 = str2.              "Syntax error WRITE: / str1, str2. </td>
        </tr>
<tr>
            <td>Introduction of Text String Literals</td>
            <td>Text string literals are enclosed by backquotes in the form str = `ABC`. Text string literals are of data type string and trailing blanks are not ignored, unlike in text field literals. Example Text string literals. DATA: str1 TYPE string VALUE 'ABC  ',       str2 TYPE string VALUE `ABC  `,       cnt1 TYPE i,       cnt2 TYPE i. cnt1 = strlen( str1 ). cnt2 = strlen( str2 ). WRITE: / cnt1, cnt2. The length for the string str1 is cnt1 = 3 and the length for the string str2 is cnt2 = 5. </td>
        </tr>
<tr>
            <td rowspan="2">SYSTEM</td>
            <td>Content of the System Field sy-calld</td>
            <td>The field sy-calld should contain space if the program is the first and only program in a call sequence. sy-calld should contain X if the program is a called program in a call sequence. Previously, sy-calld was always set to X for SUBMIT without RETURN, even though this meant that the internal session or the position in the call sequence of the caller is replaced by the called program. If the calling program was the first program in the call sequence, this produced errors in the application logic. From now on, sy-calld is not affected by SUBMIT without RETURN, and retains the value of the calling program whose internal session is being replaced. </td>
        </tr>
<tr>
            <td>Replacement of the System Field sy-repid</td>
            <td>The system field sy-repid is no longer a component of the structure SY in the ABAP program or the structured type SYST in ABAP Dictionary. Instead, from ABAP release 6.10, every program contains the built-in constants sy-repid and syst-repid, which both contain the name of the current program. There are also two built-in types sy-repid and syst-repid. The obsolete type references LIKE syst-repid and TYPE sy-repid are, therefore, still possible. In addition to considerable performance improvements in the external procedure call, this new feature also has the advantage that sy-repid can now also be passed as a parameter to external procedures. The formal parameter is set to the name of the caller and not to the name of the compilation unit, which means that a helper variable is no longer required. This incompatible change can cause problems if ABAP programs refer to the previous structure syst, for example: DATA my_syst type syst. ... my_syst-repid ... ASSIGN COMPONENT 'REPID' OF STRUCTURE sy TO ... </td>
        </tr>
<tr>
            <td rowspan="4">TYPES</td>
            <td>Constants for Structures and Internal Tables</td>
            <td>Constants can now be defined as structures and internal tables, which in turn contain internal tables or references and strings. Only the initial value is possible however. Example Constant structure. types:   begin of STRUC,     ITAB type standard table of SPFLI with non-unique key CARRID,     MREF type ref to OBJECT,   end of STRUC. constants:   CONST  type STRUC value is initial. </td>
        </tr>
<tr>
            <td>New Generic Types for Field Symbols and Formal Parameters</td>
            <td>The new generic types can be used for typing field symbols and parameters in subroutines, function modules, or methods. The following table shows their variants: Generic Type Types simple clike, xsequence, numeric clike n, d, t, struc1, csequence csequence c, string xsequence x, xstring numeric i, s, b, p, f </td>
        </tr>
<tr>
            <td>Methods for Displaying Specific Type Properties</td>
            <td>The method GET_PROPERTY, which provides information about specific type properties at runtime, has been added to the class CL_ABAP_TYPEDESCR. </td>
        </tr>
<tr>
            <td>Type Description Objects for All Friends of a Class</td>
            <td>The method GET_FRIEND_TYPES was introduced in the class CL_ABAP_CLASSDESCR. This can be used to query the type description objects for all friends of a class at runtime. </td>
        </tr>
  </table>
</details>  
<br>
<details>
  <summary>🟢 Release 30</summary>
  <!-- -->
<br>
  <table>
        <tr>
            <th>Topic</th>
            <th>Title</th>
            <th>Details</th>
        </tr>
<tr>
            <td rowspan="8">ABAP_SQL</td>
            <td>Dynamic WHERE Condition in SELECT</td>
            <td>The WHERE condition can be specified partly or fully in an internal table. This means that WHERE conditions can be constructed dynamically at runtime. Unlike a RANGES table, an internal table contains a WHERE condition as text. The internal table can then be accessed using SELECT ... WHERE (itab) or SELECT ... WHERE sql_cond AND (itab). Here, itab stands for the name of the internal table and sql_cond for the statically specified part of the WHERE condition. Examples and further information can be found in the documentation for the WHERE clause. </td>
        </tr>
<tr>
            <td>Specification of the Name of the Database Table or of the View at Runtime in ABAP SQL</td>
            <td>When using SELECT, INSERT, UPDATE, MODIFY and DELETE: The name of a database table or a view can be specified dynamically as the content of a field. Instead of specifying the table name statically in the source code, a field name in brackets is given. The content of this field is then interpreted as the table name. Examples and further information can be found in the documentation for the FROM clause. </td>
        </tr>
<tr>
            <td>Return of DUPREC Errors from INSERT ... FROM TABLE itab</td>
            <td>In cases where one or more rows cannot be inserted because rows with the specified keys already exist, a runtime error always occurred in the past. The addition ... ACCEPTING DUPLICATE KEYS has the effect of setting the return code SY-SUBRC to 4 rather than aborting the process. The remaining rows are then added after the statement has been executed. Further information can be found in the documentation of the statement INSERT. </td>
        </tr>
<tr>
            <td>Union of Solution Sets in SELECT with FOR ALL ENTRIES in itab</td>
            <td>A SELECT statement with ...FOR ALL ENTRIES IN itab WHERE sql_cond forms the union of solution sets of all SELECT statements produced when the fields of the internal table itab referenced in the WHERE condition are replaced by the corresponding values of a table row. This variant is very useful if, for example, an internal table is filled with composite primary keys. All corresponding database rows can be selected with a single SELECT statement. This technique avoids the need for a loop containing a SELECT SINGLE ... for each row of the internal table. Examples and further information can be found in the documentation for the WHERE clause. </td>
        </tr>
<tr>
            <td>Database Rows Read Package-by-Package in SELECT with PACKAGE SIZE n</td>
            <td>SELECT ... INTO TABLE itab PACKAGE SIZE n places the selected rows in the internal table in packages of n rows rather than all at once. Each new package overwrites the content of itab. This is a good way of making sure that the internal table does not get too big. If PACKAGE SIZE is used together with SELECT ... APPENDING TABLE itab, the previous content of itab is preserved and each new package is added at the end of the table. Examples and further information can be found in the documentation for the INTO clause. </td>
        </tr>
<tr>
            <td>Explicit Cursor Processing</td>
            <td>The statements OPEN CURSOR, FETCH, and CLOSE CURSOR enable nested processing of one or more database tables without the need to keep redefining the datasets. By using the addition WITH HOLD, a cursor can be opened which is then preserved across database commits. Examples and further information can be found in the documentation for the ABAP statements OPEN CURSOR, FETCH, and CLOSE CURSOR. </td>
        </tr>
<tr>
            <td>SELECT List with Aggregate Functions in SELECT</td>
            <td>Alongside SELECT *, SELECT COUNT( * ), and SELECT SINGLE *, the aggregate functions MIN, MAX, SUM, COUNT, and AVG can be specified plus fields of the database table in the SELECT list. Specifying DISTINCT causes duplicate rows to be removed automatically from the solution set. Examples and further information can be found in the documentation for the SELECT statement. </td>
        </tr>
<tr>
            <td>INTO List in SELECT and FETCH</td>
            <td>If the SELECT list specifies individual columns, the INTO clause can include a list of ABAP fields of equal length to be used as the target area. Examples and further information can be found in the documentation for the INTO clause. </td>
        </tr>
<tr>
            <td rowspan="5">DEBUGGER</td>
            <td>Debugging System Dynpros and System Programs</td>
            <td>There are now special procedures for handling system programs and system dynpros in ABAP debugging. (System programs are programs with the status 'S' in the program properties, system dynpros are the dynpros of system programs or dynpros that contain only system modules (module name SYST-...).) Examples for system dynpros: list dynpro, selection screen dynpro, .... System programs and system dynpros must be debugged in system debugging. This can be accessed from anywhere in the system by choosing System → Utilities → Debugging System or by choosing Settings → System Debugging from the debugger. </td>
        </tr>
<tr>
            <td>Debugging ABAP Lists</td>
            <td>It is now possible to track how a list is built. As soon as a list is created, it can be displayed by pressing the Display List pushbutton in debugging. The display shows all existing lines with their formats. Exception: The only exception is the current line, which is not formatted until after NEW-LINE. </td>
        </tr>
<tr>
            <td>Table Editor</td>
            <td>In the display for internal tables, it is now also possible to change the displayed table. Rows can be changed, inserted, appended, and deleted. </td>
        </tr>
<tr>
            <td>Structure Expansion</td>
            <td>In the structure and table displays, structures from fields and tables passed from external programs are now expanded. </td>
        </tr>
<tr>
            <td>Program Overview</td>
            <td>A program overview is displayed. This lists all subroutines, functions, modules, and events belonging to the current program. The program source code can be displayed by double-clicking a line. </td>
        </tr>
<tr>
            <td rowspan="28">LDB</td>
            <td>PARAMETERS as Radio Buttons</td>
            <td>Addition RADIOBUTTON GROUP group with PARAMETERS. By using this addition, PARAMETERS can be combined together in radio button groups on the selection screen. For further information, see PARAMETERS. </td>
        </tr>
<tr>
            <td>Checkbox Parameters</td>
            <td>PARAMETERS now has an addition called AS CHECKBOX which can be used to display a parameter on the selection screen as a checkbox. </td>
        </tr>
<tr>
            <td>Matchcode Selection with Logical Databases</td>
            <td>The addition AS MATCHCODE STRUCTURE of PARAMETERS makes it possible to use matchcode selection for a logical database. The standard selection screen contains a box with input fields for matchcode ID and search string. In the database program, selected records can be processed in the subroutine PUT_xxx_MATCHCODE. Matchcode selections are a particularly good way of improving performance, since they can be used to restrict the amount of data selected significantly. </td>
        </tr>
<tr>
            <td>SUBMIT rep WITH SELECTION-TABLE rspar.</td>
            <td>The addition WITH SELECTION-TABLE rspar makes it possible to specify values passed by WITH clause to the parameters or selection criteria of a report in an internal table. Here, the table rspar has the structure of RSPARAMS. The function module RS_REFRESH_FROM_SELECT_OPTIONS can be used to fill the table with the content of the current parameters or selection criteria (this FM replaces the keyword REFRESH respar FROM SELECT-OPTIONS). For further information, see the documentation on SUBMIT. </td>
        </tr>
<tr>
            <td>Self-Programmed F1 and F4 on Selection Screens</td>
            <td>Self-programmed input help (F4) and field help (F1) is now available for both database-specific and report-specific parameters and selection criteria. For database-specific objects, this is done using an addition (VALUE-REQUEST ... or HELP-REQUEST ...) with PARAMETERS and SELECT-OPTIONS. For report-specific objects, the event to be processed is specified directly at F1 or F4 (AT SELECTION-SCREEN ON VALUE-REQUEST FOR ... or AT SELECTION-SCREEN ON HELP-REQUEST FOR ...). </td>
        </tr>
<tr>
            <td>Date Calculations in Variants</td>
            <td>In addition to the variables available in table TVARV for parameters or selection criteria in variants (for retrieving values from table TVARV at runtime), it is now possible to calculate dates or periods for parameters/selection criteria of the type date at runtime. Examples include current date, last day of previous month, and first quarter of current year. </td>
        </tr>
<tr>
            <td>SUBMIT: Runtime Information from Function Module</td>
            <td>The function module RS_SUBMIT_INFO returns information about the current report execution process in the structure RSSUBINFO. It is specifically intended for use during the processing of selection screens. It indicates, for example, whether the selection screen is processed when the report is executed, when variants are defined, or when scheduling a background job. This is important, for example, if the user uses his or her own GUI status on the selection screen. </td>
        </tr>
<tr>
            <td>New OK Code Field SSCRFIELDS-UCOMM on Selection Screens</td>
            <td>The previous OK code field SY-UCOMM had the drawback of being destroyed by every CALL SCREEN. For this reason, it has been replaced by the field SSCRFIELDS-UCOMM from ABAP Dictionary. Unlike SY-UCOMM, the structure SSCRFIELDS must be declared using TABLES when reading or manipulating SSCRFIELDS-UCOMM. Only the procedure for setting SY-UCOMM in the program has changed; SY-UCOMM must now be set instead. Setting SY-UCOMM in the program, however, no longer meets the intended purpose, and SSCRFIELDS-UCOMM now needs to be set instead. </td>
        </tr>
<tr>
            <td>Function Modules: User's Own GUI Status on Selection Screen</td>
            <td>Function modules RS_SET_SELSCREEN_STATUS and RS_EXTERNAL_SELSCREEN_STATUS. These function modules make it possible to set a separate status for the selection screen or to deactivate function codes from the standard status (for example, if the 'Print' function for the report is not required). While the function module RS_SET_SELSCREEN_STATUS requires the status in question to belong to the user interface for the report, the function module RS_EXTERNAL_SELSCREEN_STATUS makes it possible to set a status that has been defined externally in a function pool. When a separate status is set, it is best to first get information about the current situation using the function module RS_SUBMIT_INFO (see point 6). In this way, the 'Execute' key is not provided instead of the 'Save' key when defining variants. </td>
        </tr>
<tr>
            <td>Size of Selection Screens Increased to 200 Lines</td>
            <td>Selection screens can now be up to 200 lines long. Since the scroll bar can be used to scroll within a screen, the continuation screens 1001, 1002 ... are no longer necessary. Benefits: Since all SELECT-OPTIONS and PARAMETERS are on one screen, it is no longer necessary to check carefully that the incorrect field is actually on the current screen when handling errors. Also, the addition 'OBLIGATORY' now applies to all SELECT-OPTIONS or PARAMETERS that were previously on a subsequent screen. This reduces the total number of generated selection screens Necessary actions: In theory, none. However, new pages previously forced by the SELECTION-SCREEN NEW-PAGE statement may have to be replaced by something else, such as a block (see also points 13 and 14). </td>
        </tr>
<tr>
            <td>SELECTION-SCREEN NEW-PAGE is No Longer Used.</td>
            <td>Since continuation screens are no longer used (see point 12), the keyword SELECTION-SCREEN NEW-PAGE is no longer required, but is retained for syntax reasons. However, it now starts a new line instead of a new page. </td>
        </tr>
<tr>
            <td>Selection Screen Versions Instead of Template Screens</td>
            <td>The option to define non-standard selection screens by specifying the number of a model screen (belonging to the database access program SAPDBxyz) in the report properties no longer exists. This has been replaced by selection screen versions. Like model screens, these have a three-character name that is specified on the report properties screen. By pressing F4 here, it is possible to get a list of all the selection screen versions for the underlying logical database. The selection screen versions are defined in the database INCLUDE DBxyzSEL using the expressions SELECTION-SCREEN, BEGIN/ END OF VERSION, and SELECTION-SCREEN EXCLUDE .... The latter is used to specify any objects to be excluded from the selection screen version. </td>
        </tr>
<tr>
            <td>Blocks (with or without Boxes) on Selection Screens</td>
            <td>The expression SELECTION-SCREEN BEGIN/END OF BLOCK block is used to define logical blocks on selection screens. The addition ... WITH FRAME makes it possible to create a box round the block and, for each TITLE addition, a text can be defined for the box. If the new event AT SELECTION-SCREEN ON BLOCK block contains an error dialog, only the fields in that block become ready for input. The boxes previously created round the database-specific or report-specific parts of the selection screen are no longer required, since the new expressions allow better and more individual grouping options. </td>
        </tr>
<tr>
            <td>Ability to Set Comments on Selection Screens at Runtime</td>
            <td>Besides numbered texts, the expression SELECTION-SCREENCOMMENT now makes it possible to specify field names up to eight characters long. These fields are then created automatically as output fields on the selection screen. The texts must be defined in these comment fields at runtime and can be changed while processing the selection screen. </td>
        </tr>
<tr>
            <td>User's Own Pushbuttons on Selection Screens</td>
            <td>The key word SELECTION-SCREEN PUSHBUTTON ...USER-COMMAND ucom makes it possible to create pushbuttons on the selection screen. As with SELECTION-SCREEN COMMENT, the text can be defined either statically in the form of a text symbol or dynamically by specifying a field name up to eight characters long. The field SSCRFIELDS-UCOMM can be read using the accompanying user command ucom in AT SELECTION-SCREEN. </td>
        </tr>
<tr>
            <td>Dynamic Selections on Selection Screens</td>
            <td>In the database INCLUDE DBxyz, the logical database and the statement SELECTION-SCREENDYNAMIC SELECTIONS FOR TABLE dbtab can be used to define 'dynamic selections' for the table dbtab of the logical database xyz. In this case, the 'Dynamic Selections' key appears on the selection screen, if the table dbtab is used in the report. After pressing this key, the user can enter selections for the fields expected by the logical database. The result is passed directly from the logical database to the SELECT statement. Hint The set of fields for which dynamic selections are to be allowed can be defined by the logical database in the form of a selection view. These are defined in the workbench or in the editing functions of logical databases. To aid identification, the origin ('SAP' or 'CUS' for 'customer'), the name of the logical database, and a name can be used (which must always be 'STANDARD' for the described functions on the selection screens). When reading the field list, the system first searches for the set with the origin 'CUS'. If it finds nothing, it then searches for the origin 'SAP'. This allows customers to define the optimal set to suit their requirements, if the SAP standard is not suitable. </td>
        </tr>
<tr>
            <td>Ability to Set Box Texts at Runtime</td>
            <td>The title of a box defined using SELECTION-SCREEN BEGIN OF BLOCK ... WITH FRAME TITLE ... can be set and modified at runtime. </td>
        </tr>
<tr>
            <td>Symbolic Positions with SELECTION-SCREEN Statements</td>
            <td>In all variants of SELECTION-SCREEN which expect format specifications, the position of the object can be defined on the selection screen symbolically (POS_LOW or POS_HIGH for the positions where the input fields of SELECT-OPTIONS are produced). </td>
        </tr>
<tr>
            <td>User's Own Key in Application Toolbar</td>
            <td>The keyword SELECTION-SCREEN FUNCTION KEY n (n must be between 1 and 5) makes it possible to activate up to five function keys on the selection screen. The texts for these must be set at runtime. The keys appear in the application toolbar. </td>
        </tr>
<tr>
            <td>Nested Blocks on Selection Screens</td>
            <td>Blocks defined using the language construct SELECTION-SCREEN BEGIN/ END OF BLOCK can be nested. For blocks in boxes, the nesting depth is restricted to 5 levels. </td>
        </tr>
<tr>
            <td>Return to the Selection Screen After F3 in Basic List</td>
            <td>If F3 is used to exit a basic list created by a program (and the program was executed from the selection screen), the system displays the selection screen again with the old content. </td>
        </tr>
<tr>
            <td>Passing Report-Specific Selections to the Database</td>
            <td>Report-specific selections with a reference field belonging to a logical database table for which dynamic selections are defined are passed directly to the database. In exceptional cases, this can be suppressed by using the addition NO DATABASE SELECTION for the keyword SELECT-OPTIONS. </td>
        </tr>
<tr>
            <td>Simplified SELECT-OPTIONS on the Selection Screen</td>
            <td>Addition NO INTERVALS with SELECT-OPTIONS and SELECTION-SCREEN BEGIN OF BLOCK. These additions can be used o display and manipulate SELECT-OPTIONS on the selection screen. The function module SELECT_OPTIONS_RESTRICT restricts the number of valid selection options per SELECT-OPTIONS at runtime. The option 'E' (= 'Exclude from selection') can also be switched off. This makes it possible to simplify the entry of selections on the selection screen. </td>
        </tr>
<tr>
            <td>Field Selection</td>
            <td>The additions FIELDS f1 ... fn in the case of GET and GET LATE plus FIELD SELECTION FOR TABLE dbtab in the case of SELECTION-SCREEN. These make it possible to specify a list of fields required in the program for tables defined for this purpose in the logical database. Only these fields are then filled with values, which leads to considerable improvement in performance. </td>
        </tr>
<tr>
            <td>Passing Dynamic Selections with SUBMIT</td>
            <td>The addition FREE SELECTIONS ... can be used with SUBMIT to pass dynamic selections directly. </td>
        </tr>
<tr>
            <td>User-Specific Variables in Variants</td>
            <td>User-specific values can be set for certain parameters and selection criteria intended for that purpose by the application. </td>
        </tr>
<tr>
            <td>No More Blank Selection Screens</td>
            <td>Previously, selection screens were generated and processed even if no parameters or selection criteria were defined. To reduce the number of selection screens, this is no longer the case. Events previously triggered by the selection screen (AT SELECTION-SCREEN, AT SELECTION-SCREEN OUTPUT) are not processed. </td>
        </tr>
<tr>
            <td>Integration of the Data Model</td>
            <td>In transaction SE36 (Logical Databases), the set of views that refer to a logical database table can be displayed by selecting Edit -&gt; Data Model -&gt; Views and Entities. The relevant entities are displayed from the Enterprise Data Model ( EDM). After choosing the views, Data Model -&gt; Graphics can be selected to display the convex wrapper of the relevant entities in the EDM. </td>
        </tr>
<tr>
            <td rowspan="16">LIST</td>
            <td>WRITE and ULINE with Variable Position and Length Specifications</td>
            <td>In WRITE and ULINE, the addition AT is now available which allows dynamic position and/or length specifications. Example Usage of AT with WRITE. DATA: POS TYPE I    VALUE 5,       LEN TYPE I    VALUE 10,       F(20)         VALUE 'Test output'. WRITE AT /POS(LEN) F. ULINE AT /POS(LEN). </td>
        </tr>
<tr>
            <td>Variable Format Specifications with FORMAT, WRITE, MODIFY LINE</td>
            <td>In FORMAT, WRITE, and MODIFY LINE, it is now possible to use variables to give parameters to all format specifications (INPUT, INTENSIFIED, INVERSE, and COLOR). In any of these additions, a = is followed by the variable. Example Dynamic formatting. DATA: COL TYPE I,       INT TYPE I,       F(20)    VALUE 'Test output'. IF &lt;condition&gt;.   INT = 1.   COL = 5. ENDIF. WRITE F COLOR = COL INTENSIFIED = INT INPUT ON. FORMAT COLOR = COL INTENSIFIED = INT. </td>
        </tr>
<tr>
            <td>Saving Lists</td>
            <td>ABAP lists can now be saved as objects known as list objects. To save the basic list or the details list of the current application, the function module 'SAVE_LIST' is used. This function module passes the required list as a list object to an internal table of the structure ABAPLIST. Alternatively, the basic list of a report can be passed to the user memory instead of the display by SUBMIT ... EXPORTING LIST TO MEMORY, and then, after returning from the report, be retrieved as a list object by the function module 'LIST_FROM_MEMORY'. The list object can be saved like any other internal table (in a database or file system, for example). For further processing, use the function modules 'WRITE_LIST' (output a list object as a list), 'DISPLAY_LIST' (output a list object in a dialog box), and any existing or planned converters ('LIST_TO_ASCI', 'LIST_TO_RTF', and so on). In SAPoffice, any displayed list can be saved in the private folders (SAPoffice) of the currently logged on user by choosing System → List Save (SAPoffice). More functions are planned. </td>
        </tr>
<tr>
            <td>Exit List and Return to the Selection Screen</td>
            <td>When the list display of a report is exited, the selection screen of the report is displayed again. The entries made on the selection screen are preserved. As a prerequisite, the selection screen has not been suppressed by the request (SUBMIT...VIA SELECTION-SCREEN). The list can be exited by F3 / going back to the basic list of the report F15 / exiting any list of the report the programmed function LEAVE SCREEN in the application program Hint For reasons of compatibility, the function LEAVE does not offer this function. Instead, ABAP release 3.0 includes a special variant LEAVE LIST-PROCESSING. </td>
        </tr>
<tr>
            <td>SET CURSOR and GET CURSOR with Addition LINE... (without FIELD)</td>
            <td>The functions SET CURSOR LINE and GET CURSOR LINE enable the cursor to be positioned or the cursor position to be determined in lists at line level. This is useful in cases where the whole list line (and not the field within the line) is relevant. Previously, the syntax rules required the addition FIELD to be used. Hint In general, the function GET CURSOR LINE l OFFSET o should be used when the absolute cursor position (column) within a list line is evaluated during an interactive list event. The system field SY-CUCOL does not refer to the list line, but to the displayed list section. After horizontal scrolling, the relative cursor position (SY-CUCOL) and the absolute cursor position are different. Furthermore, the system field SY-CUCOL contains the value 2 if (for reasons of compatibility) the cursor is on the first visible list column, which makes the calculation of the absolute position difficult. Example The following example shows how the previous usage of SY-CUCOL can be replaced by the function GET CURSOR: DATA: CURSORPOSITION TYPE I,       CURSOROFFSET   TYPE I,       CURSORLINE     TYPE I. AT LINE-SELECTION.   CURSORPOSITION = SY-CUCOL - 2 + SY-STACO.               "old AT LINE-SELECTION.   GET CURSOR LINE CURSORLINE OFFSET CURSOROFFSET.         "new   CURSORPOSITION = CURSOROFFSET + 1.                      "new </td>
        </tr>
<tr>
            <td>Handling of EXIT in List Events</td>
            <td>The function EXIT now has the same effect in all list events (AT LINE-SELECTION, AT USER-COMMAND, AT PF..., TOP-OF-PAGE ..., and END-OF-PAGE) as in MODULE, FORM, and FUNCTION. The event therefore ends immediately and the processing continues at the call location (for example, after EXIT from TOP-OF-PAGE, the triggering statement WRITE is executed; after EXIT from AT LINE-SELECTION, the list is displayed immediately). Previously, the list was displayed here in both cases (the triggering WRITE was ignored) and, if there was also indented list processing (LEAVE TO LIST-PROCESSING from CALL SCREEN), the entire list processing was terminated. Hint This change has the following consequence: In report processing (with a logical database), EXIT from TOP-OF-PAGE and END-OF-PAGE previously terminated processing of the logical database and branched directly to the list display. Now, only the event TOP-OF-PAGE or END-OF-PAGE is terminated. Any remaining logical database events (START-OF-SELECTION, GET, and so on) are not affected by this change. </td>
        </tr>
<tr>
            <td>SET PF-STATUS SPACE Activates Standard List Status</td>
            <td>If a standard list status (STLI, PICK, or INLI) is required in list processing, but a dialog with a separate user interface is needed first, this is made possible by using SET PF-STATUS SPACE (when creating the list). Previously, this was only possible by copying the standard list status to a separate PF status. </td>
        </tr>
<tr>
            <td>Fixed List Columns when Displaying a List (Leading Column)</td>
            <td>SET SCROLL-BOUNDARY enables the area of a list page affected by horizontal scrolling to be restricted. NEW-LINE NO-SCROLLING can be used to flag individual list lines as unmovable. </td>
        </tr>
<tr>
            <td>New Behavior of SKIP at End of Page</td>
            <td>Previously, if SKIP occurred at the end of a page and there was a fixed number of lines per page (as defined by NEW-PAGE LINE-COUNT), a new page was started and the blank line was displayed as the first line of that new page (after TOP-OF-PAGE). This produced unwanted line shifts on a new page, although the only purpose of SKIP was to separate different areas of the list. Therefore, SKIP is no longer executed on the next page unless explicitly requested by NEW-PAGE. </td>
        </tr>
<tr>
            <td>New Additions with WRITE [TO] for Alignment</td>
            <td>WRITE now has the additions LEFT-JUSTIFIED, CENTERED, and RIGHT-JUSTIFIED for left-aligned, centered, or right-aligned output. They can be used in output to lists and for string processing with WRITE ... TO. In the first case, the alignment refers to the output field in the list. In the latter case, it refers to the target field specified after TO. </td>
        </tr>
<tr>
            <td>Field Help (F1) and Input Help (F4) Now Also in LIKE Fields</td>
            <td>Previously, field help (F1) and input help (F4) were only available for fields which had a direct reference to a data element (usually table fields). Now this is supported for work fields that refer to a table field with DATA ... LIKE as well. This of course also applies to field symbols and parameters of subroutines. </td>
        </tr>
<tr>
            <td>GET CURSOR on Field Symbols and Literals</td>
            <td>The function GET CURSOR FIELD now always returns the name of a global symbol, in other words, a field that is still valid when the list is displayed. Names of field symbols and local variables are invalid in this context. For field symbols and reference parameters of subroutines, the function returns the name of the global symbol that may have been assigned when the list was displayed. For literals, the return code is no longer set to 4 and the field name becomes SPACE and the return code 0. The content of the literal is returned as ... VALUE. </td>
        </tr>
<tr>
            <td>System Tables %_LIST ... Protected Against Access</td>
            <td>Previously it was possible to directly address the system tables of list processing (%_LIST, ) in all ABAP programs and in debugging. This is no longer possible. The most common reason for wanting to do this (determining the number of lines in a list with DESCRIBE TABLE %_LIST LINES lin) can now be achieved by using DESCRIBE LIST NUMBER OF LINES lin INDEX SY-LSIND. </td>
        </tr>
<tr>
            <td>Symbols in Lists</td>
            <td>WRITE with the addition '...AS SYMBOL' is now used to display certain characters in a list as symbols. Example Output of a symbol. INCLUDE &lt;SYMBOL&gt;. WRITE: / SYM_PHONE AS SYMBOL.        " Output: telephone symbol </td>
        </tr>
<tr>
            <td>Hotspots in Lists</td>
            <td>By using the addition '...HOTSPOT' with the statements FORMAT and WRITE, it is now possible to define particular areas in a list as hotspots. Clicking once with the mouse in one of these areas triggers the same response as placing the cursor on the clicked position in the list and then pressing the function key F2 (in other words a double-click is achieved with a single click). Example Creating a hotspot. DATA F. FORMAT HOTSPOT. * or WRITE: / F HOTSPOT. </td>
        </tr>
<tr>
            <td>Output of QUAN Fields (WRITE Addition UNIT)</td>
            <td>WRITE now has the addition UNIT which enables quantity fields to be formatted by unit. Quantity fields are packed fields and usually have the type QUAN in ABAP Dictionary. Apart from their defined number of decimal places, they can be formatted by the unit specified in UNIT, for example, for item specifications without decimal places. </td>
        </tr>
<tr>
            <td rowspan="2">MESSAGE</td>
            <td>MESSAGE with Numbered Variables</td>
            <td>The variables &amp;1 ... &amp;4 are now also replaced in a MESSAGE. </td>
        </tr>
<tr>
            <td>MESSAGE Xnnn</td>
            <td>The current application can trigger a program termination with the short dump MESSAGE_TYPE_X if it recognizes a situation that should not have occurred. </td>
        </tr>
<tr>
            <td rowspan="19">MISC</td>
            <td>SET TITLEBAR with Numbered Variables</td>
            <td>SET TITLEBAR now also replaces the variables &amp;1 ... &amp;9. </td>
        </tr>
<tr>
            <td>New Additions for LEAVE: LEAVE PROGRAM, LEAVE LIST-PROCESSING</td>
            <td>The new additions LEAVE PROGRAM and LEAVE LIST-PROCESSING for LEAVE now make it possible to control the flow precisely when leaving processing. </td>
        </tr>
<tr>
            <td>New Language Element CONTINUE for Continuing a Loop</td>
            <td>CONTINUE terminates the current pass through a DO, WHILE, LOOP, or SELECT loop. However, the loop is not exited as in EXIT, but continues with the next loop element, as is the case when using CHECK. </td>
        </tr>
<tr>
            <td>Editing ABAP Text Elements</td>
            <td>The term numbered texts has been changed to text symbols. Until now a text symbol had an implicit length, calculated by comparing the text from the right until the first character not a blank. Due to translation difficulties (a text may sometimes need more space in a foreign language than in the original language), it became necessary to define the length explicitly. Here, the same process was chosen as already used for Screen Painter transactions. The underscore (_) allows you to vary the length for each entry. The underscore itself, however, is replaced by a blank before saving. As a result, underscores cannot be used in text symbols. In change mode, underscores (and therefore the internal length) are visible. In display mode, the text symbols are displayed as saved (without underscores). The Text Comparison function displays the text symbols in their defined lengths. Trailing blanks are shown as underscores. For all screens, the function Print has been added to the Text Elements menu. This function can be used to print a list of the required text elements, dependent on context. </td>
        </tr>
<tr>
            <td>ASSIGN COMPONENT with Component Name</td>
            <td>ASSIGN COMPONENT now handles the next field as a component name and not as a component number, if the field is of type C or has a structure that contains no internal table. </td>
        </tr>
<tr>
            <td>User Interface for Entering Dynamic Selections</td>
            <td>You can use the function modules FREE_SELECTIONS_INIT and FREE_SELECTIONS_DIALOG to create a dialog for entering selections for any database fields. The selections entered by the user are returned in several different forms, for example, as tables with WHERE clauses that can be passed directly to a SELECT statement. </td>
        </tr>
<tr>
            <td>New Addition OR fld for the WHEN Statement</td>
            <td>The addition OR fld can now be used to define any number of comparison values for the WHEN statement. </td>
        </tr>
<tr>
            <td>Exponentiation</td>
            <td>The COMPUTE statement now supports the operator **. Previously, the exponential operation X ** Y was awkward to express. Either repeated multiplication had to be used (if Y was an integer) or the expression EXP( Y * LOG( X ) ) for any exponent Y. </td>
        </tr>
<tr>
            <td>New Arithmetic Functions</td>
            <td>Most common arithmetic functions are now supported. The functions ABS, SIGN, CEIL, FLOOR, TRUNC, and FRAC are suitable for all numeric types (I, P, and F). The functions SIN and COS, intended mainly for floating point operands, were already available, as well as EXP, LOG, and SQRT. The following have been added: The trigonometry functions TAN, ACOS, ASIN, and ATAN The hyperbola functions COSH, SINH, and TANH The logarithm function LOG10 </td>
        </tr>
<tr>
            <td>Defining Types</td>
            <td>The TYPES statement has been introduced, which makes it possible to define new types. It has a similar syntax to the DATA statement. </td>
        </tr>
<tr>
            <td>Type Pools</td>
            <td>The type pools (see TYPE-POOL) make it possible to form groups of global types and their associated constants. </td>
        </tr>
<tr>
            <td>Defining Constants</td>
            <td>The CONSTANTS statement makes it possible to define constants. The syntax of the CONSTANTS statement is similar to that of the DATA statement. Both simple fields and structures can be defined as constants. Constant tables cannot be defined. </td>
        </tr>
<tr>
            <td>Defining Static Variables</td>
            <td>Fields within subroutines and function modules that are defined with DATA are re-created and re-initialized each time the routine is called. The new statement STATICS, which can be used within subroutines and function modules, makes it possible to define fields that keep their values beyond the calls. This statement supports nearly all of the additions used with the DATA statement. </td>
        </tr>
<tr>
            <td>Multiple Nesting of Structures</td>
            <td>Previously, it was only possible to define simple nested structures using the DATA statement. It was not possible to nest DATA BEGIN OF, .... END OF. This restriction has now been lifted, which makes it possible to define structures as components of another structure. This is now possible in the DATA, STATICS, CONSTANTS, and TYPES statements. </td>
        </tr>
<tr>
            <td>Internal Tables without Header Line</td>
            <td>Previously, each internal table automatically had a header line, which was used as a table work area. A table with header line is therefore a mixture of two data objects (the table and the header line) under a single name. It is now possible to define tables and table types without header lines in the statements DATA, STATICS, and TYPES, using the following syntax: DATA TAB TYPE STRUC OCCURS 0. These tables can be used without header lines as components in structures. If a work area is needed for a table, it must be under another name. If the statements DATA and STATICS are used, the addition ... WITH HEADER LINE can be used to create a table with a header line from a table without a header line. This can only be done at the top nesting level, not within a structure. The property of having a header line is property of a data object and not a type property, which means that this addition cannot be used in the TYPES statement. </td>
        </tr>
<tr>
            <td>Specifying the Types of Parameters and Field Symbols</td>
            <td>The parameters of subroutines and field symbols can now be typed using the additions LIKE fld and TYPE typ. The parameters of function modules can now be typed using references to predefined ABAP types and global types (types from type pools) as well as referencing ABAP Dictionary fields. </td>
        </tr>
<tr>
            <td>SORT ... AS TEXT: Locale-Friendly Sorting</td>
            <td>The addition ... AS TEXT of the language element SORT enables text data to be sorted in a locale-friendly way. </td>
        </tr>
<tr>
            <td>CONVERT TEXT: Conversion of Texts to a Locale-Specific Sortable Format</td>
            <td>The new CONVERT variant CONVERT TEXT t INTO SORTABLE CODE sc converts a text to a locale-friendly sortable format. </td>
        </tr>
<tr>
            <td>SET LOCALE: Explicit Setting of the Text Environment</td>
            <td>The new SET variant SET LOCALE enables program-driven configuration of the text environment. </td>
        </tr>
<tr>
            <td rowspan="3">RFC</td>
            <td>Transactional RFC</td>
            <td>CALL FUNCTION... DESTINATION ... can now also be called using the addition IN BACKGROUND TASK. The calls are collected and executed as a logical unit of work (LUW) on the target host (destination) at the next COMMIT WORK. The current program does not wait for the function modules to finish processing. </td>
        </tr>
<tr>
            <td>Asynchronous RFC</td>
            <td>Regular remote function calls (calls using CALL FUNCTION .... DESTINATION ...) only enable an ABAP program to communicate with ABAP programs running in the background. This means that a program called in this way cannot interact with the user. The calling program can be a dialog program and display a screen to the user, however this screen is always inactive. The following call starts an ABAP function module in a new session for parallel processing: CALL FUNCTION ... STARTING NEW TASK ... The addition DESTINATION IN GROUP ... allows automatic load balancing within a group of application servers. The addition DESTINATION dest enables parallel processing in a remote system. Unlike in a regular function module call, the caller carries on processing immediately as soon as the function module (either local or remote) has been started (asynchronous call). The called function module can now, for example, use CALL SCREEN ... to display a dynpro and interact with the user. The statement WAIT UNTIL condition [UP TO n SECONDS] makes it possible to wait for the confirmation of an asynchronously called function module (this also requires the addition PERFORMING form ON END OF TASK. WAIT must be executed in the same internal session. </td>
        </tr>
<tr>
            <td>Authorization Checks when Accessing Function Pools Using RFC</td>
            <td>If the profile parameter auth/rfc_authority_check is set, the system checks authorization against authorization object S_RFC for the function pool before calling the function. </td>
        </tr>
<tr>
            <td rowspan="6">STRINGS</td>
            <td>Assignments with Variable Offsets and Lengths Specified</td>
            <td>destination = source can be used with respect to the source and target field when variable offsets and lengths are specified. </td>
        </tr>
<tr>
            <td>ASSIGN and PERFORM with Offsets Specified and Field Limits Not Exceeded</td>
            <td>The language elements ASSIGN and PERFORM can now be used to specify offsets without exceeding field limits (using field+off(*)). </td>
        </tr>
<tr>
            <td>New SHIFT Variants</td>
            <td>The new variants of SHIFT allow field content to be shifted so that a prefix or suffix consisting of a certain number of characters is omitted. In the character string gaps, for example, it is possible to specify which characters constitute the prefix or suffix (using SHIFT field LEFT DELETING LEADING gaps or SHIFT field RIGHT DELETING TRAILING gaps). </td>
        </tr>
<tr>
            <td>New Language Element CONCATENATE</td>
            <td>CONCATENATE can be used to append character strings to a target field (using CONCATENATE f1 ... fn INTO g). The addition SEPARATED BY h makes it possible to insert the separator h between the character strings fi. </td>
        </tr>
<tr>
            <td>New Language Element SPLIT</td>
            <td>SPLIT can be used to split a string in accordance with a sequence of separators (SPLIT f AT g) and place the resulting substrings in the specified fields (... INTO h1 ... hn) or in an internal table (... INTO TABLE itab). </td>
        </tr>
<tr>
            <td>New Additions for WRITE [TO] for Alignment Purposes</td>
            <td>WRITE now has the additions LEFT-JUSTIFIED, CENTERED, and RIGHT-JUSTIFIED for left-aligned, centered, or right-aligned output. These can be used both when displaying data in lists and for string processing using WRITE ... TO. In the first case, the alignment refers to the output field in the list. In the latter case, it refers to the target field specified after TO. </td>
        </tr>
  </table>
</details>  

<p align="right"><a href="#top">⬆️ back to top</a></p>