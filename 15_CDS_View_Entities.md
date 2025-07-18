# CDS View Entities

- [CDS View Entities](#cds-view-entities)
  - [A Glimpse on the CDS Syntax](#a-glimpse-on-the-cds-syntax)
  - [More Information](#more-information)
  - [Executable Example](#executable-example)

Core data services (CDS) are an infrastructure for defining and consuming semantically rich data models on the [standard database](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenstandard_db_glosry.htm) of an [AS ABAP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenas_abap_glosry.htm).

> [!NOTE]
> - The executable example focuses on CDS view entities and covers a selection of features.
> - The sample CDS view entities are designed to demonstrate a selection of features with a limited number of artifacts. They are not intended to be role models for proper CDS view design. They focus on syntax options only. They are not intended to solve concrete programming tasks. You should always work out your own solution for each individual case. For more detailed information, refer to the links in the [More Information](#more-information) section.
> - The [ABAP Dictionary](26_ABAP_Dictionary.md) cheat sheet highlights that several [CDS entities](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_entity_glosry.htm) - apart from CDS view entities - represent structured types that are usable in ABAP.

## A Glimpse on the CDS Syntax
The following links take you to the source code of the cheat sheet artifacts to get a glimpse on the syntax used. To explore the syntax in action, import the ABAP cheat sheet repository into your system. 

- [zdemo_abap_cds_ve_sel](src/zdemo_abap_cds_ve_sel.ddls.asddls)
  - Input parameters
  - Specifying fields of the data source
  - Typed and untyped literals
  - Session variables
  - Multiple expressions: 
    - Cast expressions 
    - Reuse expressions
    - Arithmetic expressions
    - Case expressions
    - Logical expressions
  - Built-in functions
    - Numeric functions
    - String functions
    - Coalesce function
    - Date and time functions
- [zdemo_abap_cds_ve_agg_exp](src/zdemo_abap_cds_ve_agg_exp.ddls.asddls)
  - Aggregate expressions
- [zdemo_abap_cds_ve_joins](src/zdemo_abap_cds_ve_joins.ddls.asddls)
  - Inner joins
  - Left outer joins
  - Right outer joins
  - Cross joins
- [zdemo_abap_cds_ve_assoc](src/zdemo_abap_cds_ve_assoc.ddls.asddls)
  - Associations

## More Information

- [ABAP Data Models Guide](https://help.sap.com/docs/abap-cloud/abap-data-models/abap-data-models)
- [ABAP Core Data Services in the ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds.htm)
- [ABAP CDS Development Tools: User Guide](https://help.sap.com/docs/abap-cloud/abap-cds-tools-user-guide/about-abap-cds-development-tools-user-guide?version=sap_btp)
- [ABAP CDS Feature Tables](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_language_elements.htm)
- [ABAP CDS Glossary](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_glossary.htm)
- [ABAP CDS - SAP Annotation Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_annotations_ktd_docu.htm)
- Blogs:
  - [Feature Matrix: Data Modeling with ABAP Core Data Services](https://blogs.sap.com/2022/10/24/feature-matrix-data-modeling-with-abap-core-data-services/)
  - [ABAP CDS Cheat Sheet: Amounts and Quantities in ABAP CDS](https://blogs.sap.com/2022/07/07/abap-cds-cheat-sheet-amounts-and-quantities-in-abap-cds/)


## Executable Example

[zcl_demo_abap_cds_ve](./src/zcl_demo_abap_cds_ve.clas.abap)

> [!NOTE]
> - The executable example covers the following topics:
>   - Operands, expressions, built-in functions, and input parameters in CDS view entities
>   - Selecting data from CDS view entities using ABAP SQL `SELECT` statements
>   - Joins  
>     - Note: A sample CDS view entity contains multiple joins. You can comment in/out code sections to see the effect. See the notes in the view. 
>     - Excursion: Joins in ABAP SQL
>   - Associations
>     - Defining views with associations
>     - Exposing associations
>     - Using exposed associations in ABAP statements
> - The example CDS view entities (`zdemo_abap_cds_ve...`) and the [class](./src/zcl_demo_abap_cds_ve.clas.abap) contains comments in the code for more information.
> - The steps to import and run the code are outlined [here](README.md#-getting-started-with-the-examples).
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)