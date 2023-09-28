<a name="top"></a>

# ABAP SQL: Working with Hierarchies

- [ABAP SQL: Working with Hierarchies](#abap-sql-working-with-hierarchies)
  - [Introduction](#introduction)
  - [Overview](#overview)
  - [SQL Hierarchies](#sql-hierarchies)
  - [Creating SQL Hierarchies](#creating-sql-hierarchies)
    - [ABAP CDS Hierarchies](#abap-cds-hierarchies)
    - [ABAP SQL Hierarchy Generator HIERARCHY](#abap-sql-hierarchy-generator-hierarchy)
    - [ABAP CTE Hierarchies](#abap-cte-hierarchies)
  - [Hierarchy Navigators](#hierarchy-navigators)
    - [Hierarchy Node Navigator HIERARCHY\_DESCENDANTS](#hierarchy-node-navigator-hierarchy_descendants)
    - [Hierarchy Node Navigator HIERARCHY\_ANCESTORS](#hierarchy-node-navigator-hierarchy_ancestors)
    - [Hierarchy Node Navigator HIERARCHY\_SIBLINGS](#hierarchy-node-navigator-hierarchy_siblings)
    - [Hierarchy Aggregate Navigators](#hierarchy-aggregate-navigators)
  - [More Information](#more-information)

## Introduction

This cheat sheet summarizes the functions ABAP SQL offers together with
ABAP CDS for working with [hierarchical
data](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_glosry.htm "Glossary Entry")
that is stored in database tables. Hierarchical data in database tables
means that lines of one or more database tables are connected by
[parent-child
relationships](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenpcr_glosry.htm "Glossary Entry").
There are many use cases where hierarchical data plays a role and where
accessing information about the hierarchical relationship is important.
For example, a common task can be to find out the descendants or
ancestors of a given hierarchy node or to aggregate values of subtrees.

## Overview

In former times you had to load the data from the database into internal
tables and program it all by yourself (if you did not find an
appropriate API). In between,
[meshes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmesh_glosry.htm "Glossary Entry")
offered some features for working with hierarchies, as shown in this
[example](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenmesh_for_reflex_sngl_abexa.htm),
but have not found wide distribution.

Meanwhile, the standard AS ABAP database is a SAP HANA database that
offers a lot of helpful features. Among other things, you will find a
set of hierarchy functions there that allow you to deal with
hierarchical data directly on the database and that you can look up in
the [SAP HANA
documentation](https://help.sap.com/docs/SAP_HANA_PLATFORM/4fe29514fd584807ac9f2a04f6754767/2969da89b87f4abd85fd0b5f9f5bc395.html?version=2.0.06&locale=en-US).
Now you might expect that you must use
[AMDP](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenamdp.htm)
in order to access these functions from your ABAP programs, but no need
to do so! ABAP SQL and ABAP CDS support hierarchies directly by wrapping
the HANA built-in functions without any loss of performance. You can
stay in the comfortable ABAP world and nevertheless have access to most
modern features. All you have to do, is to understand some concepts and
learn some additional syntax and then you can start right away.

> **💡 Note**<br>
> The examples in this cheat sheet are only relevant for [standard ABAP](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstandard_abap_glosry.htm), i. e. the unrestricted ABAP language scope. Find the artifacts used in the code snippets in your on-premise ABAP system.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## SQL Hierarchies

With [SQL
hierarchy](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abensql_hierarchy_glosry.htm "Glossary Entry")
we denote a special [hierarchical data
source](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_data.htm)
that you can use in the `FROM` clause of ABAP SQL queries. A SQL
hierarchy is a tabular set of rows which form the hierarchy nodes of a
hierarchy and which contains additionally [hierarchy
columns](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_column_glosry.htm "Glossary Entry")
that contain hierarchy attributes with hierarchy-specific information
for each row. For creating a SQL hierarchy, you need the following:

-   Data Source

    This can be any data source you can access normally in an ABAP SQL
    query, as most commonly a database table or a CDS view, but also a
    CTE (common table expression). The structure and content of the data
    source should be able to represent hierarchical data.

-   Parent-Child Relation

    A parent-child relation must be defined between two or more columns
    of the data source. From the parent-child relation and the actual
    data of the data source, the SQL hierarchy consisting of parent
    nodes and child nodes can be created. The parent-child relation must
    be defined by a self-association which we call hierarchy
    association. This can be achieved with CDS associations or CTE
    associations. A data source exposing a hierarchy association can be
    used as a hierarchy source for creating a SQL hierarchy.

-   Hierarchy Creation

    From a hierarchy source, that is a data source exposing a hierarchy
    association, a SQL hierarchy can be created. This can be done either
    by defining a CDS hierarchy outside an ABAP program or with the
    hierarchy generator of ABAP SQL directly in the `FROM`
    clause of an ABAP SQL query inside an ABAP program.

The following topics show you step-by-step how SQL hierarchies can be
created and accessed.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Creating SQL Hierarchies

### ABAP CDS Hierarchies
With [CDS
hierarchies](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_column_glosry.htm "Glossary Entry"),
you outsource the hierarchy data source and the creation of the SQL
hierarchy from your ABAP program to ABAP CDS. Here the hierarchy is a
fully fledged CDS entity, it is reusable in different programs or in
other CDS entities (views), and can be part of your data model including
access control using CDS DCL. For a CDS hierarchy, the hierarchy source
cannot be anything else but a CDS view that exposes a [hierarchy
association](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_association_glosry.htm "Glossary Entry").
Here is a very simple example for that:

``` 
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity DEMO_CDS_SIMPLE_TREE_VIEW
  as select from demo_simple_tree
  association [1..1] to DEMO_CDS_SIMPLE_TREE_VIEW as _tree  
    on $projection.parent = _tree.id
{
      _tree,
      key id,
      parent_id as parent,
      name
}
```

This CDS view entity accesses the database table
`DEMO_SIMPLE_TREE`, where the actual data is
stored, and exposes a
[self-association](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenself_association_glosry.htm "Glossary Entry")
`_tree`. The `ON` condition of the association
defines a parent-child relation between the elements `id` and
`parent`. It simply means that a row of the result set where
column `parent` has the same value as the column
`id` of another row is a child of the latter row in the
hierarchy that is constructed from that view. The CDS view exposes also
another column `name` of the database table that represents
the remaining data content. Note that you can define such CDS views for
any available data source and that the `ON` condition can be
more complex than shown in the simple example here.

Now we can use the above CDS view as the hierarchy source of a CDS
hierarchy that can be defined as follows:

``` 
define hierarchy DEMO_CDS_SIMPLE_TREE
  with parameters
    p_id : abap.int4
  as parent child hierarchy(
    source
      DEMO_CDS_SIMPLE_TREE_SOURCE
      child to parent association _tree
      start where
        id = :p_id
      siblings order by
        id ascending
    )
    {
      id,
      parent,
      name
    }
```

The CDS DDL statement [`DEFINE
HIERARCHY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_f1_define_hierarchy.htm)
that can be used in the DDL source code editor of ADT defines a CDS
hierarchy as a CDS entity that can be accessed in CDS views or ABAP SQL
as a SQL hierarchy. The most important additions of the statement are:

-   `SOURCE` for specifying the hierarchy source, here our
    `DEMO_CDS_SIMPLE_TREE_VIEW`.
-   `CHILD TO PARENT ASSOCIATION` for specifying the
    hierarchy association, here `_tree`.
-   `START WHERE` for defining the root nodes of the SQL
    hierarchy, here represented by an input parameter `p_id`
    that must be passed when accessing the CDS hierarchy.
-   `SIBLINGS ORDER BY` to define also a sort order for
    sibling nodes besides the sort order that comes from the
    parent-child relationship anyhow.
-   An element list `{ ... }` that defines the columns of
    the SQL hierarchy, here simply all elements of the hierarchy source.

For a full description and all other additions see [`DEFINE
HIERARCHY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_f1_define_hierarchy.htm).

When you access the CDS hierarchy, all lines are selected from the
original data source, in our case the database table
`DEMO_SIMPLE_TREE`, that fulfill the `START WHERE` condition. Those make up the root node set of the SQL
hierarchy. In the simplest case we have exactly one root node, but more
are possible. Then, for each root node, its descendants are retrieved.
That means each line from the database table that fulfills the
`ON`-condition of the hierarchy association is added to the
SQL hierarchy. And for each descendant this is done again and again
until all descendants are found. And that is basically all! Further
additions to `DEFINE HIERARCHY` allow you to control the
creation of the SQL hierarchy, for example, whether multiple parents are
allowed or how orphans or cycles should be handled.

Besides the elements of the hierarchy, the element list can also contain
the hierarchy attributes listed under [Hierarchy
Attributes](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencds_hierarchy_attributes.htm).
Then the SQL hierarchy is enriched with columns containing information
about the role, the current line plays as a hierarchy node, as, for
example, the hierarchy rank or the hierarchy level. In our example, we
did not add such elements, because ABAP SQL does that implicitly for you
when accessing the CDS hierarchy!

The SQL hierarchy can be used in an ABAP SQL query by using the CDS
hierarchy directly as a data source of the `FROM` clause:

``` abap
DATA root_id type demo_cds_simple_tree_view-id.

...

SELECT FROM demo_cds_simple_tree( p_id = @root_id )
  FIELDS id,
         parent,
         name,
         hierarchy_rank,
         hierarchy_tree_size,
         hierarchy_parent_rank,
         hierarchy_level,
         hierarchy_is_cycle,
         hierarchy_is_orphan,
         node_id,
         parent_id
  INTO TABLE @FINAL(cds_result).
```

And although we did not define any hierarchy attributes in the element
list of the CDS hierarchy, we can add all the hierarchy columns listed
under [Hierarchy
Columns](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenddddl_hierarchy.htm)
to the `SELECT` list of our ABAP SQL statement! This is always
possible when a SQL hierarchy is accessed in ABAP SQL. We can pass any
ID to the CDS hierarchy now and see what happens. If such a line is
found in the database table, the respective hierarchical data will be
retrieved and delivered. Execute class
`CL_DEMO_SQL_HIERARCHIES` for filling the
database table with randomly generated data and inspect the tabular
result. As expected, all elements of the `SELECT` list appear as
columns. Note that the content of column `NAME` could be
anything. It is filled here with a string representation of the path
from the root node to the current node for demonstration purposes only.

From the ABAP language point of view, CDS hierarchies are the most
convenient way of using SQL hierarchies. Now let us turn to other ways,
involving more ABAP, until we do not use any CDS more in the end.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### ABAP SQL Hierarchy Generator HIERARCHY
The ABAP SQL [hierarchy
generator](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_generator_glosry.htm "Glossary Entry")
is a ABAP SQL function named
[`HIERARCHY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_generator.htm),
that allows you to define a SQL hierarchy in the ABAP program itself.
Let us look directly at an example:

``` abap
DATA root_id TYPE demo_cds_simple_tree_view-id.

...

SELECT FROM HIERARCHY( SOURCE demo_cds_simple_tree_view
                       CHILD TO PARENT ASSOCIATION _tree
                       START WHERE id = @root_id
                       SIBLINGS ORDER BY id
                       MULTIPLE PARENTS NOT ALLOWED ) "hierarchy
       FIELDS id,
              parent,
              name,
              hierarchy_rank,
              hierarchy_tree_size,
              hierarchy_parent_rank,
              hierarchy_level,
              hierarchy_is_cycle,
              hierarchy_is_orphan,
              node_id,
              parent_id
       INTO TABLE @FINAL(asql_cds_result).

ASSERT asql_cds_result = cds_result.
```

Looks familiar? Well, almost the same syntax used for defining the CDS
hierarchy is used in the brackets `HIERARCHY( ... )` and it
does exactly the same! The difference is the same as it is between ABAP
SQL joins and joins in CDS views:

-   If you code it in ABAP SQL, it is for usage in one program only.
-   If you code it in ABAP CDS, it is for usage in many programs or
    whole data models.

And, as you can see, we dare to prove this with an `ASSERT`
statement. Also note that we use the hierarchy columns again. They exist
implicitly when an SQL hierarchy, here created by the hierarchy
generator, is accessed.

The above hierarchy generator of ABAP SQL accesses the same hierarchy
source as the CDS hierarchy, namely the CDS view
`DEMO_CDS_SIMPLE_TREE_VIEW` that exposes the necessary
hierarchy association `_tree`. In the following code
snippet, we replace the CDS hierarchy source with a CTE:

``` abap
DATA root_id type demo_cds_simple_tree_view-id.

...

WITH
  +cte_simple_tree_source AS
     ( SELECT FROM demo_simple_tree
              FIELDS id,
                     parent_id AS parent,
                     name )
        WITH ASSOCIATIONS (
          JOIN TO MANY +cte_simple_tree_source AS _tree
            ON +cte_simple_tree_source~parent = _tree~id )
  SELECT FROM HIERARCHY( SOURCE +cte_simple_tree_source
                         CHILD TO PARENT ASSOCIATION _tree
                         START WHERE id = @root_id
                         SIBLINGS ORDER BY id
                         MULTIPLE PARENTS NOT ALLOWED ) "hierarchy
         FIELDS id,
                parent,
                name,
                hierarchy_rank,
                hierarchy_tree_size,
                hierarchy_parent_rank,
                hierarchy_level,
                hierarchy_is_cycle,
                hierarchy_is_orphan,
                node_id,
                parent_id
         INTO TABLE @FINAL(asql_cte_result). 

ASSERT asql_cte_result = cds_result.
```

Common table expressions (CTEs) are a very powerful tool for defining
subqueries that can be used in subsequent queries of the same
[`WITH`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwith.htm)
statement. They can be regarded as an internal ABAP SQL definition of
data sources that fulfill the same functionality as program external
data sources, especially CDS views. As you see above, the CTE
`cte_simple_tree_source` does the same as the CDS view
`DEMO_CDS_SIMPLE_TREE_VIEW`:

-   It accesses the database table `DEMO_SIMPLE_TREE`.
-   It exposes an association `_tree` by using the addition
    [`WITH ASSOCIATIONS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwith_associations.htm).

The main query of the `WITH` statement uses the hierarchy
generator in the same way as the `SELECT` above, just with the
CTE as a data source instead of the CDS view and the result is - of
course - the same.

For a full description of the hierarchy generator and all other
additions see [`SELECT, FROM HIERARCHY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_generator.htm).

We managed to create a SQL hierarchy with ABAP SQL means only. Last but
not least we will use CTEs as hierarchies themselves. You might skip the
following section and turn directly to the hierarchy navigators if you
are not too interested in this syntactic gimmicks.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### ABAP CTE Hierarchies

A CTE that produces hierarchical data can declare itself as a SQL
hierarchy of a freely defined name with the addition [`WITH HIERARCHY`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abapwith_hierarchy.htm).
That simply means that subsequent queries of the same `WITH`
statement can use the CTE as a hierarchy with its implicit hierarchy
columns or - more important - in hierarchy navigators.

The following code snippets show the three ways in which a CTE can
produce hierarchical data:

``` abap
DATA root_id TYPE demo_cds_simple_tree_view-id.

...

WITH
      +tree AS
        ( SELECT FROM demo_cds_simple_tree( p_id = @root_id )
                 FIELDS * )
          WITH HIERARCHY demo_cds_simple_tree
      SELECT FROM  +tree "hierarchy 
             FIELDS id,
                    parent,
                    name,
                    hierarchy_rank,
                    hierarchy_tree_size,
                    hierarchy_parent_rank,
                    hierarchy_level,
                    hierarchy_is_cycle,
                    hierarchy_is_orphan,
                    node_id,
                    parent_id
             INTO TABLE @FINAL(cte_cds_result).

...

WITH
      +tree AS
        ( SELECT FROM HIERARCHY(
            SOURCE demo_cds_simple_tree_view
            CHILD TO PARENT ASSOCIATION _tree
            START WHERE id = @root_id
            SIBLINGS ORDER BY id
            MULTIPLE PARENTS NOT ALLOWED ) AS asql_hierarchy
            FIELDS id,
                   parent,
                   name )
          WITH HIERARCHY asql_hierarchy
       SELECT FROM +tree "hierarchy 
             FIELDS id,
                    parent,
                    name,
                    hierarchy_rank,
                    hierarchy_tree_size,
                    hierarchy_parent_rank,
                    hierarchy_level,
                    hierarchy_is_cycle,
                    hierarchy_is_orphan,
                    node_id,
                    parent_id
             INTO TABLE @FINAL(cte_asql_result).

...

WITH
      +cte_simple_tree_source AS
        ( SELECT FROM demo_simple_tree
                 FIELDS id,
                        parent_id AS parent,
                        name )
           WITH ASSOCIATIONS (
             JOIN TO MANY +cte_simple_tree_source AS _tree
               ON +cte_simple_tree_source~parent = _tree~id ),
      +tree AS
        ( SELECT FROM HIERARCHY(
            SOURCE +cte_simple_tree_source
            CHILD TO PARENT ASSOCIATION _tree
            START WHERE id = @root_id
            SIBLINGS ORDER BY id
            MULTIPLE PARENTS NOT ALLOWED ) AS cte_hierarchy
            FIELDS id,
                   parent,
                   name  )
            WITH HIERARCHY cte_hierarchy
      SELECT FROM +tree "hierarchy 
             FIELDS id,
                    parent,
                    name,
                    hierarchy_rank,
                    hierarchy_tree_size,
                    hierarchy_parent_rank,
                    hierarchy_level,
                    hierarchy_is_cycle,
                    hierarchy_is_orphan,
                    node_id,
                    parent_id
             INTO TABLE @FINAL(cte_cte_result).

ASSERT cte_cds_result  = cds_result.
ASSERT cte_asql_result = cds_result.
ASSERT cte_cte_result  = cds_result.
```

A CTE that is exposed as a SQL hierarchy must access a SQL hierarchy
itself and in the end these are always based on a CDS hierarchy or the
ABAP SQL hierarchy generator as shown above. Again, the hierarchy source
of the hierarchy generator can be a CDS view or a CTE exposing the
hierarchy association. Running
`CL_DEMO_SQL_HIERARCHIES` shows that all
assertions are fulfilled.

<p align="right"><a href="#top">⬆️ back to top</a></p>

## Hierarchy Navigators

[Hierarchy
navigators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_navigator_glosry.htm "Glossary Entry")
are an additional set of [hierarchy
functions](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_function_glosry.htm "Glossary Entry")
in ABAP SQL that allow you to work on existing SQL hierarchies instead
of creating them. Hierarchy navigators can work on SQL hierarchies
created as shown above, namely on CDS hierarchies, the hierarchy
generator or a CTE hierarchy. They can be used as data sources in ABAP
SQL queries. If you need a SQL hierarchy multiple times, from a
performance point of view it is best to create it once with a given set
of root nodes and then access it with hierarchy navigators. Furthermore,
each hierarchy navigator can add further hierarchy columns to the result
set that offer additional options for the evaluation.

In the following examples, we access our CDS hierarchy with hierarchy
navigators. But you could also replace it with the hierarchy generator
or a CTE hierarchy. Check the examples of the
[documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_navigators.htm),
where this is also shown.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Hierarchy Node Navigator HIERARCHY_DESCENDANTS

As the name says,
[`HIERARCHY_DESCENDANTS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_node_navis.htm)
fetches all descendants for any nodes from a SQL hierarchy. It adds
`HIERARCHY_DISTANCE` as an additional hierarchy column to
the result set. Let us look at an example. All examples are code
snippets from `CL_DEMO_SQL_HIERARCHIES` again.

``` abap
DATA root_id TYPE demo_cds_simple_tree_view-id.

DATA sub_id TYPE demo_cds_simple_tree_view-id.

...

SELECT FROM HIERARCHY_DESCENDANTS(
              SOURCE demo_cds_simple_tree( p_id = @root_id )
              START WHERE id = @sub_id  )
  FIELDS id,
         parent_id,
         name,
         hierarchy_distance
  INTO TABLE @FINAL(descendants).
```

Our CDS hierarchy `DEMO_CDS_SIMPLE_TREE_VIEW` is used to
create a SQL hierarchy with a start node passed to parameter
`p_id` and for a node `sub_id` all descendants
are fetched. Running the program shows the result including the
additional column `HIERARCHY_DISTANCE` that contains the
distance to the respective start node. A further parameter
`DISTANCE` - not shown here - allows you to restrict the
distance to the respective start node.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Hierarchy Node Navigator HIERARCHY_ANCESTORS

Now the other way around: ABAP SQL function
[`HIERARCHY_ANCESTORS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_node_navis.htm)
returns the ancestors of any given node of an existing hierarchy:

``` abap
DATA root_id TYPE demo_cds_simple_tree_view-id.

DATA max_id TYPE demo_cds_simple_tree_view-id.

...

SELECT FROM HIERARCHY_ANCESTORS(
              SOURCE demo_cds_simple_tree( p_id = @root_id )
              START WHERE id = @max_id )
  FIELDS id,
          parent_id,
          name,
          hierarchy_distance
  INTO TABLE @FINAL(ancestors).
```

Looking at the result when running
`CL_DEMO_SQL_HIERARCHIES`, you see that the
value of column `HIERARCHY_DISTANCE` is negative now. Using
aggregate functions or evaluating the internal result table, you can now
easily extract further information like the number of ancestors and so
on.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Hierarchy Node Navigator HIERARCHY_SIBLINGS

Besides descendants and ancestors, hierarchy nodes also can have
siblings, that is nodes that have the same parent node. You can find
these by looking for all nodes with the same value in hierarchy column
`HIERARCHY_PARENT_RANK`. But there is also
[`HIERARCHY_SIBLINGS`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_node_navis.htm)
as a hierarchy function for that:

``` abap
DATA root_id TYPE demo_cds_simple_tree_view-id.

DATA sibl_id TYPE demo_cds_simple_tree_view-id.

...

SELECT FROM HIERARCHY_SIBLINGS(
              SOURCE demo_cds_simple_tree( p_id = @root_id )
              START WHERE id = @sibl_id )
  FIELDS id,
          parent_id,
          name,
          hierarchy_sibling_distance
  INTO TABLE @FINAL(siblings).
```

You see that this function adds another hierarchy column
`HIERARCHY_SIBLING_DISTANCE` that contains the distance to
the respective start node. Running
`CL_DEMO_SQL_HIERARCHIES`, where we start with
a node that definitely has some siblings, shows the result.

<p align="right"><a href="#top">⬆️ back to top</a></p>

### Hierarchy Aggregate Navigators

Finally let us turn to the [hierarchy aggregate
navigators](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenhierarchy_agg_navi_glosry.htm "Glossary Entry")
that allow you to apply some aggregate functions to descendants and
ancestors of any node of a SQL hierarchy:

-   [`HIERARCHY_DESCENDANTS_AGGREGATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_desc_agg.htm)
-   [`HIERARCHY_ANCESTORS_AGGREGATE`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_ancs_agg.htm)

We will show an example for the descendants case and refer to the
[documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_ancs_agg.htm)
for the ancestors.

Applying aggregate functions to columns normally means that you have
some data there for which this makes sense. In our simplistic SQL
hierarchy tree we do not have such meaningful data. On the other hand,
this can also be a use case: You can have the administrative data for
the parent-child relation in one database table and the real data in
another one. And for that use case, the hierarchy aggregate navigator
`HIERARCHY_DESCENDANTS_AGGREGATE` gives you the option of
joining such data to your SQL hierarchy:

``` abap
TYPES:
  BEGIN OF value,
    id     TYPE i,
    amount TYPE p LENGTH 16 DECIMALS 2,
  END OF value.

DATA value_tab TYPE SORTED TABLE OF value WITH UNIQUE KEY id.

DATA root_id TYPE demo_cds_simple_tree_view-id.

DATA sub_id TYPE demo_cds_simple_tree_view-id.

...

SELECT FROM HIERARCHY_DESCENDANTS_AGGREGATE(
              SOURCE demo_cds_simple_tree( p_id = @sub_id  ) AS h
              JOIN @value_tab AS v
                ON  v~id = h~id
              MEASURES SUM( v~amount ) AS amount_sum
              WHERE hierarchy_rank > 1
              WITH SUBTOTAL
              WITH BALANCE )
  FIELDS id,
         amount_sum,
         hierarchy_rank,
         hierarchy_aggregate_type
  INTO TABLE @FINAL(descendants_aggregate).
```

In our example, we join an internal table `value_tab` of the
same program to the SQL hierarchy. In a real life example you would join
another database table, of course. On the other hand the example shows
ABAP SQL's capability of using internal tables as data sources. You
even can go so far to evaluate hierarchical data in internal tables with
ABAP SQL by using an internal table as data source for a CTE hierarchy!

The example does the following:

-   We use the hierarchy aggregate navigator
    `HIERARCHY_DESCENDANTS_AGGREGATE` as a data source of a
    `FROM` clause.
-   Our CDS hierarchy `DEMO_CDS_SIMPLE_TREE_VIEW` joined
    with internal table `value_tab` is used as the data source.
-   The ABAP SQL function returns a tabular result of nodes of the data
    source.
-   The aggregate function `SUM` behind `MEASURES` sums
    up the values of the column amounts of the joined internal table for
    all descendants of each node returned by the ABAP SQL function.
-   The `WHERE` condition restricts the result set by a freely
    programmable condition.
-   The `WITH` additions add further rows to the result set that
    can be recognized by values in an additional hierarchy column
    `HIERARCHY_AGGREGATE_TYPE`:

    -   `WITH SUBTOTAL`

        In the row where `HIERARCHY_AGGREGATE_TYPE` has
        value 1, column `AMOUNT_SUM` contains the sum of the
        values of all hierarchy nodes that meet the `WHERE`
        condition.

    -   `WITH BALANCE`

        In the row where `HIERARCHY_AGGREGATE_TYPE` has
        value 2, column `AMOUNT_SUM` contains the sum of the
        values of all hierarchy nodes that do not meet the
        `WHERE` condition.

    For more `WITH` additions see the
    [documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_desc_agg.htm).

Running `CL_DEMO_SQL_HIERARCHIES` shows the
result. It also shows the result of the joined data source, where you
can check that the calculated values are correct.

<p align="right"><a href="#top">⬆️ back to top</a></p>


## More Information
For the complete reference documentation about SQL hierarchies, see [`SELECT, FROM hierarchy_data`](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenselect_hierarchy_data.htm).
