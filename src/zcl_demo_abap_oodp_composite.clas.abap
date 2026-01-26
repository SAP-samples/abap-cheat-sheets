"! <p class="shorttext"><strong>Composite</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the composite design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_composite DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA example_number TYPE i.
ENDCLASS.


CLASS zcl_demo_abap_oodp_composite IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Composite` ).

*&---------------------------------------------------------------------*
*& Creating members of the organization and setting up the
*& organizational hierarchy
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                                                 text = |{ example_number }) Creating members of the organization and setting up the organizational hierarchy| ).

    "The CEO object represents to the root node of the organizational hierarchy
    DATA(ceo) = NEW lcl_manager( is_data = VALUE #( id = 1 name = `Jasmin` job_role = `CEO` salary = 350000 office = `New York` ) ).
    DATA(hr_specialist) = NEW lcl_employee( is_data = VALUE #( id = 2 name = `Grace` job_role = `HR Specialist` salary = 85000 office = `New York` ) ).
    DATA(admin_assistant1) = NEW lcl_employee( is_data = VALUE #( id = 3 name = `Kim` job_role = `Administrative Assistant` salary = 85000 office = `New York` ) ).

    "Engineering department
    DATA(head_eng) = NEW lcl_manager( is_data = VALUE #( id = 4 name = `Michael` job_role = `Head of Engineering` salary = 220000 office = `San Francisco` ) ).
    DATA(admin_assistant2) = NEW lcl_employee( is_data = VALUE #( id = 5 name = `Matthew` job_role = `Administrative Assistant` salary = 85000 office = `New York` ) ).
    DATA(dev_manager) = NEW lcl_manager( is_data = VALUE #( id = 6 name = `Liam` job_role = `Development Manager` salary = 180000 office = `Berlin` ) ).
    DATA(dev1) = NEW lcl_employee( is_data = VALUE #( id = 7 name = `Ava` job_role = `Developer` salary = 130000 office = `Shanghai` ) ).
    DATA(dev2) = NEW lcl_employee( is_data = VALUE #( id = 8 name = `James` job_role = `Developer` salary = 120000 office = `Sydney` ) ).
    DATA(dev3) = NEW lcl_employee( is_data = VALUE #( id = 9 name = `Charlotte` job_role = `Developer` salary = 115000 office = `Cape Town` ) ).
    DATA(qa_manager) = NEW lcl_manager( is_data = VALUE #( id = 10 name = `Sophia` job_role = `QA Manager` salary = 170000 office = `Tokyo` ) ).
    DATA(qa_tester) = NEW lcl_employee( is_data = VALUE #( id = 11 name = `Isabella` job_role = `QA Tester` salary = 95000 office = `Buenos Aires` ) ).
    DATA(qa_automation) = NEW lcl_employee( is_data = VALUE #( id = 12 name = `William` job_role = `QA Automation Engineer` salary = 110000 office = `Toronto` ) ).

    "Sales department
    DATA(head_sales) = NEW lcl_manager( is_data = VALUE #( id = 13 name = `Olivia` job_role = `Head of Sales` salary = 210000 office = `London` ) ).
    DATA(sales_rep1) = NEW lcl_employee( is_data = VALUE #( id = 14 name = `Mia` job_role = `Sales Representative` salary = 90000 office = `Paris` ) ).
    DATA(sales_rep2) = NEW lcl_employee( is_data = VALUE #( id = 15 name = `Lucas` job_role = `Sales Representative` salary = 88000 office = `Dubai` ) ).
    DATA(sales_analyst) = NEW lcl_employee( is_data = VALUE #( id = 16 name = `Amelia` job_role = `Sales Analyst` salary = 95000 office = `Mumbai` ) ).

    "Building the hierarchy using the add_subordinate method,
    "reflecting the various hierarchies and reporting lines.
    "The add_subordinate method also establishes a reference
    "to the manager. To display the hierarchy in a meaningful
    "sequence (since the add_subordinate method just appends
    "lines to a table with a reference type, without further
    "sorting or ordering implementation for simplification),
    "the organization`s members are added in a specific order
    "for display purposes.
    ceo->add_subordinate( hr_specialist ).
    ceo->add_subordinate( admin_assistant1 ).

    ceo->add_subordinate( head_eng ).
    head_eng->add_subordinate( admin_assistant2 ).
    head_eng->add_subordinate( dev_manager ).

    dev_manager->add_subordinate( dev1 ).
    dev_manager->add_subordinate( dev2 ).
    dev_manager->add_subordinate( dev3 ).

    head_eng->add_subordinate( qa_manager ).
    qa_manager->add_subordinate( qa_tester ).
    qa_manager->add_subordinate( qa_automation ).

    ceo->add_subordinate( head_sales ).
    head_sales->add_subordinate( sales_rep1 ).
    head_sales->add_subordinate( sales_rep2 ).
    head_sales->add_subordinate( sales_analyst ).

    "Retrieving and displaying the employee hierarchy of
    "the entire organization
    out->write( |Hierarchy of the entire organization:\n\n| ).
    DATA(org) = ceo->lif_org~get_employee_hierarchy( ).
    out->write( org ).
    out->write( |\n| ).

    out->write( |Hierarchy of the engineering department:\n\n| ).
    DATA(org_eng_dep) = head_eng->lif_org~get_employee_hierarchy( ).
    out->write( org_eng_dep ).
    out->write( |\n| ).

    out->write( |Hierarchy of the sales department:\n\n| ).
    DATA(org_sales_dep) = head_sales->lif_org~get_employee_hierarchy( ).
    out->write( org_sales_dep ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Retrieving employees by role
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Retrieving employees by role| ).

    "Developers in the entire organization
    DATA(role) = `Developer`.
    DATA(devs_in_org) = ceo->lif_org~get_employees_by_role( role ).
    DATA(count) = lines( devs_in_org ).
    out->write( |There { COND #( WHEN count = 1 THEN `is` ELSE `are` ) } { count } { to_lower( role ) }{ COND #( WHEN count <> 1 THEN `s` ) } in the entire organization:| ).
    LOOP AT devs_in_org INTO DATA(dev).
      out->write( |- { dev->get_data( )-name }| ).
    ENDLOOP.
    out->write( |\n| ).

    "Administrative assistants in the engineering department
    role = `Administrative Assistant`.
    DATA(admin_in_eng_dep) = head_eng->lif_org~get_employees_by_role( role ).
    count = lines( admin_in_eng_dep ).
    out->write( |There { COND #( WHEN count = 1 THEN `is` ELSE `are` ) } { count } { to_lower( role ) }{ COND #( WHEN count <> 1 THEN `s` ) } in the engineering department:| ).
    LOOP AT admin_in_eng_dep INTO DATA(adm).
      out->write( |- { adm->get_data( )-name }| ).
    ENDLOOP.
    out->write( |\n| ).

    role = `Sales Representative`.
    DATA(sales_reps_in_eng_dep) = head_eng->lif_org~get_employees_by_role( role ).
    count = lines( sales_reps_in_eng_dep ).
    out->write( |There { COND #( WHEN count = 1 THEN `is` ELSE `are` ) } { count } { to_lower( role ) }{ COND #( WHEN count <> 1 THEN `s` ) } in the engineering department.| ).
    LOOP AT sales_reps_in_eng_dep INTO DATA(sales_rep).
      out->write( |- { sales_rep->get_data( )-name }| ).
    ENDLOOP.
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Retrieving employees by office
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Retrieving employees by office| ).

    DATA(office) = `New York`.
    DATA(org_ny_location) = ceo->lif_org~get_employees_by_office( office ).
    count = lines( org_ny_location ).
    out->write( |There { COND #( WHEN count = 1 THEN `is` ELSE `are` ) } { count } member{ COND #( WHEN count <> 1 THEN `s` ) } in the organization located in the { office } office:| ).
    LOOP AT org_ny_location INTO DATA(ny).
      out->write( |- { ny->get_data( )-name }| ).
    ENDLOOP.
    out->write( |\n| ).

    DATA(eng_dep_ny_location) = head_eng->lif_org~get_employees_by_office( office ).
    count = lines( eng_dep_ny_location ).
    out->write( |There { COND #( WHEN count = 1 THEN `is` ELSE `are` ) } { count } member{ COND #( WHEN count <> 1 THEN `s` ) } in the engineering department located in the { office } office:| ).
    LOOP AT eng_dep_ny_location INTO DATA(ny_eng).
      out->write( |- { ny_eng->get_data( )-name }| ).
    ENDLOOP.
    out->write( |\n| ).

    DATA(sales_dep_ny_location) = head_sales->lif_org~get_employees_by_office( office ).
    count = lines( sales_dep_ny_location ).
    out->write( |There { COND #( WHEN count = 1 THEN `is` ELSE `are` ) } { count } member{ COND #( WHEN count <> 1 THEN `s` ) } in the sales department located in the { office } office.| ).
    LOOP AT sales_dep_ny_location INTO DATA(ny_sales).
      out->write( |- { ny_sales->get_data( )-name }| ).
    ENDLOOP.
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Assigning training
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Assigning training| ).

    "Assigning training to all members of the organization
    out->write( `Training A` ).
    DATA(trainings_assigned) = ceo->lif_org~assign_training( `Training A` ).
    out->write( trainings_assigned ).
    out->write( |\n| ).

    "Assigning training to all members of the engineering department
    out->write( `Training B` ).
    trainings_assigned = head_eng->lif_org~assign_training( `Training B` ).
    out->write( trainings_assigned ).
    out->write( |\n| ).

    "Assigning training to a specific member
    out->write( `Training C` ).
    trainings_assigned = qa_tester->lif_org~assign_training( `Training C` ).
    out->write( trainings_assigned ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Retrieving employee information as JSON (inluding reporting line)
*&---------------------------------------------------------------------*

    "The example is designed in a way that the ID must refer to a subordinate
    "of the reference used. The example implementation does not, for example,
    "work for ID 12 via the head_sales reference.

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Retrieving employee information as JSON (inluding reporting line)| ).

    out->write( `Retrieving information about member with ID 3 in the hierarchy via the ceo reference` ).
    out->write( |\n| ).
    DATA(json_id_3) = ceo->lif_org~get_employee_info( 3 ).
    out->write( json_id_3 ).
    out->write( |\n| ).

    out->write( `Retrieving information about member with ID 16 in the hierarchy via the head_sales reference` ).
    out->write( |\n| ).
    DATA(json_id_16) = head_sales->lif_org~get_employee_info( 16 ).
    out->write( json_id_16 ).
    out->write( |\n| ).

    out->write( `Retrieving information about member with ID 12 in the hierarchy via the qa_manager reference` ).
    out->write( |\n| ).
    DATA(json_id_12a) = qa_manager->lif_org~get_employee_info( 12 ).
    out->write( json_id_12a ).
    out->write( |\n| ).

    out->write( `Retrieving information about member with ID 12 in the hierarchy via the head_eng reference` ).
    out->write( |\n| ).
    DATA(json_id_12b) = head_eng->lif_org~get_employee_info( 12 ).
    out->write( json_id_12b ).
    out->write( |\n| ).

    out->write( `Retrieving information about member with ID 12 in the hierarchy via the ceo reference` ).
    out->write( |\n| ).
    DATA(json_id_12c) = ceo->lif_org~get_employee_info( 12 ).
    out->write( json_id_12c ).
    out->write( |\n| ).

    out->write( `No direct managers in case of ID 1 (the root node)` ).
    out->write( |\n| ).
    DATA(json_id_1) = ceo->lif_org~get_employee_info( 1 ).
    out->write( json_id_1 ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Retrieving headcount
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Retrieving headcount| ).

    "Total headcount retrieved via the root node
    DATA(headcount) = ceo->lif_org~get_headcount( ).
    out->write( |Total headcount of the entire organization: { headcount }\n| ).

    "Headcount of engineering department
    headcount = head_eng->lif_org~get_headcount( ).
    out->write( |Engineering department headcount: { headcount }\n| ).

    "Headcount of sales department
    headcount = head_sales->lif_org~get_headcount( ).
    out->write( |Sales department headcount: { headcount }\n| ).

    "Headcount of a specific member that has no subordinates
    headcount = dev1->lif_org~get_headcount( ).
    out->write( |Headcount of a specific member: { headcount }\n| ).

*&---------------------------------------------------------------------*
*& Retrieving salary
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Retrieving salary| ).

    "Salary of the CEO + the entire organization
    ceo->lif_org~get_salary( IMPORTING individual_salary  = DATA(ind_salary)
                                       accumulated_salary = DATA(acc_salary) ).

    out->write( |Individual salary of the CEO: { ind_salary }| ).
    out->write( |Total salary of the entire organization: { acc_salary }\n| ).

    "Salary of the engineering department head + the entire engineering department
    head_eng->lif_org~get_salary( IMPORTING individual_salary  = ind_salary
                                            accumulated_salary = acc_salary ).

    out->write( |Individual salary of the engineering department head: { ind_salary }| ).
    out->write( |Total salary of the entire engineering department: { acc_salary }\n| ).

    "Salary of the sales department head + the entire sales department
    head_sales->lif_org~get_salary( IMPORTING individual_salary  = ind_salary
                                              accumulated_salary = acc_salary ).

    out->write( |Individual salary of the sales department head: { ind_salary }| ).
    out->write( |Total salary of the entire sales department: { acc_salary }\n| ).

    "Salary of a specific member
    dev1->lif_org~get_salary( IMPORTING individual_salary  = ind_salary
                                        accumulated_salary = acc_salary ).

    out->write( |Individual salary of a specific member: { ind_salary }| ).
    out->write( |Total salary of a specific member (no subordinates): { acc_salary }\n| ).

*&---------------------------------------------------------------------*
*& Removing subordinates
*&---------------------------------------------------------------------*

    example_number += 1.
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out
                         text = |{ example_number }) Removing subordinates| ).

    "A composite trying to remove itself is ruled out.
    ceo->remove_subordinate( ceo  ).
    DATA(org_test1) = ceo->lif_org~get_employee_hierarchy( ).
    DATA(hc1) = ceo->lif_org~get_headcount( ).

    "A composite trying to remove itself is ruled out.
    dev_manager->remove_subordinate( dev_manager  ).
    DATA(org_test2) = ceo->lif_org~get_employee_hierarchy( ).
    DATA(hc2) = ceo->lif_org~get_headcount( ).
    ASSERT org_test2 = org_test1.
    ASSERT hc2 = hc1.

    "A manager (composite) trying to remove an employee (leaf) that is not
    "a subordinate is ruled out.
    dev_manager->remove_subordinate( admin_assistant1 ).
    DATA(org_test3) = ceo->lif_org~get_employee_hierarchy( ).
    DATA(hc3) = ceo->lif_org~get_headcount( ).
    ASSERT hc3 = hc1.
    ASSERT org_test3 = org_test1.

    "Removal example 1
    "The removal is performed via the ceo reference (the root node).
    out->write( |--------------- Removing member with ID 2 ---------------\n| ).
    ceo->remove_subordinate( hr_specialist  ).
    out->write( |New total headcount: { ceo->lif_org~get_headcount( ) }\n| ).
    out->write( `New hierarchy:` ).
    org = ceo->lif_org~get_employee_hierarchy( ).
    out->write( org ).
    out->write( |\n| ).

    "Removal example 2
    "The removal is performed via the head_sales reference.
    out->write( |--------------- Removing member with ID 15 ---------------\n| ).
    head_sales->remove_subordinate( sales_rep2 ).
    out->write( |New total headcount: { ceo->lif_org~get_headcount( ) }\n| ).
    out->write( `New hierarchy:` ).
    org = ceo->lif_org~get_employee_hierarchy( ).
    out->write( org ).
    out->write( |\n| ).

    "Removal example 3
    "The removal is performed via the ceo reference (the root node). In this
    "example, a composite is removed. The example is implemented in a way that
    "the subordinate members arere assigned to the manager of the deleted manager.
    out->write( |--------------- Removing member with ID 10 ---------------\n| ).
    ceo->remove_subordinate( qa_manager ).
    out->write( |New total headcount: { ceo->lif_org~get_headcount( ) }\n| ).
    out->write( `New hierarchy:` ).
    org = ceo->lif_org~get_employee_hierarchy( ).
    out->write( org ).
    out->write( |\n| ).

    "Removal example 4
    "The removal is performed via the ceo reference (the root node). In this example,
    "another composite is removed.
    out->write( |--------------- Removing member with ID 13 ---------------\n| ).
    ceo->remove_subordinate( head_sales ).
    out->write( |New total headcount: { ceo->lif_org~get_headcount( ) }\n| ).
    out->write( `New hierarchy:` ).
    org = ceo->lif_org~get_employee_hierarchy( ).
    out->write( org ).
    out->write( |\n| ).

    "Removal example 5
    "The removal is performed via the ceo reference (the root node). In this example,
    "another composite is removed.
    out->write( |--------------- Removing member with ID 4 ---------------\n| ).
    ceo->remove_subordinate( head_eng ).
    out->write( |New total headcount: { ceo->lif_org~get_headcount( ) }\n| ).
    out->write( `New hierarchy:` ).
    org = ceo->lif_org~get_employee_hierarchy( ).
    out->write( org ).
    out->write( |\n| ).
  ENDMETHOD.
ENDCLASS.
