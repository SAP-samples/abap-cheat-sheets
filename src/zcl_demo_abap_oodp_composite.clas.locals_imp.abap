*&---------------------------------------------------------------------*
*& Component interface
*&---------------------------------------------------------------------*

INTERFACE lif_org.

  "Type for information about individual organization members
  TYPES: BEGIN OF ty_employee_data,
           id       TYPE i,
           name     TYPE string,
           job_role TYPE string,
           office   TYPE string,
           salary   TYPE i,
         END OF ty_employee_data,
         tt_org TYPE TABLE OF REF TO lif_org WITH EMPTY KEY.

  "Returns salary of individual members and combined
  "salary of manager and subordinates
  METHODS get_salary
    EXPORTING individual_salary  TYPE i
              accumulated_salary TYPE i.

  "Returns headcount of individual members or combined
  "headcount of manager and subordinates
  METHODS get_headcount
    RETURNING VALUE(count) TYPE i.

  "Returns the organizational hierarchy
  METHODS get_employee_hierarchy
    IMPORTING level                   TYPE i DEFAULT 0
    RETURNING VALUE(employee_details) TYPE string_table.

  "Returns employee data
  METHODS get_data
    RETURNING VALUE(data) TYPE ty_employee_data.

  "Sets the manager
  METHODS set_manager
    IMPORTING manager TYPE REF TO lif_org.

  "Returns the manager
  METHODS get_manager
    RETURNING VALUE(manager) TYPE REF TO lif_org.

  "Returns employees filtered by role
  METHODS get_employees_by_role
    IMPORTING role             TYPE string
    RETURNING VALUE(employees) TYPE tt_org.

  "Returns employees filtered by office
  METHODS get_employees_by_office
    IMPORTING office           TYPE string
    RETURNING VALUE(employees) TYPE tt_org.

  "Returns information about specific members as JSON,
  "including the reporting line
  METHODS get_employee_info
    IMPORTING employee_id TYPE i
    RETURNING VALUE(json) TYPE string.

  "Assigns a training
  METHODS assign_training
    IMPORTING training_name    TYPE string
    RETURNING VALUE(assignees) TYPE string_table.

  "Types for the JSON creation in the get_employee_info
  "method implemenation
  TYPES: BEGIN OF ty_manager_info,
           id       TYPE i,
           name     TYPE string,
           job_role TYPE string,
         END OF ty_manager_info,
         tt_manager_info TYPE TABLE OF ty_manager_info WITH EMPTY KEY,
         BEGIN OF ty_employee_json,
           id              TYPE i,
           name            TYPE string,
           job_role        TYPE string,
           direct_managers TYPE tt_manager_info,
         END OF ty_employee_json.

ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Leaf
*&---------------------------------------------------------------------*

CLASS lcl_employee DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_org.
    METHODS constructor
      IMPORTING is_data TYPE lif_org~ty_employee_data.

  PRIVATE SECTION.
    DATA: employee_data TYPE lif_org~ty_employee_data,
          mngr          TYPE REF TO lif_org.
ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.
  METHOD constructor.
    "Assumption: No duplicate IDs are expected.
    employee_data = is_data.
  ENDMETHOD.

  METHOD lif_org~get_data.
    data = employee_data.
  ENDMETHOD.

  METHOD lif_org~set_manager.
    mngr = manager.
  ENDMETHOD.

  METHOD lif_org~get_salary.
    individual_salary = employee_data-salary.
    "Assiging the same value to accumulated_salary as
    "leaves do not have subordinates.
    accumulated_salary = employee_data-salary.
  ENDMETHOD.

  METHOD lif_org~get_headcount.
    count = 1.
  ENDMETHOD.

  METHOD lif_org~get_employee_hierarchy.
    DATA(indent) = ` `.
    DO level TIMES.
      indent &&= `       |`.
    ENDDO.
    APPEND |{ indent }-- (LEAF) ID { employee_data-id }, { employee_data-name }, { employee_data-job_role }, { employee_data-office }| TO employee_details.
  ENDMETHOD.

  METHOD lif_org~get_employees_by_role.
    IF employee_data-job_role = role.
      APPEND me TO employees.
    ENDIF.
  ENDMETHOD.

  METHOD lif_org~get_employees_by_office.
    IF employee_data-office = office.
      APPEND me TO employees.
    ENDIF.
  ENDMETHOD.

  METHOD lif_org~get_employee_info.
    IF employee_data-id <> employee_id.
      RETURN.
    ENDIF.

    DATA: employee_json   TYPE lif_org~ty_employee_json,
          managers_tab    TYPE lif_org~tt_manager_info,
          current_manager TYPE REF TO lif_org.

    employee_json-id       = employee_data-id.
    employee_json-name     = employee_data-name.
    employee_json-job_role = employee_data-job_role.

    "Moving up the hierachy to retrieve managers
    current_manager = mngr.
    WHILE current_manager IS BOUND.
      DATA(manager_data) = current_manager->get_data( ).
      APPEND VALUE #( id = manager_data-id name = manager_data-name job_role = manager_data-job_role ) TO managers_tab.
      current_manager = current_manager->get_manager( ).
    ENDWHILE.

    employee_json-direct_managers = managers_tab.

    json = /ui2/cl_json=>serialize( data          = employee_json
                                    format_output = abap_true
                                    pretty_name   = /ui2/cl_json=>pretty_mode-camel_case ).
  ENDMETHOD.

  METHOD lif_org~assign_training.
    APPEND |Assigning training '{ training_name }' to employee { employee_data-name }| TO assignees.
  ENDMETHOD.

  METHOD lif_org~get_manager.
    manager = mngr.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Composite
*&---------------------------------------------------------------------*

CLASS lcl_manager DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_org.

    METHODS constructor
      IMPORTING is_data TYPE lif_org~ty_employee_data.

    METHODS add_subordinate
      IMPORTING component TYPE REF TO lif_org.

    METHODS remove_subordinate
      IMPORTING component TYPE REF TO lif_org.

    METHODS get_subordinates
      RETURNING VALUE(subordinates) TYPE lif_org~tt_org.

  PRIVATE SECTION.
    DATA: employee_data    TYPE lif_org~ty_employee_data,
          mngr             TYPE REF TO lif_org,
          subordinates_tab TYPE lif_org~tt_org.
ENDCLASS.

CLASS lcl_manager IMPLEMENTATION.
  METHOD constructor.
    "Assumption: No duplicate IDs are expected.
    employee_data = is_data.
  ENDMETHOD.

  METHOD lif_org~get_data.
    data = employee_data.
  ENDMETHOD.

  METHOD add_subordinate.
    APPEND component TO subordinates_tab.
    "Setting the manager reference on the child
    component->set_manager( me ).
  ENDMETHOD.

  METHOD remove_subordinate.
    "Example logic:
    "- Checking if the component is a direct subordinate. If it is a leaf, it is removed directly.
    "  If it is a manager, its subordinates are assigned to the manager's manager. Then, the
    "  component is removed.
    "- If the component is not a direct subordinate, then the search is delegated to subordinates.
    "- Each subordinate is checked if a type cast to manager can be performed. The remove_subordinate
    "  method is called recursively. If the cast cannot be performed, it means the subordinate is a
    "  leaf node which cannot contain other components. In that case, the iteration is continued with
    "  the next subordinate.

    READ TABLE subordinates_tab INTO DATA(ref) WITH KEY table_line = component.
    IF sy-subrc = 0.
      IF ref IS INSTANCE OF lcl_manager.
        DATA(manager_of_manager) = ref->get_manager( ).
        DATA(subordinates_of_manager) = CAST lcl_manager( ref )->get_subordinates( ).

        LOOP AT subordinates_of_manager INTO DATA(sub).
          sub->set_manager( manager_of_manager ).
          CAST lcl_manager( manager_of_manager )->add_subordinate( sub ).
        ENDLOOP.

        DELETE subordinates_tab WHERE table_line = component.
        RETURN.
      ELSE.
        DELETE subordinates_tab WHERE table_line = component.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT subordinates_tab INTO sub.
      IF sub IS INSTANCE OF lcl_manager.
        "Type cast to manager as only managers can have subordinates
        DATA(manager) = CAST lcl_manager( sub ).
        manager->remove_subordinate( component ).
      ELSE.
        "Continuing with the iteration as the current node is
        "a leaf node, which does not have subordinates.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_org~set_manager.
    mngr = manager.
  ENDMETHOD.

  METHOD lif_org~get_manager.
    manager = mngr.
  ENDMETHOD.

  METHOD lif_org~get_salary.
    individual_salary = employee_data-salary.
    accumulated_salary = employee_data-salary.

    LOOP AT subordinates_tab INTO DATA(sub).
      sub->get_salary( IMPORTING accumulated_salary = DATA(acc_salary) ).
      accumulated_salary += acc_salary.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_org~get_headcount.
    count = 1.
    LOOP AT subordinates_tab INTO DATA(sub).
      count += sub->get_headcount( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_org~get_employee_hierarchy.
    DATA(indent) = ` `.
    DO level TIMES.
      indent &&= `       |`.
    ENDDO.

    APPEND |{ indent }-- (COMPOSITE) ID { employee_data-id }, { employee_data-name }, { employee_data-job_role }, { employee_data-office }| TO employee_details.

    LOOP AT subordinates_tab INTO DATA(sub).
      DATA(line) = sub->get_employee_hierarchy( level = level + 1 ).
      APPEND LINES OF line TO employee_details.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_org~get_employees_by_role.
    IF employee_data-job_role = role.
      APPEND me TO employees.
    ENDIF.

    LOOP AT subordinates_tab INTO DATA(sub).
      DATA(empl) = sub->get_employees_by_role( role ).
      APPEND LINES OF empl TO employees.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_org~get_employees_by_office.
    IF employee_data-office = office.
      APPEND me TO employees.
    ENDIF.

    LOOP AT subordinates_tab INTO DATA(sub).
      DATA(off) = sub->get_employees_by_office( office ).
      APPEND LINES OF off TO employees.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_org~get_employee_info.
    IF employee_data-id = employee_id.
      DATA: employee_json   TYPE lif_org~ty_employee_json,
            managers_tab    TYPE lif_org~tt_manager_info,
            current_manager TYPE REF TO lif_org.

      employee_json-id       = employee_data-id.
      employee_json-name     = employee_data-name.
      employee_json-job_role = employee_data-job_role.

      current_manager = mngr.
      WHILE current_manager IS BOUND.
        DATA(manager_data) = current_manager->get_data( ).
        APPEND VALUE #( id = manager_data-id name = manager_data-name job_role = manager_data-job_role ) TO managers_tab.
        current_manager = current_manager->get_manager( ).
      ENDWHILE.

      employee_json-direct_managers = managers_tab.

      json = /ui2/cl_json=>serialize( data = employee_json
                                      format_output = abap_true
                                      pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
      RETURN.
    ENDIF.

    LOOP AT subordinates_tab INTO DATA(sub).
      DATA(lv_sub_json) = sub->get_employee_info( employee_id ).
      IF lv_sub_json IS NOT INITIAL.
        json = lv_sub_json.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_org~assign_training.
    APPEND |Assigning training '{ training_name }' to manager { employee_data-name } and team| TO assignees.

    LOOP AT subordinates_tab INTO DATA(sub).
      DATA(trainings) = sub->assign_training( training_name = training_name ).
      APPEND LINES OF trainings TO assignees.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_subordinates.
    subordinates = subordinates_tab.
  ENDMETHOD.
ENDCLASS.
