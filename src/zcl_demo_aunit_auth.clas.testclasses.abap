CLASS ltc_auth DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA ctrl_auth TYPE REF TO if_aunit_auth_check_controller.
    DATA cut TYPE REF TO zcl_demo_aunit_auth.
    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS teardown.

    METHODS allow_all_authorizations.
    METHODS restrict_to_chg_disp_auth RAISING cx_static_check.
    METHODS assert_auth
      IMPORTING
        field_value        TYPE zcl_demo_aunit_auth=>enum_field_value
        exp_is_authorized  TYPE abap_boolean
        msg                TYPE string OPTIONAL.
    METHODS assert_execution_log_counts
      IMPORTING
        exp_passed TYPE i
        exp_failed TYPE i.

    METHODS test_create_full_auth FOR TESTING.
    METHODS test_change_full_auth FOR TESTING.
    METHODS test_display_full_auth FOR TESTING.
    METHODS test_delete_full_auth FOR TESTING.

    METHODS test_create_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_change_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_display_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_delete_chg_disp_restr FOR TESTING RAISING cx_static_check.
    METHODS test_exec_log_chg_disp_restr FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_auth IMPLEMENTATION.
  METHOD class_setup.
    ctrl_auth = cl_aunit_authority_check=>get_controller( ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
    allow_all_authorizations( ).
  ENDMETHOD.

  METHOD teardown.
    ctrl_auth->reset( ).
  ENDMETHOD.

  METHOD allow_all_authorizations.
    ctrl_auth->reset( ).
  ENDMETHOD.

  METHOD restrict_to_chg_disp_auth.
    DATA(auth_change_display) = VALUE cl_aunit_auth_check_types_def=>role_auth_objects(
        ( object         = 'ZAUTH_OB'
          authorizations = VALUE #( ( VALUE #( ( fieldname   = 'ACTVT'
                                                 fieldvalues = VALUE #( ( lower_value = '02' )
                                                                        ( lower_value = '03' ) ) ) ) ) ) ) ).

    DATA(auth_obj_set) = cl_aunit_authority_check=>create_auth_object_set(
      VALUE cl_aunit_auth_check_types_def=>user_role_authorizations( ( role_authorizations = auth_change_display ) ) ).

    ctrl_auth->restrict_authorizations_to( auth_obj_set ).
  ENDMETHOD.

  METHOD assert_auth.
    cl_abap_unit_assert=>assert_equals(
      act = cut->call_authority_check( field_value )
      exp = exp_is_authorized
      msg = msg ).
  ENDMETHOD.

  METHOD assert_execution_log_counts.
    ctrl_auth->get_auth_check_execution_log( )->get_execution_status(
      IMPORTING
        passed_execution = DATA(passed)
        failed_execution = DATA(failed) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( passed )
      exp = exp_passed
      msg = 'Unexpected number of passed AUTHORITY-CHECK executions.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( failed )
      exp = exp_failed
      msg = 'Unexpected number of failed AUTHORITY-CHECK executions.' ).
  ENDMETHOD.

  METHOD test_create_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>create
      exp_is_authorized = abap_true
      msg               = 'Create should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_change_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>change
      exp_is_authorized = abap_true
      msg               = 'Change should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_display_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>display
      exp_is_authorized = abap_true
      msg               = 'Display should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_delete_full_auth.
    allow_all_authorizations( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>delete
      exp_is_authorized = abap_true
      msg               = 'Delete should be authorized in unrestricted mode.' ).
  ENDMETHOD.

  METHOD test_create_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>create
      exp_is_authorized = abap_false
      msg               = 'Create should be blocked when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_change_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>change
      exp_is_authorized = abap_true
      msg               = 'Change should be allowed when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_display_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>display
      exp_is_authorized = abap_true
      msg               = 'Display should be allowed when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_delete_chg_disp_restr.
    restrict_to_chg_disp_auth( ).
    assert_auth(
      field_value       = zcl_demo_aunit_auth=>delete
      exp_is_authorized = abap_false
      msg               = 'Delete should be blocked when only change/display are allowed.' ).
  ENDMETHOD.

  METHOD test_exec_log_chg_disp_restr.
    restrict_to_chg_disp_auth( ).

    assert_auth( field_value = zcl_demo_aunit_auth=>create exp_is_authorized = abap_false ).
    assert_auth( field_value = zcl_demo_aunit_auth=>change exp_is_authorized = abap_true ).
    assert_auth( field_value = zcl_demo_aunit_auth=>display exp_is_authorized = abap_true ).
    assert_auth( field_value = zcl_demo_aunit_auth=>delete exp_is_authorized = abap_false ).

    assert_execution_log_counts(
      exp_passed = 2
      exp_failed = 2 ).
  ENDMETHOD.
ENDCLASS.
