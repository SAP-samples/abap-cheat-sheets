"! <p class="shorttext"><strong>Memento</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the memento design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_memento DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_memento IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Memento` ).

*&-------------------------------------------------------------------------------------*
*& 1) Creating objects and mementos, changing and getting states
*&-------------------------------------------------------------------------------------*

zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `1) Creating objects and mementos, changing and getting states` ).

    DATA(oref_lcl) = lcl=>get_obj( ).
    DATA(oref_memento_ctrl)  = NEW lcl_memento_controller( oref_lcl ).

    oref_lcl->change_state( `Lorem` ).
    oref_memento_ctrl->save_memento( `TAG_1` ).

    DATA(state_a) = oref_lcl->get_state( ).
    out->write( state_a ).

    oref_lcl->change_state( `ipsum` ).
    oref_memento_ctrl->save_memento( `TAG_2` ).

    DATA(state_b) = oref_lcl->get_state( ).
    out->write( state_b ).

    oref_lcl->change_state( `dolor` ).
    oref_memento_ctrl->save_memento( `TAG_3` ).

    DATA(state_c) = oref_lcl->get_state( ).
    out->write( state_c ).

    oref_lcl->change_state( `sit` ).
    oref_memento_ctrl->save_memento( `TAG_4` ).

    DATA(state_d) = oref_lcl->get_state(  ).
    out->write( state_d ).

    oref_lcl->change_state( `amet` ).
    oref_memento_ctrl->save_memento( `TAG_5` ).

    DATA(state_e) = oref_lcl->get_state( ).
    out->write( state_e ).

*&-------------------------------------------------------------------------------------*
*& 2) Memento log
*&-------------------------------------------------------------------------------------*

zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `2) Memento log` ).

    DATA(memento_log) = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

*&-------------------------------------------------------------------------------------*
*& 3) Undo operations
*&-------------------------------------------------------------------------------------*

zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `3) Undo operations` ).

    DATA(state_f) = oref_lcl->get_state( ).
    out->write( `Current state:` ).
    out->write( state_f ).

    out->write( |\n| ).
    out->write( `State after first undo operation:` ).
    oref_memento_ctrl->undo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_g) = oref_lcl->get_state( ).
    out->write( state_g ).

    oref_memento_ctrl->undo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_h) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after second undo operation:` ).
    out->write( state_h ).

    oref_memento_ctrl->undo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_i) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after third undo operation:` ).
    out->write( state_i && |\n\n| ).

    memento_log = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

*&-------------------------------------------------------------------------------------*
*& 4) Redo operations
*&-------------------------------------------------------------------------------------*

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `4) Redo operations` ).

    DATA(state_j) = oref_lcl->get_state( ).
    out->write( `Current state:` ).
    out->write( state_j ).

    oref_memento_ctrl->redo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_k) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after first redo operation:` ).
    out->write( state_k && |\n\n| ).

    memento_log = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

    oref_memento_ctrl->redo( CHANGING lcl_obj = oref_lcl ).

    DATA(state_l) = oref_lcl->get_state( ).

    out->write( |\n| ).
    out->write( `State after second redo operation:` ).
    out->write( state_l && |\n\n| ).

    memento_log = oref_memento_ctrl->get_memento_log( ).
    out->write( data = memento_log name = `memento_log` ).

*&-------------------------------------------------------------------------------------*
*& 5) Restoring by tag
*&-------------------------------------------------------------------------------------*

      zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `5) Restoring by tag` ).

    DATA(state_m) = oref_lcl->get_state( ).
    out->write( `Current state:` ).
    out->write( state_m ).
    out->write( |\n| ).

    DATA flag TYPE abap_boolean.

    "At this stage, the memento with the tag TAG_5 is not stored in the memento table but
    "in the redo table. Therefore, the current state remains unchanged because the restoration
    "only considers mementos available in the memento table in the example.
    out->write( `Tag used: TAG_5` ).
    oref_memento_ctrl->restore_by_tag( EXPORTING tag = `TAG_5`
                                       CHANGING  lcl_obj = oref_lcl
                                       RECEIVING restoration_ok = flag ).

    DATA(state_n) = oref_lcl->get_state( ).
    out->write( |Restoration operation successful? { COND #( WHEN flag = abap_true THEN `YES` ELSE `NO` ) }| ).
    out->write( state_n ).
    out->write( |\n| ).

    out->write( `Tag used: TAG_2` ).
    oref_memento_ctrl->restore_by_tag( EXPORTING tag = `TAG_2`
                                       CHANGING  lcl_obj = oref_lcl
                                       RECEIVING restoration_ok = flag ).

    DATA(state_o) = oref_lcl->get_state( ).
    out->write( |Restoration operation successful? { COND #( WHEN flag = abap_true THEN `YES` ELSE `NO` ) }| ).
    out->write( state_o ).
    out->write( |\n| ).

    "Checking on multiple tags in a loop
    DO 7 TIMES.
      DATA(tag) = |TAG_{ sy-index }|.
      out->write( |Tag used: { tag }| ).

      oref_memento_ctrl->restore_by_tag( EXPORTING tag = tag
                                         CHANGING  lcl_obj = oref_lcl
                                         RECEIVING restoration_ok = flag ).

      DATA(state_p) = oref_lcl->get_state( ).
      out->write( |Restoration operation successful? { COND #( WHEN flag = abap_true THEN `YES` ELSE `NO` ) }| ).
      out->write( state_p ).
      out->write( |\n| ).
    ENDDO.
  ENDMETHOD.
ENDCLASS.
