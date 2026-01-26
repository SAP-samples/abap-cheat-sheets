*&---------------------------------------------------------------------*
*& Command interface
*&---------------------------------------------------------------------*

INTERFACE lif_command.
  METHODS: execute,
    undo.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Receiver class
*&---------------------------------------------------------------------*

CLASS lcl_receiver DEFINITION.
  PUBLIC SECTION.
    DATA: wd  TYPE i,
          ht  TYPE i,
          ang TYPE i.

    METHODS:
      constructor IMPORTING width TYPE i height TYPE i,
      resize IMPORTING width TYPE i height TYPE i,
      rotate IMPORTING angle TYPE i,
      get_current_state RETURNING VALUE(state) TYPE string.
ENDCLASS.

CLASS lcl_receiver IMPLEMENTATION.
  METHOD constructor.
    wd  = width.
    ht = height.
    ang  = 0.
  ENDMETHOD.

  METHOD resize.
    wd  = width.
    ht = height.

    APPEND |New image size: { wd }x{ ht }| TO zcl_demo_abap_oodp_command=>log.
  ENDMETHOD.

  METHOD rotate.
    ang = ( ang + angle ) MOD 360.

    APPEND |Image rotated to { ang } (angle used: { angle })| TO zcl_demo_abap_oodp_command=>log.
  ENDMETHOD.

  METHOD get_current_state.
    state = |Current image state: { wd }x{ ht }, angle { ang } |.
    APPEND state TO zcl_demo_abap_oodp_command=>log.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete command classes
*&---------------------------------------------------------------------*

CLASS lcl_cmd_resize DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS: constructor IMPORTING receiver TYPE REF TO object
                                   width    TYPE i
                                   height   TYPE i.
  PRIVATE SECTION.
    DATA: rec             TYPE REF TO object,
          wd              TYPE i,
          ht              TYPE i,
          previous_width  TYPE i,
          previous_height TYPE i.
ENDCLASS.

CLASS lcl_cmd_resize IMPLEMENTATION.
  METHOD constructor.
    rec = receiver.
    wd  = width.
    ht = height.
  ENDMETHOD.

  METHOD lif_command~execute.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      previous_width  = img->wd.
      previous_height = img->ht.
      img->resize( width = wd height = ht ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_command~undo.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      DATA(current_wd) = img->wd.
      DATA(current_ht) = img->ht.
      img->resize( width = previous_width height = previous_height ).
      APPEND |Resizing undone: { previous_width }x{ previous_height } (previous value: { current_wd }x{ current_ht })| TO zcl_demo_abap_oodp_command=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cmd_rotate DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_command.
    METHODS: constructor IMPORTING receiver TYPE REF TO object
                                   angle    TYPE i.
  PRIVATE SECTION.
    DATA: rec            TYPE REF TO object,
          ang            TYPE i,
          previous_angle TYPE i.
ENDCLASS.

CLASS lcl_cmd_rotate IMPLEMENTATION.
  METHOD constructor.
    rec = receiver.
    ang = angle.
  ENDMETHOD.

  METHOD lif_command~execute.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      previous_angle = img->ang.
      img->rotate( angle = ang ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_command~undo.
    IF rec IS BOUND AND rec IS INSTANCE OF lcl_receiver.
      DATA(img) = CAST lcl_receiver( rec ).
      DATA(current_value) = img->ang.
      img->ang = previous_angle.
      APPEND |Rotation undone: { previous_angle } (previous value: { current_value })| TO zcl_demo_abap_oodp_command=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Invoker classes
*&---------------------------------------------------------------------*

CLASS lcl_invoker DEFINITION.
  PUBLIC SECTION.
    METHODS:
      exec_cmd IMPORTING cmd TYPE REF TO lif_command,
      undo,
      redo.
  PRIVATE SECTION.
    DATA: undo_tab TYPE TABLE OF REF TO lif_command WITH EMPTY KEY,
          redo_tab TYPE TABLE OF REF TO lif_command WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_invoker IMPLEMENTATION.
  METHOD exec_cmd.
    APPEND `---` TO zcl_demo_abap_oodp_command=>log.
    IF cmd IS BOUND.
      cmd->execute( ).
      APPEND cmd TO undo_tab.
      CLEAR redo_tab.
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD undo.
    APPEND `---` TO zcl_demo_abap_oodp_command=>log.
    APPEND `Undo operation triggered` TO zcl_demo_abap_oodp_command=>log.
    DATA(tabix) = lines( undo_tab ).
    READ TABLE undo_tab INTO DATA(cmd) INDEX tabix.
    IF sy-subrc = 0.
      cmd->undo( ).
      DELETE undo_tab INDEX tabix.
      APPEND cmd TO redo_tab.
    ELSE.
      APPEND `Undo operation not possible` TO zcl_demo_abap_oodp_command=>log.
    ENDIF.
  ENDMETHOD.

  METHOD redo.
    APPEND `---` TO zcl_demo_abap_oodp_command=>log.
    APPEND `Redo operation triggered` TO zcl_demo_abap_oodp_command=>log.
    DATA(tabix) = lines( redo_tab ).
    READ TABLE redo_tab INTO DATA(cmd) INDEX tabix.
    IF sy-subrc = 0.
      cmd->execute( ).
      DELETE redo_tab INDEX tabix.
      APPEND cmd TO undo_tab.
    ELSE.
      APPEND `Redo operation not possible` TO zcl_demo_abap_oodp_command=>log.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
