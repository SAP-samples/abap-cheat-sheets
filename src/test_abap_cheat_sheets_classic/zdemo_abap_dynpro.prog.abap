*&---------------------------------------------------------------------*
*&                   ABAP cheat sheet: Dynpro
*&
*&-------------------------- PURPOSE ----------------------------------*
*& - Example to demonstrate dynpro-related statements
*& - Topics covered: Dynpro flow logic and related statements (MODULE,
*&   FIELD, CHAIN/ENDCHAIN, LOOP/ENDLOOP, CALL SUBSCREEN), ABAP statements
*&   for calling and leaving dynpros (SET SCREEN, CALL SCREEN, LEAVE
*&   SCREEN), modifying static attributes (LOOP AT SCREEN, MODIFY SCREEN),
*&   statements related to the GUI status and title (GET/SET PF-STATUS,
*&   SET TITLEBAR), controls (table and tabstrip controls)
*&
*&----------------------- GETTING STARTED -----------------------------*
*& - Open the program with the ABAP development tools for Eclipse (ADT).
*& - Choose F8 to run the program.
*& - Select the radio buttons and/or provide input in input fields etc.
*&   to check out dynpro-related syntax in action.
*&
*&----------------------------- NOTE -----------------------------------*
*& The code presented in this class is intended only to support the ABAP
*& cheat sheets. It is not intended for direct use in a production system
*& environment. The code examples in the ABAP cheat sheets are primarily
*& intended to provide a better explanation and visualization of the
*& syntax and semantics of ABAP statements, not to solve concrete
*& programming tasks. For production application programs, you should
*& always work out your own solution for each individual case. There is
*& no guarantee for the correctness or completeness of the code.
*& Furthermore, there is no legal responsibility or liability for any
*& errors or their consequences that may occur when using the the example
*& code.
*&---------------------------------------------------------------------*
REPORT zdemo_abap_dynpro.

DATA:
  "OK field
  ok_code   LIKE sy-ucomm,
  "Auxiliary variable for storing the value of the OK field
  save_ok   LIKE ok_code,
  dyn_num   LIKE sy-dynnr,
  dyn_title LIKE sy-title.

"Note: This demo database table does not have any semantic information
"that may be used for input helps etc.
TABLES zdemo_abap_flsch.

"----- Related to dynpro 9850 (Home page) -----
DATA: screen_elements TYPE abap_bool,
      stmts           TYPE abap_bool,
      stmts2          TYPE abap_bool,
      controls        TYPE abap_bool.

"----- Related to dynpro 9860 (Screen elements) -----

DATA: "Icons
  icon            TYPE icons-text,
  icon_name       TYPE c LENGTH 20,
  icon_text       TYPE c LENGTH 10,
  "check boxes
  ch_a            TYPE abap_bool,
  ch_b            TYPE abap_bool,
  ch_c            TYPE abap_bool,
  checkbox_msg    TYPE c LENGTH 4,
  input_field     TYPE c LENGTH 20,
  "radio buttons
  rb_x            TYPE abap_bool,
  rb_y            TYPE abap_bool,
  rb_z            TYPE abap_bool,
  radiobutton_msg TYPE c LENGTH 1.

"----- Related to dynpro 9870 (Statements I) -----

DATA: call_screen          TYPE abap_bool,
      call_screen_starting TYPE abap_bool,
      call_screen_st_end   TYPE abap_bool,
      set_screen           TYPE abap_bool,
      leave_screen_next    TYPE abap_bool,
      leave_screen_set     TYPE abap_bool,
      leave_to_screen      TYPE abap_bool,
      set_pfstatus         TYPE abap_bool,
      set_pf_excluding     TYPE abap_bool,
      get_pfstatus         TYPE abap_bool,
      get_pfstatsus_excl   TYPE abap_bool,
      set_titlebar         TYPE abap_bool,
      set_titlebar_with    TYPE abap_bool,
      with_title           TYPE c LENGTH 15,
      col1                 TYPE i,
      lin1                 TYPE i,
      col2                 TYPE i,
      lin2                 TYPE i,
      col3                 TYPE i,
      lin3                 TYPE i,
      pf                   TYPE c LENGTH 1,
      pfkey                TYPE syst_pfkey,
      title                TYPE c LENGTH 1.

"Data type and objects for the EXCLUDING addition of GET/SET PF-STATUS
"statements
TYPES fu_key_tab TYPE TABLE OF sy-ucomm WITH NON-UNIQUE KEY table_line.
DATA: excl_list1 TYPE fu_key_tab,
      excl_list2 TYPE fu_key_tab,
      excl_list3 TYPE fu_key_tab.

"----- Related to dynpro 9880 (Statements II) -----

DATA: loop_modify    TYPE abap_bool,
      field_stmt     TYPE abap_bool,
      mod_at_exit    TYPE abap_bool,
      selected       TYPE abap_bool,
      set            TYPE abap_bool,
      field_a        TYPE c LENGTH 1,
      field_b        TYPE c LENGTH 1,
      field_c        TYPE c LENGTH 1,
      field_d        TYPE c LENGTH 1,
      field_e        TYPE c LENGTH 1,
      field_f        TYPE c LENGTH 1,
      output_field_1 TYPE c LENGTH 100,
      output_field_2 TYPE c LENGTH 100,
      output_field_3 TYPE c LENGTH 100,
      field_help     TYPE c LENGTH 30,
      input_help     TYPE c LENGTH 30,
      "For GET/SET CURSOR statements
      fld            TYPE c LENGTH 20,
      off            TYPE i,
      val            TYPE string,
      len            TYPE i.

"For field and input help
TYPES: BEGIN OF values,
         carrid TYPE zdemo_abap_flsch-carrid,
         connid TYPE zdemo_abap_flsch-connid,
       END OF values.

DATA: progname      TYPE sy-repid,
      dynnum        TYPE sy-dynnr,
      dynpro_values TYPE TABLE OF dynpread,
      field_value   LIKE LINE OF dynpro_values,
      values_tab    TYPE TABLE OF values,
      tab           TYPE TABLE OF zdemo_abap_flsch-carrid,
      carr          TYPE  zdemo_abap_flsch-carrid,
      conn          TYPE  zdemo_abap_flsch-connid.

DATA itab_flsch TYPE TABLE OF zdemo_abap_flsch-carrid.

TYPES: BEGIN OF carrid_line,
         carrid TYPE zdemo_abap_flsch-carrid,
       END OF carrid_line.

DATA carrid_list TYPE STANDARD TABLE OF carrid_line.

"----- Related to dynpro 9890 (Exit command) -----

"For demonstration purposes, implementations for the
"related dialog modules are done in methods of a local
"class.

"----- Related to dynpro 9990 (Controls) -----

"For table controls
DATA lines TYPE i.
DATA fill  TYPE i.
CONTROLS flights TYPE TABLEVIEW USING SCREEN 9900.

"Internal table to hold data that is to be displayed.
DATA itab_flights TYPE TABLE OF zdemo_abap_flsch WITH EMPTY KEY.

"Filling the internal table
SELECT *
 FROM zdemo_abap_flsch
 ORDER BY carrid, connid
 INTO TABLE @itab_flights.

"For tabstrip controls
CONTROLS tabstr TYPE TABSTRIP.
DATA number LIKE sy-dynnr.

"For ALV Grid control
TYPES: BEGIN OF flight_struc,
         carrid    TYPE zdemo_abap_flsch-carrid,
         connid    TYPE zdemo_abap_flsch-connid,
         countryfr TYPE zdemo_abap_flsch-countryfr,
         cityfrom  TYPE zdemo_abap_flsch-cityfrom,
         airpfrom  TYPE zdemo_abap_flsch-airpfrom,
         countryto TYPE zdemo_abap_flsch-countryto,
         cityto    TYPE zdemo_abap_flsch-cityto,
         airpto    TYPE zdemo_abap_flsch-airpto,
         fltime    TYPE zdemo_abap_flsch-fltime,
       END OF flight_struc.

DATA itab4alv TYPE TABLE OF flight_struc WITH EMPTY KEY.

"----- For other dialog modules -----

DATA: links     TYPE TABLE OF tline,
      field_tab TYPE TABLE OF dfies WITH EMPTY KEY.

"----- Local class -----

CLASS local_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: provide_fu_keys IMPORTING example        TYPE i
                                   RETURNING VALUE(fu_keys) TYPE fu_key_tab,

      cc_alv_display IMPORTING container TYPE c,
      "Implementations for dialog modules for dynpro 9890
      pbo9890,
      pai9890,
      cancel.

    "Variable declarations for dialog modules for dynpro 9890
    CLASS-DATA: mandatory_field TYPE c LENGTH 20,
                btn_exit_1      TYPE abap_bool,
                btn_exit_2      TYPE abap_bool.
ENDCLASS.

CLASS local_class IMPLEMENTATION.

  METHOD provide_fu_keys.
    CASE example.
      WHEN 1.
        fu_keys = VALUE #( ( 'CANCEL' ) ( 'SAP' )  ).
      WHEN 2.
        fu_keys = VALUE #( ( 'CANCEL' ) ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
  METHOD cc_alv_display.
    DATA(cont) = NEW cl_gui_custom_container( container_name = container ).

    SELECT  carrid, connid, countryfr, cityfrom, airpfrom, countryto, cityto, airpto, fltime
      FROM zdemo_abap_flsch
      ORDER BY carrid, connid
      INTO CORRESPONDING FIELDS OF TABLE @itab4alv.

    TRY.
        cl_salv_table=>factory( EXPORTING r_container = cont
                                          container_name = CONV string( container )
                                IMPORTING r_salv_table = DATA(alv)
                                CHANGING t_table = itab4alv ).
        alv->get_columns( )->set_optimize( abap_true ).
        alv->get_display_settings( )->set_list_header( 'Flights' ).
        LOOP AT alv->get_columns( )->get( ) ASSIGNING FIELD-SYMBOL(<col>).
          DATA(column) = <col>-r_column.
          column->set_long_text( |{ column->get_columnname( ) }| ).
        ENDLOOP.
        alv->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
        MESSAGE `ALV display not possible` TYPE 'I'
                DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
  METHOD pai9890.
    save_ok = ok_code.
    CLEAR ok_code.

    CASE save_ok.
      WHEN 'CLOSE1'.
        MESSAGE `Processing continues. It does so only after the input field is filled.` TYPE 'I'.
        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN 'CLOSE2'.
        MESSAGE `Superfluouse message :)` TYPE 'I'.
      WHEN OTHERS.
        MESSAGE `Processing continues. It does so only after the input field is filled.` TYPE 'I'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.

  METHOD pbo9890.
    SET PF-STATUS 'STATUS9890'.
    SET TITLEBAR 'TITLE9890'.

    dyn_num = sy-dynnr.
  ENDMETHOD.

  METHOD cancel.
    MESSAGE `Module "cancel" was called. The dynpro will be left without the need to fill in the mandatory input field.` TYPE 'I'.

    IF ok_code = 'CLOSE2'.
      CLEAR: ok_code, mandatory_field.
      SET SCREEN 0.
      LEAVE SCREEN.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

END-OF-SELECTION.

  "Filling demo database tables
  zcl_demo_abap_aux=>fill_dbtabs( ).

  CALL SCREEN 9850.

**********************************************************************

***************** Dialog modules for 9850 ****************************

MODULE status_9850 OUTPUT.
  SET PF-STATUS 'STATUS9850'.
  SET TITLEBAR 'TITLE9850'.

  CLEAR: screen_elements, stmts, stmts2.
  dyn_num = sy-dynnr.
ENDMODULE.

MODULE user_command_9850 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'ENTER' OR 'GO'.
      CASE abap_true.
        WHEN screen_elements.
          CALL SCREEN 9860.
        WHEN stmts.
          CALL SCREEN 9870.
        WHEN stmts2.
          CALL SCREEN 9880.
        WHEN controls.
          CALL SCREEN 9900.
      ENDCASE.
    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

***************** Dialog modules for 9860 ****************************

MODULE status_9860 OUTPUT.
  SET PF-STATUS 'STATUS9860'.
  SET TITLEBAR 'TITLE9860'.

  "Modifying static attributes
  IF loop_modify = 'X'.
    LOOP AT SCREEN INTO DATA(scr).
      IF scr-name = 'ICON3'.
        scr-invisible    = '1'.
        MODIFY SCREEN FROM scr.
      ENDIF.
      IF scr-name = 'INPUT_FIELD'.
        scr-length = 10.
        scr-intensified = '1'.
        "scr-required = '1'.
        MODIFY SCREEN FROM scr.
      ENDIF.
      IF scr-name = 'RB_Z'.
        scr-active    = '0'.
        MODIFY SCREEN FROM scr.
      ENDIF.
      IF scr-name = 'CH_B'.
        scr-input    = '0'.
        MODIFY SCREEN FROM scr.
      ENDIF.
    ENDLOOP.
  ENDIF.

  dyn_num = sy-dynnr.
ENDMODULE.

MODULE user_command_9860 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
      "Icons
    WHEN 'ICON1'.
      icon_name = 'ICON_GREEN_LIGHT'.
      icon_text =  `green`.
    WHEN 'ICON2'.
      icon_name = 'ICON_YELLOW_LIGHT'.
      icon_text =  `yellow`.
    WHEN 'ICON3'.
      icon_name = 'ICON_RED_LIGHT'.
      icon_text =  `red`.
      "Input field
    WHEN 'BTN_NO'.
      MESSAGE |The text "{ input_field }" in the input field has { numofchar( input_field ) } characters. Trailing blanks are ignored.| TYPE 'I'.
      "Check boxes
    WHEN 'CH_BTN'.
      IF ch_a = abap_true.
        checkbox_msg = 'A'.
      ENDIF.
      IF ch_b = abap_true.
        checkbox_msg = checkbox_msg && 'B'.
      ENDIF.
      IF ch_c = abap_true.
        checkbox_msg = checkbox_msg && 'C'.
      ENDIF.
      IF checkbox_msg IS INITIAL.
        checkbox_msg = 'None'.
      ENDIF.
      checkbox_msg =  checkbox_msg.
      MESSAGE checkbox_msg TYPE 'I'.
      CLEAR checkbox_msg.
      "Radio buttons
    WHEN 'RB_BTN'.
      CASE abap_true.
        WHEN rb_x.
          radiobutton_msg = 'X'.
        WHEN rb_y.
          radiobutton_msg = 'Y'.
        WHEN rb_z.
          radiobutton_msg = 'Z'.
        WHEN OTHERS.
          radiobutton_msg = ' '.
      ENDCASE.
      MESSAGE radiobutton_msg TYPE 'I'.
      CLEAR radiobutton_msg.
    WHEN 'HOME'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.

  IF save_ok = 'ICON1' OR save_ok = 'ICON2' OR save_ok = 'ICON3'.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon_name
        text                  = icon_text
        info                  = 'Status'
        add_stdinf            = 'X'
      IMPORTING
        result                = icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      MESSAGE `Issue.` TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDMODULE.

***************** Dialog modules for 9870 ****************************

MODULE status_9870 OUTPUT.

  "Control structures in case of calling the dynpro again
  IF set_pfstatus IS NOT INITIAL AND pfkey = 'STATUS9870'.
    SET PF-STATUS 'DEMOSTATUS'.
  ELSEIF set_pfstatus IS NOT INITIAL AND pfkey = 'DEMOSTATUS'.
    SET PF-STATUS 'STATUS9870'.
  ELSEIF set_pf_excluding IS NOT INITIAL AND pfkey = 'STATUS9870'.
    excl_list1 = local_class=>provide_fu_keys( 1 ).
    SET PF-STATUS 'DEMOSTATUS' EXCLUDING excl_list1.
  ELSEIF set_pf_excluding IS NOT INITIAL AND pfkey = 'DEMOSTATUS'.
    excl_list2 = local_class=>provide_fu_keys( 2 ).
    SET PF-STATUS 'STATUS9870' EXCLUDING excl_list2.
  ELSE.
    SET PF-STATUS 'STATUS9870'.
  ENDIF.

  IF set_titlebar IS NOT INITIAL.
    IF dyn_title CS 'Demo title'.
      SET TITLEBAR 'TITLE9870'.
    ELSE.
      SET TITLEBAR 'DEMOTITLE'.
    ENDIF.
  ELSEIF SET_TITLEBAR_with IS NOT INITIAL.
    SET TITLEBAR 'DEMOTITLE' WITH '"' with_title '"'.
  ELSE.
    SET TITLEBAR 'TITLE9870'.
  ENDIF.

  "Prepopulating values
  col1 = 5.
  lin1 = 10.
  col2 = 1.
  lin2 = 1.
  col3 = 80.
  lin3 = 20.
  dyn_num = sy-dynnr.
  dyn_title = sy-title.
ENDMODULE.

MODULE user_command_9870 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'ENTER' OR 'GO'.
      CASE abap_true.
        WHEN set_screen.
          MESSAGE |A SET SCREEN statement is about to get called. The static next dynpro of dynpro { sy-dynnr } is 0.| &&
          ` It is dynamically overwritten. Another dynpro will be displayed. A new dynpro sequence is not started.` TYPE 'I'.
          SET SCREEN 9860.
        WHEN call_screen.
          MESSAGE `A CALL SCREEN statement is about to get called. You will switch to another dynpro. A new dynpro sequence is started.` TYPE 'I'.
          CALL SCREEN 9860.
        WHEN call_screen_starting.
          IF col1 < 0 OR lin1 < 0.
            MESSAGE `Only positive integers can be specified.` TYPE 'E'.
          ELSE.
            MESSAGE |The statement "CALL SCREEN ... STARTING AT { col1 } { lin1 }." is about to get called. | &&
            `Note: In the dialog box that opens, click the icon in the popup footer to remain in the program. Clicking the X leaves the program.` TYPE 'I'.
            CALL SCREEN 9860 STARTING AT col1 lin1.
          ENDIF.
        WHEN call_screen_st_end.
          IF col2 < 0 OR lin2 < 0
          OR col3 < 0 OR lin3 < 0.
            MESSAGE `Only positive integers can be specified.`
             TYPE 'E'.
          ELSE.
            MESSAGE |The statement "CALL SCREEN ... STARTING AT { col2 } { lin2 } ENDING AT { col3 } { lin3 }| &&
             `." is about to get called. Note: In the dialog box that opens, click the icon in the popup footer ` &&
             `to remain in the program. Clicking the X leaves the program.` TYPE 'I'.
            CALL SCREEN 9860 STARTING AT col2 lin2 ENDING AT col3 lin3.
          ENDIF.
        WHEN leave_screen_next.
          MESSAGE `A LEAVE SCREEN statement is about to get called. The current dynpro is exited, and the statically ` &&
          `defined next dynpro is called, which is 0 in this case. Hence, the current dynpro sequence is terminated. ` &&
          `Since the dynpro sequence is nested, you return to the home page and the parent dynpro sequence.` TYPE 'I'.
          LEAVE SCREEN.
        WHEN leave_screen_set.
          MESSAGE `A LEAVE SCREEN statement is about to get called. In this case, a SET SCREEN statement before the LEAVE ` &&
          `SCREEN statement overwrites the next dynpro (0 here). A new dynpro sequence is not started, instead the statement ` &&
          `branches to another dynpro in the same sequence.` TYPE 'I'.
          SET SCREEN 9900.
          LEAVE SCREEN.
        WHEN leave_to_screen.
          MESSAGE `A LEAVE TO SCREEN statement is about to get called. A new dynpro sequence is not started, instead the ` &&
          `statement branches to another dynpro in the same sequence. It has the same effect as the previous example.` TYPE 'I'.
          LEAVE TO SCREEN 9900.
        WHEN set_pfstatus.
          pfkey = sy-pfkey.
          MESSAGE |The current GUI status is { pfkey }. The current dynpro { sy-dynnr } will be set as the next dynpro.| &&
           ` It will have a different GUI status.` TYPE 'I'.
          SET SCREEN 9870.
        WHEN set_pf_excluding.
          GET PF-STATUS pfkey.
          MESSAGE |The current GUI status is { pfkey }. The current dynpro will be set as the next dynpro.| &&
          ` It will have a different GUI status (set at PBO) but excluding function codes (for example, the CANCEL button).` TYPE 'I'.
          SET SCREEN 9870.
        WHEN get_pfstatus.
          GET PF-STATUS pfkey.
          MESSAGE |The current GUI status is { pfkey }, which was retrieved using GET-PF-STATUS. Value of sy-pfkey: { sy-pfkey }.| TYPE 'I'.
          SET SCREEN 9870.
        WHEN get_pfstatsus_excl.
          GET PF-STATUS pfkey EXCLUDING excl_list3.
          MESSAGE |The current GUI status is { pfkey }. The exluded function codes are: {
            COND #( WHEN excl_list3 IS INITIAL THEN `None are excluded. Try to run SET PF-STATUS ... EXCLUDING ... first before this one.`
                                               ELSE concat_lines_of( table = excl_list3 sep = `, ` ) ) }| TYPE 'I'.
          SET SCREEN 9870.
          CLEAR excl_list3.
        WHEN set_titlebar.
          MESSAGE |The current title is "{ dyn_title }". The current dynpro is set as the next dynpro.| &&
          `The text in the title bar should change.` TYPE 'I'.
          SET SCREEN 9870.
        WHEN set_titlebar_with.
          MESSAGE |The current title is "{ dyn_title }". The current dynpro is set as the next dynpro.| &&
          `The text in the title bar should change according to your input in the input field.` TYPE 'I'.
          SET SCREEN 9870.
      ENDCASE.
    WHEN 'SAP'.
      cl_gui_frontend_services=>execute( document = 'https://www.sap.com' ).
      SET SCREEN 9870.
    WHEN 'SAYHI'.
      MESSAGE `Hallo ` && sy-uname && `.` TYPE 'I'.
      SET SCREEN 9870.
    WHEN 'HOME'.
      LEAVE TO SCREEN 9850.
    WHEN 'LEAVE' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

***************** Dialog modules for 9880 ****************************

MODULE status_9880 OUTPUT.
  SET PF-STATUS 'STATUS9880'.
  SET TITLEBAR 'TITLE9880'.

  "Clearing variables in case the dynpro is called again
  IF field_stmt IS INITIAL.
    CLEAR: output_field_1, output_field_2, output_field_3,
    field_a, field_b, field_c, field_d, field_e, field_f.
  ENDIF.
  IF field_d IS INITIAL.
    CLEAR output_field_2.
  ENDIF.
  IF field_e IS INITIAL OR field_f IS INITIAL.
    CLEAR output_field_3.
  ENDIF.

  "Prepopulating fields for input help
  progname = sy-repid.
  dynnum   = sy-dynnr.
  CLEAR: field_value, dynpro_values.
  field_value-fieldname = 'CARR'.
  APPEND field_value TO dynpro_values.

  "Setting the cursor field
  IF set = abap_true.
    SET CURSOR FIELD 'CONN'.
    CLEAR set.
  ENDIF.

  dyn_num = sy-dynnr.
ENDMODULE.

MODULE user_command_9880 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'ENTER' OR 'GO'.
      CASE abap_true.
        WHEN loop_modify.
          MESSAGE `Another dynpro will be set as the next dynpro. This dynpro includes a LOOP AT SCREEN ` &&
          ` and MODIFY SCREEN statements in the PBO dialog module. Some screen elements have their static ` &&
          `attributes changed. For example, they are made invisible or inactive, or the length is changed.` TYPE 'I'.
          SET SCREEN 9860.
          LEAVE SCREEN.
        WHEN field_stmt.
          MESSAGE `The current dynpro will be set as the next dynpro. Depending on which checkboxes you ` &&
          `have selected, text is displayed in the fields below showing the effect of FIELD statements.` TYPE 'I'.
          SET SCREEN 9880.
        WHEN mod_at_exit.
          SET SCREEN 9890.
      ENDCASE.
    WHEN 'SELECTED'.
      CLEAR conn.
      SET SCREEN 9880.
    WHEN 'GET'.
      GET CURSOR FIELD fld OFFSET off VALUE val LENGTH len.
      MESSAGE |Current position of cursor: Field: "{ fld }", value: "{ val }", offset: "{ off }", length "{ len }"|  TYPE 'I'.
      SET SCREEN 9880.
    WHEN 'SET'.
      set = abap_true.
      MESSAGE `Clicking the pushbutton triggers a SET CURSOR statement to be called at PBO. This sets the cursor on ` &&
      `a specific input field (the one in the lower left corner).`  TYPE 'I'.
      SET SCREEN 9880.
    WHEN 'HOME'.
      LEAVE TO SCREEN 9850.
    WHEN 'LEAVE' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

***************** Dialog modules for 9890 ****************************
"For demonstration purposes, the dialog module implementations are
"done in methods of a local class.

MODULE status_9890 OUTPUT.
  local_class=>pbo9890( ).
ENDMODULE.

MODULE user_command_9890 INPUT.
  local_class=>pai9890( ).
ENDMODULE.

***************** Dialog modules for 9900 ****************************

MODULE status_9900 OUTPUT.
  SET PF-STATUS 'STATUS9900'.
  SET TITLEBAR 'TITLE9900'.

  "For vertically scrolling in the table control
  "To do this, the LINES component of the scxtab_control structure is set
  "to the correct line number. Each time the scroll bar is scrolled, the
  "PAI event is triggered with an empty function code, and the top_line
  "component of the cxtab_control structure is automatically set to the
  "new top line before PBO.
  flights-lines = lines( itab_flights ).

  "Setting a tab to be active by default in case of calling the
  "dynpro again after leaving it.
  IF tabstr-activetab IS INITIAL
  OR number IS INITIAL.
    number = '9920'.
    tabstr-activetab = 'TAB2'.
  ENDIF.
  local_class=>cc_alv_display( 'ALVCONT' ).
  dyn_num = sy-dynnr.
ENDMODULE.

MODULE user_command_9900 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'TAB1'.
      number = '9910'.
      tabstr-activetab = save_ok.
    WHEN 'TAB2'.
      number = '9920'.
      tabstr-activetab = save_ok.
    WHEN 'HOME'.
      "REFRESH statement: You can check out the effect, for example, by setting
      "the vertical scroll bar somewhere. If you then go back and call the dynpro
      "again, the control is refreshed (the scroll bar is again at the top).
      REFRESH CONTROL 'FLIGHTS' FROM SCREEN sy-dynnr.
      CLEAR: number, tabstr-activetab.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'LEAVE'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

***************** Further dialog modules to support the example ****************************

MODULE module_1 INPUT.
  output_field_1 = 'Available in module_1:' && ` ` &&
                   COND #( WHEN field_a = abap_true THEN 'A' ELSE '' ) &&
                   COND #( WHEN field_b = abap_true THEN 'B' ELSE '' ) &&
                   COND #( WHEN field_c = abap_true THEN 'C' ELSE '' ) &&
                   ' /'.
ENDMODULE.

MODULE module_2 INPUT.
  output_field_1 = output_field_1 &&
                   ' Available in module_2:' && ` ` &&
                   COND #( WHEN field_a = abap_true THEN 'A' ELSE '' ) &&
                   COND #( WHEN field_b = abap_true THEN 'B' ELSE '' ) &&
                   COND #( WHEN field_c = abap_true THEN 'C' ELSE '' ) &&
                   ' /'.
ENDMODULE.

MODULE module_3 INPUT.
  output_field_1 = output_field_1 &&
                   ' Available in module_3:' && ` ` &&
                   COND #( WHEN field_a = abap_true THEN 'A' ELSE '' ) &&
                   COND #( WHEN field_b = abap_true THEN 'B' ELSE '' ) &&
                   COND #( WHEN field_c = abap_true THEN 'C' ELSE '' ).
  CLEAR: field_a, field_b, field_c.
ENDMODULE.

MODULE module_4 INPUT.
  output_field_2 = 'module_4 was called because field_d was selected.'.
ENDMODULE.

MODULE module_5 INPUT.
  output_field_3 = 'module_5 was called because field_e and field_f were selected.'.
ENDMODULE.

MODULE module_f1 INPUT.
  "In this example, a demo object from the ABAP Keyword
  "Documentation examples is used.
  CALL FUNCTION 'HELP_OBJECT_SHOW'
    EXPORTING
      dokclass = 'TX'
      doklangu = sy-langu
      dokname  = 'DEMO_FOR_F1_HELP'
    TABLES
      links    = links.
ENDMODULE.

MODULE module_f4 INPUT.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = progname
      dynumb             = dynnum
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynpro_values.

  field_value = dynpro_values[ 1 ].

  SELECT  carrid, connid
    FROM  zdemo_abap_flsch
    WHERE carrid = @( CONV #( field_value-fieldvalue ) )
    INTO  CORRESPONDING FIELDS OF TABLE @values_tab.

  "Note: To save extra artifacts, this example goes without a dedicated DDIC
  "structure with which input help can be created comfortably. Hence, creating
  "a table manually for the field_tab parameter of the function module below.
  field_tab = VALUE #(
    ( tabname = 'ZDEMO_ABAP_FLSCH' fieldname = 'CARRID' langu = 'E'  position = '1'
      offset = '0' leng = '3' intlen = '6'  outputlen = '3' datatype = 'CHAR' inttype = 'C' )
    ( tabname = 'ZDEMO_ABAP_FLSCH' fieldname = 'CONNID' langu = 'E'  position = '2'
      offset = '6' leng = '4' intlen = '8'  outputlen = '4'  datatype = 'NUMC' inttype = 'N' ) ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'CONNID'
      dynpprog     = progname
      dynpnr       = dynnum
      dynprofield  = 'CONN'
      value_org    = 'S'
      window_title = 'Selection'
    TABLES
      value_tab    = values_tab
      field_tab    = field_tab.
ENDMODULE.

MODULE module_dropdown INPUT.
  SELECT DISTINCT carrid
     FROM zdemo_abap_flsch
     INTO CORRESPONDING FIELDS OF TABLE @carrid_list.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CARRID'
      value_org       = 'S'
    TABLES
      value_tab       = carrid_list
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    carr = 'LH'.
  ENDIF.
ENDMODULE.

MODULE module_fill_table OUTPUT.
  TRY.
      zdemo_abap_flsch = itab_flights[ flights-current_line ].
    CATCH cx_sy_itab_line_not_found.
      RETURN.
  ENDTRY.
ENDMODULE.

MODULE module_read_table INPUT.
  lines = sy-loopc.
  MODIFY itab_flights FROM zdemo_abap_flsch INDEX flights-current_line.
ENDMODULE.

MODULE cancel INPUT.
  local_class=>cancel( ).
ENDMODULE.

