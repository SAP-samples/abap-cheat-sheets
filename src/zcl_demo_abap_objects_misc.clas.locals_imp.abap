CLASS local_class DEFINITION FRIENDS zcl_demo_abap_objects_misc.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA hello TYPE zcl_demo_abap_objects_misc=>str4friend.
    CLASS-METHODS say_hello.

ENDCLASS.

CLASS local_class IMPLEMENTATION.
  METHOD say_hello.
    hello = |{ zcl_demo_abap_objects_misc=>get_hello( ) } { sy-uname }.|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Interface methods declared with DEFAULT IGNORE and DEFAULT FAIL
*&---------------------------------------------------------------------*

INTERFACE lif_some_interface.

  TYPES c3 TYPE c LENGTH 3.
  DATA add_result TYPE i.
  CLASS-DATA: subtr_result TYPE i.
  METHODS addition IMPORTING num1 TYPE i
                             num2 TYPE i.
  CLASS-METHODS subtraction IMPORTING num1 TYPE i
                                      num2 TYPE i.

  METHODS meth_ignore DEFAULT IGNORE RETURNING VALUE(str) TYPE string.
  METHODS meth_fail DEFAULT FAIL RETURNING VALUE(str) TYPE string.

ENDINTERFACE.

CLASS lcl_without_impl DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_some_interface.
    ALIASES res FOR lif_some_interface~add_result.
    ALIASES add FOR lif_some_interface~addition.
    ALIASES subtr FOR lif_some_interface~subtraction.
    CLASS-METHODS run_cl IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_without_impl IMPLEMENTATION.

  METHOD run_cl.

    "Examples using an object reference variable
    DATA(oref) = NEW lcl_without_impl( ).

    oref->add( num1 = 1 num2 = 2 ).
    DATA(res1) = oref->res.
    out->write( res1 ).

    oref->subtr( num1 = 1 num2 = 2 ).
    DATA(res2) = oref->lif_some_interface~subtr_result.
    out->write( res2 ).

    "Referring to a type declared in the interface
    DATA char_a TYPE lif_some_interface~c3.
    DATA char_b TYPE lif_some_interface=>c3.

    "Calling non-implemented methods
    DATA(str_ig_a) = oref->lif_some_interface~meth_ignore( ).
    out->write( |"{ str_ig_a }"| ).

    TRY.
        DATA(str_fl_a) = oref->lif_some_interface~meth_fail( ).
        out->write( |"{ str_fl_a }"| ).
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

    "Similar examples using an interface reference variable
    DATA iref TYPE REF TO lif_some_interface.
    iref = NEW lcl_without_impl( ).

    iref->addition( num1 = 3 num2 = 5 ).
    DATA(res3) = iref->add_result.
    out->write( res3 ).

    iref->subtraction( num1 = 3 num2 = 5 ).
    DATA(res4) = iref->subtr_result.
    out->write( res4 ).

    "Referring to a type declared in the interface
    DATA char_c TYPE iref->c3.

    "Calling non-implemented methods
    DATA(str_ig_b) = iref->meth_ignore( ).
    out->write( |"{ str_ig_b }"| ).

    TRY.
        DATA(str_fl_b) = iref->meth_fail( ).
        out->write( |"{ str_fl_b }"| ).
      CATCH cx_sy_dyn_call_illegal_method INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD add.
    res = num1 + num2.
  ENDMETHOD.

  METHOD subtr.
    lif_some_interface~subtr_result = num1 - num2.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_with_impl DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS run_cl IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    INTERFACES lif_some_interface.
    ALIASES res FOR lif_some_interface~add_result.
    ALIASES add FOR lif_some_interface~addition.
    ALIASES subtr FOR lif_some_interface~subtraction.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_with_impl IMPLEMENTATION.

  METHOD run_cl.

    "Examples using an object reference variable
    DATA(oref) = NEW lcl_with_impl( ).

    oref->add( num1 = 1 num2 = 2 ).
    DATA(res1) = oref->res.
    out->write( res1 ).

    oref->subtr( num1 = 1 num2 = 2 ).
    DATA(res2) = oref->lif_some_interface~subtr_result.
    out->write( res2 ).

    "Referring to a type declared in the interface
    DATA char_a TYPE lif_some_interface~c3.
    DATA char_b TYPE lif_some_interface=>c3.

    "Calling non-implemented methods
    DATA(str_ig_a) = oref->lif_some_interface~meth_ignore( ).
    out->write( |"{ str_ig_a }"| ).

    TRY.
        DATA(str_fl_a) = oref->lif_some_interface~meth_fail( ).
        out->write( |"{ str_fl_a }"| ).
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

    "Similar examples using an interface reference variable
    DATA iref TYPE REF TO lif_some_interface.
    iref = NEW lcl_with_impl( ).

    iref->addition( num1 = 3 num2 = 5 ).
    DATA(res3) = iref->add_result.
    out->write( res3 ).

    iref->subtraction( num1 = 3 num2 = 5 ).
    DATA(res4) = iref->subtr_result.
    out->write( res4 ).

    "Referring to a type declared in the interface
    DATA char_c TYPE iref->c3.

    "Calling non-implemented methods
    DATA(str_ig_b) = iref->meth_ignore( ).
    out->write( |"{ str_ig_b }"| ).

    TRY.
        DATA(str_fl_b) = iref->meth_fail( ).
        out->write( |"{ str_fl_b }"| ).
      CATCH cx_sy_dyn_call_illegal_method INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD add.
    res = num1 + num2.
  ENDMETHOD.

  METHOD subtr.
    lif_some_interface~subtr_result = num1 - num2.
  ENDMETHOD.

  METHOD lif_some_interface~meth_fail.
    str = `meth_fail is implemented`.
  ENDMETHOD.

  METHOD lif_some_interface~meth_ignore.
    str = `meth_ignore is implemented`.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Events
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Example class 1
*&---------------------------------------------------------------------*

"- The class declares four instance events without parameters.
"- The same class also defines four event handler methods.
"- Additionally, a separate method implements the raising of the four events.
"- Based on the current time, a string (an instance attribute in the class) is
"  populated with a greeting appropriate for the time of day.

CLASS lcl_greetings DEFINITION.
  PUBLIC SECTION.
    "Attributes for display purposes
    DATA: greets TYPE string,
          time   TYPE t.

    "Event declarations
    EVENTS: morning,
      afternoon,
      evening,
      night.

    "Event handler methods
    METHODS: morning_greets FOR EVENT morning OF lcl_greetings,
      afternoon_greets FOR EVENT afternoon OF lcl_greetings,
      evening_greets FOR EVENT evening OF lcl_greetings,
      night_greets FOR EVENT night OF lcl_greetings.

    "Method that includes event raising in the implementation
    METHODS: greetings.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_greetings IMPLEMENTATION.
  METHOD greetings.
    time = cl_abap_context_info=>get_system_time( ).

    IF time BETWEEN '050001' AND '120000'.
      RAISE EVENT morning.
    ELSEIF time BETWEEN '120001' AND '170000'.
      RAISE EVENT afternoon.
    ELSEIF time BETWEEN '170001' AND '210000'.
      RAISE EVENT evening.
    ELSEIF time BETWEEN '210001' AND '050000'.
      RAISE EVENT night.
    ENDIF.
  ENDMETHOD.

  METHOD morning_greets.
    greets = |Good morning, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.

  METHOD afternoon_greets.
    greets = |Good afternoon, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.

  METHOD evening_greets.
    greets = |Good evening, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.

  METHOD night_greets.
    greets = |Good night, { sy-uname }. It's { time TIME = ENVIRONMENT }.|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Example class 2
*&---------------------------------------------------------------------*

"- The class lcl_1 declares instance and static events, some with and some without
"  exporting parameters.
"- The class also declares event handlers. The evt_handler_c method intentionally omits the num2
"  formal parameter, demonstrating that it is optional to specify all parameters. Additionally,
"  explicitly specifying the sender parameter is also optional.
"- The example implementations populate a string table for visualization and display purposes.
"- Note that for simplicity of the example, the instance method raise_event not only raises instance
"  events but also static events.
"- Based on the value of the importing parameter of the raise_event method, a specific event
"  is raised.

CLASS lcl_1 DEFINITION.
  PUBLIC SECTION.
    "Attributes for display purposes
    CLASS-DATA event_log_lcl_1 TYPE string_table.
    DATA some_text TYPE string.

    "Event declarations
    "Instance events
    EVENTS: inst_event_a,
      inst_event_b EXPORTING VALUE(num) TYPE i,
      inst_event_c EXPORTING VALUE(num1) TYPE i VALUE(num2) TYPE i VALUE(text) TYPE string.
    "Static events
    CLASS-EVENTS: stat_event_d,
      stat_event_e EXPORTING VALUE(num) TYPE i.

    "Event handler methods
    "Instance methods
    METHODS: evt_handler_a FOR EVENT inst_event_a OF lcl_1,
      evt_handler_b FOR EVENT inst_event_b OF lcl_1 IMPORTING num,
      evt_handler_c FOR EVENT inst_event_c OF lcl_1 IMPORTING num1 text sender.
    "Static events
    CLASS-METHODS: evt_handler_d FOR EVENT stat_event_d OF lcl_1,
      evt_handler_e FOR EVENT stat_event_e OF lcl_1 IMPORTING num.

    "Method that includes event raising in the implementation
    METHODS raise_event IMPORTING int TYPE i
                                  txt TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_1 IMPLEMENTATION.
  METHOD evt_handler_a.
    APPEND `evt_handler_a: Event handled` TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_b.
    APPEND `evt_handler_b: Event handled` TO event_log_lcl_1.
    APPEND |Value of num passed: { num }| TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_c.
    APPEND `evt_handler_c: Event handled` TO event_log_lcl_1.
    APPEND |Value of num1 passed: { num1 }| TO event_log_lcl_1.
    APPEND |Value of text passed: { text }| TO event_log_lcl_1.
    DATA(sender_cl_name) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( sender ) )->get_relative_name( ).
    APPEND |Accessing sender reference variable; class name: { sender_cl_name }| TO event_log_lcl_1.
    APPEND |Value of instance attribute, accessed via sender: { sender->some_text }| TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_d.
    APPEND `evt_handler_d: Event handled` TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD evt_handler_e.
    APPEND `evt_handler_e: Event handled` TO event_log_lcl_1.
    APPEND |Value of num passed: { num }| TO event_log_lcl_1.
    APPEND INITIAL LINE TO event_log_lcl_1.
  ENDMETHOD.

  METHOD raise_event.
    CASE int.
      WHEN 1.
        RAISE EVENT inst_event_a.
      WHEN 2.
        RAISE EVENT inst_event_b EXPORTING num = int.
      WHEN 3.
        RAISE EVENT inst_event_c EXPORTING num1 = int num2 = int text = txt.
      WHEN 4.
        RAISE EVENT stat_event_d.
      WHEN 5.
        RAISE EVENT stat_event_e EXPORTING num = int.
      WHEN OTHERS.
        RAISE EVENT stat_event_d.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Example class 3
*&---------------------------------------------------------------------*

"This example class is setup similarly to lcl_1.

CLASS lcl_2 DEFINITION.
  PUBLIC SECTION.
    "Attribute for display purposes
    CLASS-DATA event_log_lcl_2 TYPE string_table.

    "Event declarations
    EVENTS: inst_event_f,
            inst_event_g.
    CLASS-EVENTS stat_event_h.

    "Event handler methods
    METHODS: evt_handler_f FOR EVENT inst_event_f OF lcl_2,
           evt_handler_g FOR EVENT inst_event_g OF lcl_2.
    CLASS-METHODS: evt_handler_h FOR EVENT stat_event_h OF lcl_2.

    "Method that includes event raising in the implementation
    METHODS raise_event IMPORTING num TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_2 IMPLEMENTATION.
  METHOD evt_handler_f.
    APPEND `evt_handler_f: Event handled` TO event_log_lcl_2.
    APPEND INITIAL LINE TO event_log_lcl_2.
  ENDMETHOD.

  METHOD evt_handler_g.
    APPEND `evt_handler_g: Event handled` TO event_log_lcl_2.
    APPEND INITIAL LINE TO event_log_lcl_2.
  ENDMETHOD.

  METHOD evt_handler_h.
    APPEND `evt_handler_h: Event handled` TO event_log_lcl_2.
    APPEND INITIAL LINE TO event_log_lcl_2.
  ENDMETHOD.

  METHOD raise_event.
    CASE num.
      WHEN 1.
        RAISE EVENT inst_event_f.
      WHEN 2.
        RAISE EVENT inst_event_g.
      WHEN OTHERS.
        RAISE EVENT stat_event_h.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Example classes 4 - 7
*&---------------------------------------------------------------------*

    "The example classes 4 - 6 represent an inheritance tree:
    "LCL_A
    "  |
    "  |--LCL_B
    "  |   |
    "  |   |--LCL_C
    "
    "Example class 7, LCL_D, is a class that implements event handlers.

    "Example notes:
    "- Static and instance events are declared in the superclass lcl_a.
    "- The superclass and its subclasses contain a static and an instance
    "  method that raise the events.
    "- The class lcl_d declares event handlers for the events in lcl_b, which
    "  are inherited from the superclass lcl_a.
    "- In the global class, different options are used to access methods.
    "- A log table visualizes the method call flow. Additionally, a flag is
    "  populated illustrating whether the event is handled.

CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: log_tab TYPE string_table,
                is_handled TYPE abap_boolean.
    CLASS-EVENTS stat_event.
    CLASS-METHODS stat_meth_1.
    EVENTS  inst_event.
    METHODS inst_meth_1.
ENDCLASS.

CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    CLASS-METHODS stat_meth_2.
    METHODS inst_meth_2.
ENDCLASS.

CLASS lcl_c DEFINITION INHERITING FROM lcl_b.
  PUBLIC SECTION.
    CLASS-METHODS stat_meth_3.
    METHODS inst_meth_3.
ENDCLASS.

CLASS lcl_d DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS stat_evt_handler FOR EVENT stat_event OF lcl_b.
    METHODS inst_evt_handler FOR EVENT inst_event OF lcl_b.
ENDCLASS.

CLASS lcl_a IMPLEMENTATION.
  METHOD stat_meth_1.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_a/stat_meth_1 called` TO lcl_a=>log_tab.
    RAISE EVENT stat_event.
  ENDMETHOD.
  METHOD inst_meth_1.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_a/inst_meth_1 called` TO lcl_a=>log_tab.
    RAISE EVENT inst_event.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_b IMPLEMENTATION.
  METHOD stat_meth_2.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_b/stat_meth_2 called` TO lcl_a=>log_tab.
    RAISE EVENT stat_event.
  ENDMETHOD.
  METHOD inst_meth_2.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_b/inst_event called` TO lcl_a=>log_tab.
    RAISE EVENT inst_event.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_c IMPLEMENTATION.
  METHOD stat_meth_3.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_c/stat_meth_3 called` TO lcl_a=>log_tab.
    RAISE EVENT stat_event.
  ENDMETHOD.
  METHOD inst_meth_3.
    CLEAR lcl_a=>is_handled.
    APPEND `lcl_c/inst_meth_3 called` TO lcl_a=>log_tab.
    RAISE EVENT inst_event.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_d IMPLEMENTATION.
  METHOD stat_evt_handler.
    lcl_a=>is_handled = abap_true.
    APPEND `lcl_d/stat_evt_handler called` TO lcl_a=>log_tab.
    APPEND `--- Static event handled ---` TO lcl_a=>log_tab.
  ENDMETHOD.
  METHOD inst_evt_handler.
    lcl_a=>is_handled = abap_true.
    APPEND `lcl_d/inst_evt_handler called` TO lcl_a=>log_tab.
    APPEND `--- Instance event handled ---` TO lcl_a=>log_tab.
  ENDMETHOD.
ENDCLASS.
