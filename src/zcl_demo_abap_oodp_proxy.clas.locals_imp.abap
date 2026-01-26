*&---------------------------------------------------------------------*
*& Interface serving as the common interface for both the original
*& and proxy classes
*&---------------------------------------------------------------------*

INTERFACE lif_user_info.
  METHODS get_user_info IMPORTING user_id     TYPE string
                        RETURNING VALUE(info) TYPE string.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Local exception class for errors due to failed authorization
*& validations
*&---------------------------------------------------------------------*

CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.

    DATA text TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        text     TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->text = text.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Original class
*&---------------------------------------------------------------------*

"- Contains the business logic for retrieving user information
"- Declared with CREATE PRIVATE to prevent external object creation. Only the
"  class itself and its friends can instantiate objects, which is why the proxy
"  class is declared as a friend of the class. Since the proxy class is not
"  known yet in the CCIMP include, the class declaration is deferred.

CLASS lcl_proxy DEFINITION DEFERRED.

CLASS lcl_user_info DEFINITION CREATE PRIVATE FRIENDS lcl_proxy.
  PUBLIC SECTION.
    INTERFACES lif_user_info.
  PRIVATE SECTION.
    "For demo purposes, adding hardcoded values for a user authorized
    "to retrieve the data.
    CONSTANTS: user_name TYPE string VALUE `power_user`,
               pw        TYPE string VALUE `abc123`.
ENDCLASS.

CLASS lcl_user_info IMPLEMENTATION.
  METHOD lif_user_info~get_user_info.

    "Here goes an operation to get user information.
*    SELECT user_id, info_a, info_b
*     FROM some_data_source
*     WHERE user_id = @user_id
*     INTO ...

    info = |Information for user { user_id }: Some information ...|.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Proxy class
*&---------------------------------------------------------------------*

"- Implements the interface to serve as a surrogate of the original class.
"  So, users interact with this class instead of the the original class.
"- Maintains a reference to the original class and delegates requests to it.
"  The business logic can only be executed, and data can only be retrieved
"  if authentication validation succeeds. If validation fails, the an object
"  of the original class cannot be created, and exception is raised.
"- Extra functionality is added to the proxy class by implementing a
"  simplified logging mechanism, realized by an internal table in the example.
CLASS lcl_proxy DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_user_info.
    METHODS constructor IMPORTING username TYPE string
                                  password TYPE string
                        RAISING   lcx_error.

    TYPES: BEGIN OF ty_log,
             user           TYPE string,
             time_stamp     TYPE utclong,
             access_granted TYPE abap_boolean,
           END OF ty_log,
           tt_log TYPE TABLE OF ty_log WITH EMPTY KEY.

    CLASS-METHODS: add_log_entry IMPORTING user_name      TYPE string
                                           time_stamp     TYPE utclong
                                           access_granted TYPE abap_boolean,
                   get_log RETURNING VALUE(log) TYPE tt_log.

  PRIVATE SECTION.

    DATA oref TYPE REF TO lcl_user_info..
    CLASS-DATA log_tab TYPE tt_log.
ENDCLASS.

CLASS lcl_proxy IMPLEMENTATION.
  METHOD constructor.

    "Here goes a proper authorization check ...
    "The simplified example includes a dummy authorization check.
    IF username = lcl_user_info=>user_name AND password = lcl_user_info=>pw.
      "Adding a log entry
      add_log_entry( user_name = username time_stamp = utclong_current( ) access_granted = abap_true ).

      "Instantiating the original class
      oref = NEW #( ).
    ELSE.
      "Add log entry
      add_log_entry( user_name = username time_stamp = utclong_current( ) access_granted = abap_false ).

      "Dummy authentication failed
      RAISE EXCEPTION TYPE lcx_error.
    ENDIF.

  ENDMETHOD.

  METHOD lif_user_info~get_user_info.
    "Calling a method of the original class via the proxy class
    "The calling is only possible if the authorization validation was successful
    "and an instance was created.
    TRY.
        info = oref->lif_user_info~get_user_info( user_id ).
      CATCH cx_sy_ref_is_initial.
        info = `No authorization`.
    ENDTRY.
  ENDMETHOD.

  METHOD add_log_entry.
    APPEND VALUE #( user = user_name time_stamp = time_stamp access_granted = access_granted ) TO log_tab.
  ENDMETHOD.

  METHOD get_log.
    log = log_tab.
  ENDMETHOD.
ENDCLASS.
