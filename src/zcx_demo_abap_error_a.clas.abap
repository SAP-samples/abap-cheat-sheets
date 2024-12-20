CLASS zcx_demo_abap_error_a DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message.

    CONSTANTS message_class TYPE symsgid VALUE 'ZDEMO_ABAP_MESSAGES'.

    CONSTANTS:
      BEGIN OF zcx_demo_abap_error_a,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_demo_abap_error_a.

    CONSTANTS:
      BEGIN OF error_002,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_002.

    CONSTANTS:
      BEGIN OF error_003,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'P_003_A',
        attr2 TYPE scx_attrname VALUE 'P_003_B',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_003.

    CONSTANTS:
      BEGIN OF error_004,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'P_004_A',
        attr2 TYPE scx_attrname VALUE 'P_004_B',
        attr3 TYPE scx_attrname VALUE 'P_004_C',
        attr4 TYPE scx_attrname VALUE 'P_004_D',
      END OF error_004.

    CONSTANTS:
      BEGIN OF error_005,
        msgid TYPE symsgid VALUE message_class,
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'P_005_A',
        attr2 TYPE scx_attrname VALUE 'P_005_B',
        attr3 TYPE scx_attrname VALUE 'P_005_C',
        attr4 TYPE scx_attrname VALUE 'P_005_D',
      END OF error_005.

    DATA p_003_a TYPE string.
    DATA p_003_b TYPE string.
    DATA p_004_a TYPE string.
    DATA p_004_b TYPE string.
    DATA p_004_c TYPE string.
    DATA p_004_d TYPE string.
    DATA p_005_a TYPE string.
    DATA p_005_b TYPE string.
    DATA p_005_c TYPE string.
    DATA p_005_d TYPE string.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        p_003_a  TYPE string OPTIONAL
        p_003_b  TYPE string OPTIONAL
        p_004_a  TYPE string OPTIONAL
        p_004_b  TYPE string OPTIONAL
        p_004_c  TYPE string OPTIONAL
        p_004_d  TYPE string OPTIONAL
        p_005_a  TYPE string OPTIONAL
        p_005_b  TYPE string OPTIONAL
        p_005_c  TYPE string OPTIONAL
        p_005_d  TYPE string OPTIONAL
      .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_demo_abap_error_a IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->p_003_a = p_003_a.
    me->p_003_b = p_003_b.
    me->p_004_a = p_004_a.
    me->p_004_b = p_004_b.
    me->p_004_c = p_004_c.
    me->p_004_d = p_004_d.
    me->p_005_a = p_005_a.
    me->p_005_b = p_005_b.
    me->p_005_c = p_005_c.
    me->p_005_d = p_005_d.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
