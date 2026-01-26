*&---------------------------------------------------------------------*
*& State interface
*&---------------------------------------------------------------------*

INTERFACE lif_calc.
  DATA number TYPE i.
  METHODS calculate IMPORTING value         TYPE i
                    RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Concrete state classes
*&---------------------------------------------------------------------*

CLASS lcl_state_addition DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calc.
ENDCLASS.

CLASS lcl_state_addition IMPLEMENTATION.
  METHOD lif_calc~calculate.
    result = lif_calc~number + value.
    lif_calc~number = result.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_state_subtraction DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calc.
ENDCLASS.

CLASS lcl_state_subtraction IMPLEMENTATION.
  METHOD lif_calc~calculate.
    result = lif_calc~number - value.
    lif_calc~number = result.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_state_multiplication DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calc.
ENDCLASS.

CLASS lcl_state_multiplication IMPLEMENTATION.
  METHOD lif_calc~calculate.
    result = lif_calc~number * value.
    lif_calc~number = result.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class whose objects' behavior changes (Example 1)
*&---------------------------------------------------------------------*

CLASS lcl_math DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING start_num TYPE i,
      set_state IMPORTING state TYPE REF TO lif_calc,
      execute IMPORTING value TYPE i RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA: num       TYPE i,
          state_ref TYPE REF TO lif_calc.
ENDCLASS.

CLASS lcl_math IMPLEMENTATION.
  METHOD constructor.
    num = start_num.
  ENDMETHOD.

  METHOD set_state.
    num = COND #( WHEN state_ref IS BOUND THEN state_ref->number ELSE num ).
    state_ref = state.
    state_ref->number = num.
  ENDMETHOD.

  METHOD execute.
    IF state_ref IS BOUND.
      result = state_ref->calculate( value ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class whose objects' behavior changes (Example 2)
*&---------------------------------------------------------------------*

CLASS lcl_math_cond DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM calc_operation,
             plus,
             minus,
             multiply,
           END OF ENUM calc_operation.
    METHODS:
      constructor IMPORTING start_num TYPE i,
      set_state IMPORTING calc_operation TYPE lcl_math_cond=>calc_operation,
      execute IMPORTING value TYPE i RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA num TYPE i.
    DATA state_ref TYPE REF TO lif_calc.
ENDCLASS.

CLASS lcl_math_cond IMPLEMENTATION.
  METHOD constructor.
    num = start_num.
  ENDMETHOD.

  METHOD set_state.
    num = COND #( WHEN state_ref IS BOUND THEN state_ref->number ELSE num ).

    state_ref = COND #( WHEN calc_operation = plus AND state_ref IS NOT INSTANCE OF lcl_state_addition THEN NEW lcl_state_addition( )
                        WHEN calc_operation = minus AND state_ref IS NOT INSTANCE OF lcl_state_subtraction THEN NEW lcl_state_subtraction( )
                        WHEN calc_operation = multiply AND state_ref IS NOT INSTANCE OF lcl_state_multiplication THEN NEW lcl_state_multiplication( ) ).

    state_ref->number = num.
  ENDMETHOD.

  METHOD execute.
    IF state_ref IS BOUND.
      result = state_ref->calculate( value ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
