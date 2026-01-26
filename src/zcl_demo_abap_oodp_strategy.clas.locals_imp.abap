*&---------------------------------------------------------------------*
*& 1) Example setup with an interface
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Interface
*&---------------------------------------------------------------------*

INTERFACE lif_calculation.
  METHODS calculate
    IMPORTING num1          TYPE i
              num2          TYPE i
    RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Concrete strategy classes
*&---------------------------------------------------------------------*

CLASS lcl_addition DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_addition IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    result = num1 + num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_subtraction DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_subtraction IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    result = num1 - num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_multiplication DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_multiplication IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    result = num1 * num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_division DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_calculation.
ENDCLASS.

CLASS lcl_division IMPLEMENTATION.
  METHOD lif_calculation~calculate.
    IF num1 <> 0 AND num2 = 0.
      result = 0.
    ELSE.
      result = num1 / num2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Base class that delegates the execution of functionality
*&---------------------------------------------------------------------*

CLASS lcl_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_strategy_ref IMPORTING strat_ref TYPE REF TO lif_calculation,
      calculate IMPORTING num1          TYPE i
                          num2          TYPE i
                RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA strategy TYPE REF TO lif_calculation.
ENDCLASS.

CLASS lcl_calculator IMPLEMENTATION.
  METHOD set_strategy_ref.
    me->strategy = strat_ref.
  ENDMETHOD.

  METHOD calculate.
    IF strategy IS BOUND.
      result = strategy->calculate( num1 = num1 num2 = num2 ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************

*&---------------------------------------------------------------------*
*& 2) Example setup with an abstract class
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Abstract class
*&---------------------------------------------------------------------*

CLASS lcl_strategy_abstract DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS calculate ABSTRACT
      IMPORTING
        num1          TYPE i
        num2          TYPE i
      RETURNING
        VALUE(result) TYPE i.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete strategy classes
*&---------------------------------------------------------------------*

CLASS lcl_add DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_add IMPLEMENTATION.
  METHOD calculate.
    result = num1 + num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sub DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_sub IMPLEMENTATION.
  METHOD calculate.
    result = num1 - num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mult DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_mult IMPLEMENTATION.
  METHOD calculate.
    result = num1 * num2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_div DEFINITION INHERITING FROM lcl_strategy_abstract FINAL.
  PUBLIC SECTION.
    METHODS calculate REDEFINITION.
ENDCLASS.

CLASS lcl_div IMPLEMENTATION.
  METHOD calculate.
    IF num1 <> 0 AND num2 = 0.
      result = 0.
    ELSE.
      result = num1 / num2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Base class that delegates the execution of functionality
*&---------------------------------------------------------------------*

CLASS lcl_calc DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_strategy_ref IMPORTING strat_ref TYPE REF TO lcl_strategy_abstract,
      calculate IMPORTING num1          TYPE i
                          num2          TYPE i
                RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    DATA strategy TYPE REF TO lcl_strategy_abstract.
ENDCLASS.

CLASS lcl_calc IMPLEMENTATION.
  METHOD set_strategy_ref.
    strategy = strat_ref.
  ENDMETHOD.

  METHOD calculate.
    IF strategy IS BOUND.
      result = strategy->calculate( num1 = num1 num2 = num2 ).
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
