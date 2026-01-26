*&---------------------------------------------------------------------*
*& Example 1
*&---------------------------------------------------------------------*

INTERFACE lif_string_processing.
  DATA str TYPE string READ-ONLY.
  METHODS add IMPORTING string     TYPE clike
              RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS precede IMPORTING string     TYPE clike
                  RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS replace_all IMPORTING sub        TYPE clike
                                with       TYPE clike
                      RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS replace_occ IMPORTING sub        TYPE clike
                                with       TYPE clike
                                occ        TYPE i DEFAULT 1
                      RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS lowercase RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS uppercase RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS remove_leading_trailing_spaces RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS remove_all_spaces RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS reverse_string RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS insert_string IMPORTING string     TYPE clike
                                  off        TYPE i
                        RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  METHODS get_string RETURNING VALUE(str) TYPE string.
  METHODS split_into_table IMPORTING split_at   TYPE clike
                           RETURNING VALUE(tab) TYPE string_table.
ENDINTERFACE.

CLASS lcl_string DEFINITION DEFERRED.
CLASS lcl_string_processing DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_string.
  PUBLIC SECTION.
    INTERFACES lif_string_processing.
    ALIASES: add FOR lif_string_processing~add,
             get_string FOR lif_string_processing~get_string,
             insert_string FOR lif_string_processing~insert_string,
             precede FOR lif_string_processing~precede,
             remove_all_spaces FOR lif_string_processing~remove_all_spaces,
             remove_leading_trailing_spaces FOR lif_string_processing~remove_leading_trailing_spaces,
             replace_all FOR lif_string_processing~replace_all,
             replace_occ FOR lif_string_processing~replace_occ,
             reverse_string FOR lif_string_processing~reverse_string,
             split_into_table FOR lif_string_processing~split_into_table,
             lowercase FOR lif_string_processing~lowercase,
             uppercase FOR lif_string_processing~uppercase.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES string_content FOR lif_string_processing~str.
    METHODS constructor IMPORTING content TYPE string.
    DATA oref TYPE REF TO lcl_string_processing.
ENDCLASS.

CLASS lcl_string_processing IMPLEMENTATION.
  METHOD add.
    oref->string_content &&= string.
    ref = oref.
  ENDMETHOD.

  METHOD get_string.
    str = oref->string_content.
  ENDMETHOD.

  METHOD insert_string.
    TRY.
        oref->string_content = insert( val = oref->string_content sub = string off = off ).
      CATCH cx_sy_range_out_of_bounds.
    ENDTRY.
    ref = oref.
  ENDMETHOD.

  METHOD precede.
    oref->string_content = string && oref->string_content.
    ref = oref.
  ENDMETHOD.

  METHOD remove_all_spaces.
    oref->string_content = condense( val = oref->string_content to = `` ).
    ref = oref.
  ENDMETHOD.

  METHOD remove_leading_trailing_spaces.
    oref->string_content = condense( val = oref->string_content from = `` ).
    ref = oref.
  ENDMETHOD.

  METHOD replace_all.
    oref->string_content = replace( val = oref->string_content sub = sub with = with  occ = 0 ).
    ref = oref.
  ENDMETHOD.

  METHOD replace_occ.
    oref->string_content = replace( val = oref->string_content sub = sub with = with occ = occ ).
    ref = oref.
  ENDMETHOD.

  METHOD reverse_string.
    oref->string_content = reverse( oref->string_content ).
    ref = oref.
  ENDMETHOD.

  METHOD split_into_table.
    SPLIT oref->string_content AT split_at INTO TABLE tab.
  ENDMETHOD.

  METHOD lowercase.
    oref->string_content = to_lower( oref->string_content ).
    ref = oref.
  ENDMETHOD.

  METHOD uppercase.
    oref->string_content = to_upper( oref->string_content ).
    ref = oref.
  ENDMETHOD.

  METHOD constructor.
    string_content = content.
    oref = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_string DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_string_processing.
  PUBLIC SECTION.
    CLASS-METHODS string IMPORTING string     TYPE clike
                         RETURNING VALUE(ref) TYPE REF TO lif_string_processing.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_string IMPLEMENTATION.
  METHOD string.
    ref = NEW lcl_string_processing( string ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

*&---------------------------------------------------------------------*
*& Example 2
*&---------------------------------------------------------------------*

CLASS lcl_calc DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING num TYPE decfloat34.
    METHODS plus IMPORTING num        TYPE decfloat34
                 RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS minus IMPORTING num        TYPE decfloat34
                  RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS multiply IMPORTING num        TYPE decfloat34
                     RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS divide IMPORTING num        TYPE decfloat34
                   RETURNING VALUE(ref) TYPE REF TO lcl_calc.
    METHODS get_result RETURNING VALUE(result) TYPE decfloat34.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA number TYPE decfloat34.
ENDCLASS.

CLASS lcl_calc IMPLEMENTATION.
  METHOD constructor.
    number = num.
  ENDMETHOD.

  METHOD divide.
    TRY.
        number /= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD minus.
    TRY.
        number -= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD multiply.
    TRY.
        number *= num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD plus.
    TRY.
        number += num.
      CATCH cx_sy_arithmetic_error.
    ENDTRY.
    ref = me.
  ENDMETHOD.

  METHOD get_result.
    result = number.
  ENDMETHOD.
ENDCLASS.
