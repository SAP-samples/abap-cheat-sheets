CLASS zcl_demo_abap_abp_aux_cl96 DEFINITION
 PUBLIC
  FOR BEHAVIOR OF zr_demo_abap96 .

  PUBLIC SECTION.
    TYPES log TYPE TABLE OF zdemoabaplog96 WITH EMPTY KEY.
    TYPES det_keys TYPE TABLE FOR DETERMINATION zr_demo_abap96\\demo_abap~det_save.
    TYPES read_result TYPE TABLE FOR READ RESULT zr_demo_abap96\\demo_abap.

    TYPES: basetype TYPE land1,
           BEGIN OF ENUM t_enum_base BASE TYPE basetype,
             none VALUE IS INITIAL,
             at   VALUE 'AT',
             ch   VALUE 'CH',
             de   VALUE 'DE',
             es   VALUE 'ES',
             fr   VALUE 'FR',
             it   VALUE 'IT',
             pt   VALUE 'PT',
             ro   VALUE 'RO',
             se   VALUE 'SE',
             sk   VALUE 'SK',
           END OF ENUM t_enum_base.

    CLASS-METHODS get_db_entries
      EXPORTING count      TYPE i
                db_entries TYPE log.

    CLASS-METHODS read_instances
      IMPORTING keys               TYPE det_keys
      RETURNING VALUE(read_result) TYPE read_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_abp_aux_cl96 IMPLEMENTATION.
  METHOD get_db_entries.
    SELECT COUNT( * ) FROM zdemoabaplog96 INTO @count.
    SELECT * FROM zdemoabaplog96 ORDER BY timestamp DESCENDING INTO TABLE @db_entries.
  ENDMETHOD.

  METHOD read_instances.
    READ ENTITIES OF zr_demo_abap96 IN LOCAL MODE
      ENTITY demo_abap
      FIELDS ( num1 ) WITH CORRESPONDING #( keys )
      RESULT read_result.
  ENDMETHOD.
ENDCLASS.
