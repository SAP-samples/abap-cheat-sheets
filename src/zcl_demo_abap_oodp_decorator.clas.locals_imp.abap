**********************************************************************
"Interface

INTERFACE lif_pizza.
  METHODS create_pizza RETURNING VALUE(ingredients) TYPE string_table.
  METHODS get_costs RETURNING VALUE(costs) TYPE i.
ENDINTERFACE.

**********************************************************************
"Basic pizza to be decorated

CLASS lcl_basic_pizza DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pizza.
    METHODS constructor IMPORTING basic_pizza_costs TYPE i.
  PRIVATE SECTION.
    DATA pizza_costs TYPE i.
ENDCLASS.

CLASS lcl_basic_pizza IMPLEMENTATION.
  METHOD constructor.
    pizza_costs = basic_pizza_costs.
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = VALUE #( ( `dough` ) ( `tomato sauce` ) ( `cheese` ) ).
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = pizza_costs.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Abstract decorator class

CLASS lcl_pizza_decorator DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_pizza.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
  PROTECTED SECTION.
    DATA decorated_pizza TYPE REF TO lif_pizza.
ENDCLASS.

CLASS lcl_pizza_decorator IMPLEMENTATION.
  METHOD constructor.
    decorated_pizza = pizza.
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = decorated_pizza->create_pizza( ).
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = decorated_pizza->get_costs( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
"Concrete decorator classes

CLASS lcl_decorator_salami DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_salami IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    APPEND `salami` TO ingredients.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_decorator_mushrooms DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_mushrooms IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    APPEND `mushrooms` TO ingredients.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_decorator_vegetables DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_vegetables IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    APPEND LINES OF VALUE string_table( ( `red pepper` ) ( `zucchini` ) ( `broccoli` ) ) TO ingredients.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_decorator_allergy_friendly DEFINITION INHERITING FROM lcl_pizza_decorator.
  PUBLIC SECTION.
    METHODS constructor IMPORTING pizza TYPE REF TO lif_pizza.
    METHODS lif_pizza~create_pizza REDEFINITION.
    METHODS lif_pizza~get_costs REDEFINITION.
ENDCLASS.

CLASS lcl_decorator_allergy_friendly IMPLEMENTATION.
  METHOD constructor.
    super->constructor( pizza ).
  ENDMETHOD.

  METHOD lif_pizza~create_pizza.
    ingredients = super->lif_pizza~create_pizza( ).
    REPLACE ALL OCCURRENCES OF `dough` IN TABLE ingredients WITH `gluten-free dough`.
    REPLACE ALL OCCURRENCES OF `cheese` IN TABLE ingredients WITH `lactose-free cheese`.
  ENDMETHOD.

  METHOD lif_pizza~get_costs.
    costs = super->lif_pizza~get_costs( ) + 1.
  ENDMETHOD.
ENDCLASS.
