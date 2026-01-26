*&---------------------------------------------------------------------*
*& Abstract products
*&---------------------------------------------------------------------*

CLASS starters DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS get_starters ABSTRACT RETURNING VALUE(starters) TYPE string_table.
ENDCLASS.

CLASS main_dishes DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS get_main_dishes ABSTRACT RETURNING VALUE(main_dishes) TYPE string_table.
ENDCLASS.

CLASS desserts DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS get_desserts ABSTRACT RETURNING VALUE(desserts) TYPE string_table.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete products
*&---------------------------------------------------------------------*

CLASS starters_italian DEFINITION INHERITING FROM starters.
  PUBLIC SECTION.
    METHODS get_starters REDEFINITION.
ENDCLASS.

CLASS starters_italian IMPLEMENTATION.
  METHOD get_starters.
    starters = VALUE #( ( `Bruschetta (1)` ) ( `Caprese salad (1)` ) ( `Antipasto platter (1)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS starters_vegan DEFINITION INHERITING FROM starters.
  PUBLIC SECTION.
    METHODS get_starters REDEFINITION.
ENDCLASS.

CLASS starters_vegan IMPLEMENTATION.
  METHOD get_starters.
    starters = VALUE #( ( `Stuffed mushrooms (1)` ) ( `Zucchini fritters (1)` ) ( `Tomato soup (1)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS starters_seafood DEFINITION INHERITING FROM starters.
  PUBLIC SECTION.
    METHODS get_starters REDEFINITION.
ENDCLASS.

CLASS starters_seafood IMPLEMENTATION.
  METHOD get_starters.
    starters = VALUE #( ( `Shrimp cocktail (1)` ) ( `Crab cakes (1)` ) ( `Calamari (1)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS main_dishes_italian DEFINITION INHERITING FROM main_dishes.
  PUBLIC SECTION.
    METHODS get_main_dishes REDEFINITION.
ENDCLASS.

CLASS main_dishes_italian IMPLEMENTATION.
  METHOD get_main_dishes.
    main_dishes = VALUE #( ( `Spaghetti Carbonara (2)` ) ( `Lasagna alla Bolognese (2)` ) ( `Saltimbocca alla Romana (2)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS main_dishes_vegan DEFINITION INHERITING FROM main_dishes.
  PUBLIC SECTION.
    METHODS get_main_dishes REDEFINITION.
ENDCLASS.

CLASS main_dishes_vegan IMPLEMENTATION.
  METHOD get_main_dishes.
    main_dishes = VALUE #( ( `Chickpea curry (2)` ) ( `Cauliflower steak (2)` ) ( `Vegan burger (2)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS main_dishes_seafood DEFINITION INHERITING FROM main_dishes.
  PUBLIC SECTION.
    METHODS get_main_dishes REDEFINITION.
ENDCLASS.

CLASS main_dishes_seafood IMPLEMENTATION.
  METHOD get_main_dishes.
    main_dishes = VALUE #( ( `Baked salmon (2)` ) ( `Grilled lobster (2)` ) ( `Fish and chips (2)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS desserts_italian DEFINITION INHERITING FROM desserts.
  PUBLIC SECTION.
    METHODS get_desserts REDEFINITION.
ENDCLASS.

CLASS desserts_italian IMPLEMENTATION.
  METHOD get_desserts.
    desserts = VALUE #( ( `Tiramisu (3)` ) ( `Panna cotta (3)` ) ( `Tartufo (3)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS desserts_vegan DEFINITION INHERITING FROM desserts.
  PUBLIC SECTION.
    METHODS get_desserts REDEFINITION.
ENDCLASS.

CLASS desserts_vegan IMPLEMENTATION.
  METHOD get_desserts.
    desserts = VALUE #( ( `Fruit sorbet (3)` ) ( `Almond milk vanilla pudding (3)` ) ( `Apple crumble (3)` ) ).
  ENDMETHOD.
ENDCLASS.

CLASS desserts_seafood DEFINITION INHERITING FROM desserts.
  PUBLIC SECTION.
    METHODS get_desserts REDEFINITION.
ENDCLASS.

CLASS desserts_seafood IMPLEMENTATION.
  METHOD get_desserts.
    desserts = VALUE #( ( `Lemon sorbet (3)` ) ( `Cheesecake (3)` ) ( `Chocolate mousse (3)` ) ).
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Abstract factory
*&---------------------------------------------------------------------*

CLASS menu_factory DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM menu_variant,
             italian,
             seafood,
             vegan,
           END OF ENUM menu_variant.

    METHODS: create_starters ABSTRACT RETURNING VALUE(starters_ref) TYPE REF TO starters,
      create_main_dishes ABSTRACT RETURNING VALUE(main_dishes_ref) TYPE REF TO main_dishes,
      create_desserts ABSTRACT RETURNING VALUE(desserts_ref) TYPE REF TO desserts.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Concrete factories
*&---------------------------------------------------------------------*

CLASS italian_menu_creator DEFINITION INHERITING FROM menu_factory.
  PUBLIC SECTION.
    METHODS: create_starters REDEFINITION,
      create_main_dishes REDEFINITION,
      create_desserts REDEFINITION.

ENDCLASS.

CLASS italian_menu_creator IMPLEMENTATION.

  METHOD create_starters.
    starters_ref = NEW starters_italian( ).
  ENDMETHOD.

  METHOD create_main_dishes.
    main_dishes_ref = NEW main_dishes_italian( ).
  ENDMETHOD.

  METHOD create_desserts.
    desserts_ref = NEW desserts_italian( ).
  ENDMETHOD.
ENDCLASS.

CLASS seafood_menu_creator DEFINITION INHERITING FROM menu_factory.
  PUBLIC SECTION.
    METHODS: create_starters REDEFINITION,
      create_main_dishes REDEFINITION,
      create_desserts REDEFINITION.

ENDCLASS.

CLASS seafood_menu_creator IMPLEMENTATION.

  METHOD create_starters.
    starters_ref = NEW starters_seafood( ).
  ENDMETHOD.

  METHOD create_main_dishes.
    main_dishes_ref = NEW main_dishes_seafood( ).
  ENDMETHOD.

  METHOD create_desserts.
    desserts_ref = NEW desserts_seafood( ).
  ENDMETHOD.

ENDCLASS.

CLASS vegan_menu_creator DEFINITION INHERITING FROM menu_factory.
  PUBLIC SECTION.
    METHODS: create_starters REDEFINITION,
      create_main_dishes REDEFINITION,
      create_desserts REDEFINITION.

ENDCLASS.

CLASS vegan_menu_creator IMPLEMENTATION.

  METHOD create_starters.
    starters_ref = NEW starters_vegan( ).
  ENDMETHOD.

  METHOD create_main_dishes.
    main_dishes_ref = NEW main_dishes_vegan( ).
  ENDMETHOD.

  METHOD create_desserts.
    desserts_ref = NEW desserts_vegan( ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Client
*&---------------------------------------------------------------------*

CLASS menu_provider DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING factory TYPE REF TO menu_factory,
             create_menu RETURNING VALUE(menu) TYPE string_table.
  PRIVATE SECTION.
    DATA factory TYPE REF TO menu_factory.
    DATA starters_factory TYPE REF TO starters.
    DATA main_dishes_factory TYPE REF TO main_dishes.
    DATA desserts_factory TYPE REF TO desserts.
ENDCLASS.

CLASS menu_provider IMPLEMENTATION.
  METHOD create_menu.
    "The more detailed out code lines are to emphasize that this class deals
    "with references to abstract types. Appending the lines may also
    "be achieved with fewer lines of code, as commented out below.
    starters_factory = factory->create_starters( ).
    main_dishes_factory = factory->create_main_dishes( ).
    desserts_factory = factory->create_desserts( ).

    DATA(starters_for_menu) = starters_factory->get_starters( ).
    DATA(main_dishes_for_menu) = main_dishes_factory->get_main_dishes( ).
    DATA(desserts_for_menu) = desserts_factory->get_desserts( ).

    APPEND LINES OF starters_for_menu TO menu.
    APPEND LINES OF main_dishes_for_menu TO menu.
    APPEND LINES OF desserts_for_menu TO menu.

    "APPEND LINES OF factory->create_starters( )->get_starters( ) TO menu.
    "APPEND LINES OF factory->create_main_dishes( )->get_main_dishes( ) TO menu.
    "APPEND LINES OF factory->create_desserts( )->get_desserts( ) TO menu.
  ENDMETHOD.

  METHOD constructor.
    me->factory = factory.
  ENDMETHOD.
ENDCLASS.

"Helper class
CLASS customer_order DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS place_order IMPORTING menu_variant   TYPE menu_factory=>menu_variant
                              RETURNING VALUE(factory) TYPE REF TO menu_factory.
ENDCLASS.

CLASS customer_order IMPLEMENTATION.
  METHOD place_order.
    CASE menu_variant.
      WHEN menu_factory=>italian.
        factory = NEW italian_menu_creator( ).
      WHEN menu_factory=>seafood.
        factory = NEW seafood_menu_creator( ).
      WHEN menu_factory=>vegan.
        factory = NEW vegan_menu_creator( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
