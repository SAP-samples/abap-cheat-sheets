"! <p class="shorttext"><strong>Decorator</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the decorator design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_decorator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_decorator IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Decorator` ).

    "---------------- Basic pizza ----------------
    DATA(basic_pizza) = NEW lcl_basic_pizza( 10 ).
    DATA(basic_pizza_created) = basic_pizza->lif_pizza~create_pizza( ).
    DATA(basic_pizza_costs) = basic_pizza->lif_pizza~get_costs( ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Basic pizza` ).
    out->write( |\nIngredients:| ).
    out->write( basic_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( basic_pizza_costs ).

    "---------------- Salami pizza ----------------
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Salami pizza` ).

    DATA(salami_pizza) = NEW lcl_decorator_salami( pizza = basic_pizza ).
    DATA(salami_pizza_created) = salami_pizza->lif_pizza~create_pizza( ).
    DATA(salami_pizza_costs) = salami_pizza->lif_pizza~get_costs( ).

    out->write( |\nIngredients:| ).
    out->write( salami_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( salami_pizza_costs ).

    "---------------- Vegetable pizza ----------------
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Vegetable pizza` ).

    DATA(vegetarian_pizza) = NEW lcl_decorator_vegetables( pizza = basic_pizza ).
    DATA(vegetarian_pizza_created) = vegetarian_pizza->lif_pizza~create_pizza( ).
    DATA(vegetarian_pizza_costs) = vegetarian_pizza->lif_pizza~get_costs( ).

    out->write( |\nIngredients:| ).
    out->write( vegetarian_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( vegetarian_pizza_costs ).

    "---------------- Vegetable/salami pizza ----------------
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Vegetable/salami pizza` ).

    DATA(vegetable_salami_pizza) = NEW lcl_decorator_vegetables(
      pizza = NEW lcl_decorator_salami(
      pizza = NEW lcl_basic_pizza( 10 ) ) ).
    DATA(vegetable_salami_pizza_created) = vegetable_salami_pizza->lif_pizza~create_pizza( ).
    DATA(vegetable_salami_pizza_costs) = vegetable_salami_pizza->lif_pizza~get_costs( ).

    out->write( |\nIngredients:| ).
    out->write( vegetable_salami_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( vegetable_salami_pizza_costs ).

    "---------------- Mushroom/salami pizza ----------------
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Mushrooms/salami pizza` ).

    DATA(mushrooms_salami_pizza) = NEW lcl_decorator_mushrooms(
      pizza = NEW lcl_decorator_salami(
      pizza = NEW lcl_basic_pizza( 10 ) ) ).
    DATA(mushrooms_salami_pizza_created) = mushrooms_salami_pizza->lif_pizza~create_pizza( ).
    DATA(mushrooms_salami_pizza_costs) = mushrooms_salami_pizza->lif_pizza~get_costs( ).

    out->write( |\nIngredients:| ).
    out->write( mushrooms_salami_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( mushrooms_salami_pizza_costs ).

    "--- Various pizzas (created using the same interface reference variable) ---
    DATA some_pizza TYPE REF TO lif_pizza.
    some_pizza = NEW lcl_basic_pizza( 10 ).
    DATA(some_pizza_created_a) = some_pizza->create_pizza( ).
    DATA(some_pizza_costs_a) = some_pizza->get_costs( ).

    some_pizza = NEW lcl_decorator_vegetables( pizza = some_pizza ).
    DATA(some_pizza_created_b) = some_pizza->create_pizza( ).
    DATA(some_pizza_costs_b) = some_pizza->get_costs( ).

    some_pizza = NEW lcl_decorator_salami( pizza = some_pizza ).
    DATA(some_pizza_created_c) = some_pizza->create_pizza( ).
    DATA(some_pizza_costs_c) = some_pizza->get_costs( ).

    "---------------- Allergy-friendly vegetable pizza ----------------
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Allergy-friendly vegetable pizza` ).

    DATA(allergy_friendly_vegetbl_pizza) = NEW lcl_decorator_allergy_friendly(
        pizza = NEW lcl_decorator_vegetables(
        pizza = NEW lcl_basic_pizza( 10 ) ) ).
    DATA(all_friendly_veg_pizza_created) = allergy_friendly_vegetbl_pizza->lif_pizza~create_pizza( ).
    DATA(all_friendly_veg_pizza_costs) = allergy_friendly_vegetbl_pizza->lif_pizza~get_costs( ).

    out->write( |\nIngredients:| ).
    out->write( all_friendly_veg_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( all_friendly_veg_pizza_costs ).

    "---------------- Pizza with all ingredients ----------------
    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Pizza with all ingredients` ).

    "Using all concrete decorator classes of the example
    DATA(misc_pizza) = NEW lcl_decorator_allergy_friendly(
       pizza = NEW lcl_decorator_mushrooms(
       pizza = NEW lcl_decorator_salami(
       pizza = NEW lcl_decorator_vegetables(
       pizza = NEW lcl_basic_pizza( 10 ) ) ) ) ).
    DATA(misc_pizza_created) = misc_pizza->lif_pizza~create_pizza( ).
    DATA(misc_pizza_costs) = misc_pizza->lif_pizza~get_costs( ).

    out->write( |\nIngredients:| ).
    out->write( misc_pizza_created ).
    out->write( |\nCosts:| ).
    out->write( misc_pizza_costs ).
  ENDMETHOD.
ENDCLASS.
