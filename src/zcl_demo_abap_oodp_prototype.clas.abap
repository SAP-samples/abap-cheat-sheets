"! <p class="shorttext"><strong>Prototype</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the prototype design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_prototype DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oodp_prototype IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Prototype` ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `1) Processing original object (prototype 1)` ).

    "Notes:
    "- Creating objects of the prototype classes.
    "- Methods are called and class attributes are accessed to show changes in attribute values,
    "  resulting in specific object states.

    DATA(oref_1) = NEW lcl_prototype_1( str = `ABC` ).
    oref_1->do_something( `DEF` ).
    DATA(content) = oref_1->return_data( ).
    DATA(num) = oref_1->number.
    DATA(txt) = oref_1->txt.

    "Displaying content
    out->write( data = content name = `content` ).
    out->write( |\n| ).
    out->write( data = num name = `num` ).
    out->write( |\n| ).
    out->write( data = txt name = `txt` ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `2) Cloning prototype (1)` ).

    "Notes:
    "- Cloning objects using the clone method, which creates copies of existing
    "  objects.
    "- The assumption is that the user needs a copy of the object to reuse a
    "  specific state instead of creating a new object from scratch.
    "- At this stage, the cloned object has the same state as the original object.

    DATA(oref_2) = oref_1->lif_prototype~clone( ).

    "Displaying the public class atrributes
    out->write( data = oref_2 name = `oref_2` ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `3) Enhancing cloned prototype (1)` ).

    "Notes:
    "- Enhancing the cloned objects.
    "- This involves casting to the prototype class to access its attributes
    "  and methods.
    "- The assumption is to create a variant of the object by modifying its
    "  attributes while using the original state as a foundation.
    "- Working with the variant involves a separate object, distinct from the original.
    "  A demo attribute shows that the clone method can be implemented to allow selective
    "  cloning, excluding certain attribute values.

    DATA(oref_3) = CAST lcl_prototype_1( oref_2 ).
    content = oref_3->return_data( ).
    num = oref_3->number.
    txt = oref_3->txt.

    "Displaying content (original state)
    out->write( data = content name = `content` ).
    out->write( |\n| ).
    out->write( data = num name = `num` ).
    out->write( |\n| ).
    out->write( data = txt name = `txt` ).
    out->write( |\n\n| ).

    "Enhancing the cloned object
    oref_3->do_something( `GHI` ).
    content = oref_3->return_data( ).
    num = oref_3->number.
    txt = oref_3->txt.

    "Displaying content (enhanced state)
    out->write( data = content name = `content` ).
    out->write( |\n| ).
    out->write( data = num name = `num` ).
    out->write( |\n| ).
    out->write( data = txt name = `txt` ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `4) Processing original object (prototype 2)` ).

    "Notes:
    "- Creating objects of the prototype classes.
    "- Methods are called and class attributes are accessed to show changes in attribute values,
    "  resulting in specific object states.

    DATA(oref_4) = NEW lcl_prototype_2( `JKL` ).
    oref_4->do_another_thing( `MNO` ).
    DATA(tab) = oref_4->tab.
    DATA(string) = oref_4->string.
    DATA(state) = oref_4->get_state( ).

    "Displaying content
    out->write( data = tab name = `tab` ).
    out->write( |\n| ).
    out->write( data = string name = `string` ).
    out->write( |\n| ).
    out->write( data = state name = `state` ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `5) Cloning prototype (2)` ).

    "Notes:
    "- Cloning objects using the clone method, which creates copies of existing
    "  objects.
    "- The assumption is that the user needs a copy of the object to reuse a
    "  specific state instead of creating a new object from scratch.
    "- At this stage, the cloned object has the same state as the original object.

    DATA(oref_5) = oref_4->lif_prototype~clone( ).

    "Displaying the public class atrributes
    out->write( data = oref_5 name = `oref_5` ).

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `6) Enhancing cloned prototype (2)` ).

    "Notes:
    "- Enhancing the cloned objects.
    "- This involves casting to the prototype class to access its attributes
    "  and methods.
    "- The assumption is to create a variant of the object by modifying its
    "  attributes while using the original state as a foundation.
    "- Working with the variant involves a separate object, distinct from the original.
    "  A demo attribute shows that the clone method can be implemented to allow selective
    "  cloning, excluding certain attribute values.

    DATA(oref_6) = CAST lcl_prototype_2( oref_5 ).
    tab = oref_6->tab.
    string = oref_6->string.
    state = oref_6->get_state( ).

    "Displaying content (original state)
    out->write( data = tab name = `tab` ).
    out->write( |\n| ).
    out->write( data = string name = `string` ).
    out->write( |\n| ).
    out->write( data = state name = `state` ).
    out->write( |\n\n| ).

    "Enhancing the cloned object
    oref_6->do_another_thing( `PQR` ).
    tab = oref_6->tab.
    string = oref_6->string.
    state = oref_6->get_state( ).

    "Displaying content (enhanced state)
    out->write( data = tab name = `tab` ).
    out->write( |\n| ).
    out->write( data = string name = `string` ).
    out->write( |\n| ).
    out->write( data = state name = `state` ).
  ENDMETHOD.

ENDCLASS.
