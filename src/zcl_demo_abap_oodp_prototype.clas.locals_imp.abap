*&---------------------------------------------------------------------*
*& Interface serving as the common interface for prototypes
*&---------------------------------------------------------------------*
"- The interface serves as the common interface for all prototype classes,
"  enforcing the implementation of the clone method.
"- This method allows the creation of an clone of an object of a class that
"  implements this interface.
"- It returns a new reference, representing the object clone.

INTERFACE lif_prototype.
  METHODS clone RETURNING VALUE(ref) TYPE REF TO lif_prototype.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& Prototype class 1
*&---------------------------------------------------------------------*
"- This demo class implements the lif_prototype interface that allows users
"  to retrieve a clone of an object from the class.
"- Contains various class attributes and methods. Methods such as 'do_something'
"  modify the value of class attributes.

CLASS lcl_prototype_1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_prototype.
    METHODS constructor IMPORTING str TYPE string.
    METHODS do_something IMPORTING text TYPE string.
    METHODS return_data RETURNING VALUE(str) TYPE string.

    DATA number TYPE i.
    DATA txt TYPE string.
    DATA itab TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA private_text TYPE string.
ENDCLASS.

CLASS lcl_prototype_1 IMPLEMENTATION.

  METHOD constructor.
    "The constructor (or any other part of the class that modifies the object's state) assumes
    "there may be complex operations involved. However, this simplified example only assigns
    "values to class attributes to demonstrate object cloning and enhancement.

    txt = str.
    private_text = `#`.
    itab = VALUE #( ( |{ txt } (added at { utclong_current( ) })| ) ).
  ENDMETHOD.

  METHOD lif_prototype~clone.
    "Creating a clone of the current object by instantiating a new class instance with the NEW
    "operator and copying the class attribute values. This example selectively clones certain
    "attributes, showcasing a predefined state for the cloned object.

    DATA(obj_ref) = NEW lcl_prototype_1( txt ).
    obj_ref->itab = me->itab.
    obj_ref->txt = me->txt.
    obj_ref->private_text = me->private_text.
    ref = obj_ref.
  ENDMETHOD.

  METHOD do_something.
    "The demo implementation includes some simple class attribute modifications.

    APPEND |{ text } (added at { utclong_current( ) })| TO itab.
    number += 1.
    txt &&= `!`.
    private_text &&= `#`.
  ENDMETHOD.

  METHOD return_data.
    "The demo implementation returns the content of the string table as string.
    "Additionally and for display purposes, the content of a private attribute
    "is added to the result string.

    str = concat_lines_of( table = itab sep = |\n| ).
    str &&= |\nprivate_text = "{ private_text }"|.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Prototype class 2
*&---------------------------------------------------------------------*
"- This demo class implements the lif_prototype interface that allows users
"  to retrieve a clone of an object from the class.
"- It is structured similarly to lcl_prototype_1, but it features different
"  attributes and behaviors.

CLASS lcl_prototype_2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_prototype.
    METHODS constructor IMPORTING t TYPE string.
    METHODS do_another_thing IMPORTING text TYPE string.
    METHODS get_state RETURNING VALUE(state) TYPE string.

    DATA string TYPE string.
    DATA tab TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA private_integer TYPE i.
ENDCLASS.

CLASS lcl_prototype_2 IMPLEMENTATION.

  METHOD constructor.
    string = t.
    tab = VALUE #( ( |{ string } (added at { utclong_current( ) }| ) ).
  ENDMETHOD.

  METHOD lif_prototype~clone.
    DATA(obj_ref) = NEW lcl_prototype_2( string ).
    obj_ref->tab = me->tab.
    obj_ref->string = me->string.
    ref = obj_ref.
  ENDMETHOD.

  METHOD do_another_thing.
    APPEND |{ text } (added at { utclong_current( ) })| TO tab.
    private_integer += 1.
    string &&= `#`.
  ENDMETHOD.

  METHOD get_state.
    "This demo implementation is intended to visualize enhancement of cloned objects.
    "In this case, the value of the private attribute is evaluated.
    IF private_integer > 0.
      state = |State changed. Value of "private_integer": { private_integer }|.
    ELSE.
      state = |State not changed. Value of "private_integer": { private_integer }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
