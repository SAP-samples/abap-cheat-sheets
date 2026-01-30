"! <p class="shorttext"><strong>ABAP object orientation - Inheritance</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class explores inheritance and demonstrate a selection of the inheritance-related syntax.
"! The inheritance tree consists of four example classes. The base class {@link zcl_demo_abap_oo_inheritance_1}
"! includes the implementation of the classrun interface. Choose F9 in ADT to run the base class.</p>
"! <p>Classes of the inheritance tree:</p>
"! <ul><li>{@link zcl_demo_abap_oo_inheritance_1}</li>
"! <li>{@link zcl_demo_abap_oo_inheritance_2}</li>
"! <li>{@link zcl_demo_abap_oo_inheritance_3}</li>
"! <li>{@link zcl_demo_abap_oo_inheritance_4}</li>
"! </ul>
"!
"! <h2>Notes on this class</h2>
"! <ul><li>Inherits from {@link zcl_demo_abap_oo_inheritance_1} and thus from {@link zcl_demo_abap_oo_inheritance_2}</li>
"! <li>Declared as abstract class using the <em>ABSTRACT</em> addition, so no instances can be created from the class</li>
"! <li>Declares several instance methods</li>
"! <li>Two abstract methods are included using the <em>ABSTRACT</em> addition, so they can only be implemented in subclasses (there
"! is no implementation of these methods in the class)</li>
"! <li>Instance methods of the direct superclass are redefined as well as methods from two levels up the inheritance hierarchy</li>
"! <li>One redefined method specifies <em>FINAL REDEFINITION</em>, so a further redefinition in subclasses is not possible.</li></ul>
"!
"! <h2>More information</h2>
"! <ul><li>Find general information on the inheritance example in class {@link zcl_demo_abap_oo_inheritance_1}</li>
"! <li>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</li></ul>
CLASS zcl_demo_abap_oo_inheritance_3 DEFINITION
  INHERITING FROM zcl_demo_abap_oo_inheritance_2
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Instance/static constructor declarations
    METHODS constructor.
    CLASS-METHODS class_constructor.

    "Instance method declarations
    METHODS meth_public_3.
    "Abstract method
    METHODS meth_public_3_abstract ABSTRACT.

    "Redefining methods from the class ...
    "... one level up in the inheritance hierarchy (i.e. the direct superclass)
    METHODS meth_public_2  REDEFINITION.
    "... two levels up in the inheritance hierarchy
    METHODS meth_public_1 REDEFINITION.

  PROTECTED SECTION.
    "Instance method declarations
    METHODS meth_protected_3.
    "Abstract method
    METHODS meth_protected_3_abstract ABSTRACT.

    "Redefining methods from the class ...
    "... one level up in the inheritance hierarchy (i.e. the direct superclass)
    METHODS meth_protected_2 REDEFINITION.
    "... two levels up in the inheritance hierarchy
    "Specifying the FINAL addition
    METHODS meth_protected_1 FINAL REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_abap_oo_inheritance_3 IMPLEMENTATION.

  METHOD class_constructor.
     INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_3.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_3.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_2.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_2.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_1.
    "Reimplementing a method from the class two levels up in the inheritance hierarchy
    "Note that the method is specified with FINAL REDEFINITION. So, a further redefinition in subclasses is not possible.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1.
    "Reimplementing a method from the class two levels up in the inheritance hierarchy
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.
ENDCLASS.
