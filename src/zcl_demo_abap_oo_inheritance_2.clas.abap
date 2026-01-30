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
"! <ul><li>Specifies <em>CREATE PROTECTED</em>, so the class can only be instantiated in methods of its subclasses,
"! of the class itself, and of its friends</li>
"! <li>You may want to try to create an instance of the class in {@link zcl_demo_abap_oo_inheritance_1}
"! like this <em>DATA(oref) = NEW zcl_demo_abap_oo_inheritance_2( ).</em>. It is not possible. In
"! {@link zcl_demo_abap_oo_inheritance_3} and {@link zcl_demo_abap_oo_inheritance_4}, for example, it is
"! possible.</li>
"! <li>Declares several instance methods</li>
"! <li>One of them is declared with <em>FINAL</em>, so no redefinition is possible in subclasses</li>
"! <li>Instance methods of the direct superclass are redefined</li>
"! <li>Note: Private methods of superclasses cannot be redefined. You cannot specify abstract methods, which
"! is only possible in abstract classes. Abstract methods are generally not possible in the private visibility
"! section since they cannot be redefined.</li>
"! <li>Declares a static method to delegate method calls of this class</li>
"! <li>The implementation includes the creation of an instance of the class and instance method calls
"! (including redefined methods).</li>
"! <li>It is called in the classrun implementation in {@link zcl_demo_abap_oo_inheritance_1}.</li></ul>
"!
"! <h2>More information</h2>
"! <ul><li>Find general information on the inheritance example in class {@link zcl_demo_abap_oo_inheritance_1}</li>
"! <li>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</li></ul>
CLASS zcl_demo_abap_oo_inheritance_2 DEFINITION
  INHERITING FROM zcl_demo_abap_oo_inheritance_1
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    "Instance/static constructor declarations
    METHODS constructor.
    CLASS-METHODS class_constructor.

    "Instance method declarations
    METHODS meth_public_2.
    METHODS meth_public_2_final FINAL.

    "Static method declaration for display purposes
    CLASS-METHODS perform_meth_calls_2.

    "Redefining method from the class one level up in the inheritance hierarchy
    "(i.e. the direct superclass)
    METHODS meth_public_1 REDEFINITION.

    "Excursions:
    "- Redefining the final public method of the superclass is not possible.
    "- The same applies to constructors.
    "- The following type has the same name as a type in the superclass. Since components
    "  are inherited, the following declaration is not possible.
    "TYPES t_log TYPE string_table.
    "- Similary, the log table, which is a static component in the superclass, can be
    "  referenced in the method implementations using 'log_table'. Using the class name
    "  and => is also possible, but not required.
    "INSERT VALUE #( ) INTO TABLE zcl_demo_abap_oo_inheritance_1=>log_tab.

  PROTECTED SECTION.
    "Instance method declaration
    METHODS meth_protected_2.

    "Redefining method from the class one level up in the inheritance hierarchy
    "(i.e. the direct superclass)
    METHODS meth_protected_1 REDEFINITION.

  PRIVATE SECTION.
    "Ecursion:
    "- The following declarations are not possible.
    "- Private methods cannot be redefined.
    "METHODS meth_private_1 REDEFINITION.
    "- Abstract methods can only be declared in abstract classes. And, since
    "  private methods cannot be redefined, abstract private methods are not
    "  possible.
    "METHODS meth_private_2_abstract ABSTRACT.
ENDCLASS.



CLASS zcl_demo_abap_oo_inheritance_2 IMPLEMENTATION.
  METHOD class_constructor.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_2.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_2_final.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_2.
    "Method of this class
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_1.
    "Reimplementing a method from the class one level up in the inheritance hierarchy (direct superclass)
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD perform_meth_calls_2.
    "Method of this class
    "Creating an instance of the class
    DATA(oref_2) = NEW zcl_demo_abap_oo_inheritance_2( ).

    "Calling methods of this class
    oref_2->meth_public_2( ).
    oref_2->meth_public_2_final( ).
    oref_2->meth_protected_2( ).
    "Calling redefined methods from the class one level up in the inheritance hierarchy (direct superclass)
    oref_2->meth_protected_1( ).
    oref_2->meth_public_1( ).

  ENDMETHOD.

ENDCLASS.
