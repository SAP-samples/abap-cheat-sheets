"! <p class="shorttext"><strong>ABAP object orientation - Inheritance</strong><br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class explores inheritance and demonstrate a selection of the inheritance-related syntax.
"! The inheritance tree consists of four example classes. The base class {@link zcl_demo_abap_oo_inheritance_1}
"! includes the implementation of the classrun interface. Choose F9 in ADT to run the class.</p>
"! <p>Classes of the inheritance tree:</p>
"! <ul><li>{@link zcl_demo_abap_oo_inheritance_1}</li>
"! <li>{@link zcl_demo_abap_oo_inheritance_2}</li>
"! <li>{@link zcl_demo_abap_oo_inheritance_3}</li>
"! <li>{@link zcl_demo_abap_oo_inheritance_4}</li>
"! </ul>
"!
"! <h2>Purpose</h2>
"! <p>The purpose of the example and information output is to visualize and explore concepts and syntax related to
"! inheritance, checking out when and how methods are called, redefining methods, abstract and final classes and methods.</p>
"!
"! <h2>General notes on the example</h2>
"! <p>Many instance methods are declared in all classes to demonstrate inheritance. However, there is no meaningful implementation
"! in these methods in all classes. All instance methods include the same code. The purpose of the code in the method implementations
"! is to add a line to a log table (which is output to the console) with various pieces of information:</p>
"! <ul><li>Name of the method that is called</li>
"! <li>In which class the method is implemented when it is called</li>
"! <li>From which class the method is called</li>
"! <li>Whether the method is inherited, redefined, final, or a static method</li>
"! <li>Visibility of the method</li>
"! <li>Visibility of the method</li></ul>
"! <p>The information retrieval is implemented in a static method in the {@link zcl_demo_abap_oo_inheritance_1} class by getting callstack
"! information to determine which method in which class was called by whom. Based on the retrieved class and method names, RTTI
"! is used to get detailed information about the methods.</p>
"!
"! <h2>Notes on this class</h2>
"! <ul><li>Allows inheritance and represents the root class of the inheritance hierarchy</li>
"! <li>Declares several instance methods in each visibility section</li>
"! <li>One of them is declared with FINAL, so no redefinition is possible in subclasses.</li>
"! <li>Includes the implementation of the classrun interface meaning this class is executable using F9 in ADT.</li>
"! <li>The class includes an internal table that represents a log table and that is output to the console as described above.</li></ul>
"!
"! <h2>More information</h2>
"! <p>Find information on <strong>getting started with the example class</strong> and the
"! <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_aux}.</p>
CLASS zcl_demo_abap_oo_inheritance_1 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    "Classrun interface
    INTERFACES if_oo_adt_classrun.

    "Instance/static constructor declarations
    METHODS constructor.
    CLASS-METHODS class_constructor.

    "Instance method declarations
    METHODS meth_public_1.
    "Final method
    METHODS meth_public_1_final FINAL.

    "Components used for logging information about method calls
    TYPES: BEGIN OF s_log,
             method            TYPE string,
             implemented_where TYPE string,
             called_from       TYPE syrepid,
             is_inherited      TYPE abap_boolean,
             is_redefined      TYPE abap_boolean,
             is_final          TYPE abap_boolean,
             visibility        TYPE abap_visibility,
             is_static_method  TYPE abap_boolean,
             called_at         TYPE utclong,
           END OF s_log,
           t_log TYPE TABLE OF s_log WITH EMPTY KEY.

    CLASS-DATA log_tab TYPE t_log.
    CLASS-METHODS get_method_info RETURNING VALUE(info) TYPE s_log.

  PROTECTED SECTION.
    METHODS meth_protected_1.

  PRIVATE SECTION.
    METHODS meth_private_1.
ENDCLASS.



CLASS zcl_demo_abap_oo_inheritance_1 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP cheat sheet example: ABAP Object Orientation - Inheritance\n\n| ).

    "----- First level in the inheritance hierarchy ----
    "Creating an instance of the class
    DATA(oref_super) = NEW zcl_demo_abap_oo_inheritance_1( ).

    "Calling methods of the class
    oref_super->meth_public_1( ).
    oref_super->meth_public_1_final( ).
    oref_super->meth_protected_1( ).
    oref_super->meth_private_1( ).

    "----- Second level in the inheritance hierarchy ----
    "The instance creation and method calling is delegated to
    "a static method in the class
    zcl_demo_abap_oo_inheritance_2=>perform_meth_calls_2( ).

    "----- Third level in the inheritance hierarchy ----
    "Note: The class zcl_demo_abap_oo_inheritance_3 is abstract and contains
    "both non-abstract and abstract instance methods. Instances of abstract
    "classes cannot be created. So, the following statement is not possible.
    "DATA(oref_3) = NEW zcl_demo_abap_oo_inheritance_3( ).

    "Instance components of an abstract class can be accessed via its subclasses.
    "zcl_demo_abap_oo_inheritance_4 inherits from zcl_demo_abap_oo_inheritance_3 and
    "redefines methods of zcl_demo_abap_oo_inheritance_3. Both abstract methods (which
    "are mandatory to implement) and non-abstract methods are redefined. To also access
    "the method implementations of the non-abstract instance methods of
    "zcl_demo_abap_oo_inheritance_3, the respective implementations of the redefined
    "methods in zcl_demo_abap_oo_inheritance_4 include method calls to the direct
    "superclass using the syntax super->meth( ).. The instance methods of
    "zcl_demo_abap_oo_inheritance_3 are called in the context of the static method call
    "via zcl_demo_abap_oo_inheritance_4 below.

    "----- Fourth level in the inheritance hierarchy ----
    "As above, the instance creation and method calling is delegated to
    "a static method in the class. This method call includes method calls to
    "non-abstract instance methods implemented in zcl_demo_abap_oo_inheritance_3.
    zcl_demo_abap_oo_inheritance_4=>perform_meth_calls_4( ).

    "Writing the log table to the console
    out->write( data = log_tab name = `log_tab` ).

    "Excursion: Using RTTI to retrieve the name of the superclass
    "As this class starts an inheritance hierarchy, the superclass of this class
    "is the root class OBJECT.
    DATA(tdo_cl) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( 'ZCL_DEMO_ABAP_OO_INHERITANCE_1' ) ).
    DATA(superclass) = tdo_cl->get_super_class_type( )->get_relative_name( ).
    out->write( |\n\n| ).
    out->write( data = superclass name = `superclass` ).

  ENDMETHOD.

  METHOD class_constructor.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD constructor.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_private_1.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_protected_1.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD meth_public_1_final.
    INSERT VALUE #( called_at = utclong_current( ) ) INTO TABLE log_tab ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = CORRESPONDING #( BASE ( <fs> ) get_method_info( ) EXCEPT called_at ).
  ENDMETHOD.

  METHOD get_method_info.
    "This method retrieves callstack information to determine which method in which
    "class was called by whom.
    "Based on the retrieved class and method names, RTTI is used to get detailed
    "information about methods (such as the visibility or whether the method is
    "inherited, redefined, final, and a static method).

    "Getting callstack information
    DATA(call_stack_tab) = xco_cp=>current->call_stack->full( )->from->position( 2
                            )->to->position( 2 )->as_text( xco_cp_call_stack=>format->adt( )
                            )->get_lines( )->value.

    IF lines( call_stack_tab ) < 2.
      RETURN.
    ENDIF.

    LOOP AT call_stack_tab INTO DATA(wa) TO 2.
      DATA(tabix) = sy-tabix.
      SPLIT wa AT ` ` INTO TABLE DATA(entry).
      DELETE entry WHERE table_line IS INITIAL.

      DATA(class_name) = condense( val = entry[ 1 ] to = `` ).

      IF tabix = 1.
        info-implemented_where = class_name.

        DATA(meth_name) = condense( val = to_upper( entry[ 2 ] ) to = `` ).
        info-method = meth_name.

        IF class_name IS NOT INITIAL AND meth_name IS NOT INITIAL.
          DATA(tdo_cl) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( class_name ) ).
          DATA(methods_cl) = tdo_cl->methods.
          DATA(meth_info) = VALUE #( methods_cl[ name = meth_name ] OPTIONAL ).
          IF meth_info IS NOT INITIAL.
            info-is_inherited = meth_info-is_inherited.
            info-is_redefined = meth_info-is_redefined.
            info-is_final = meth_info-is_final.
            info-visibility = meth_info-visibility.
            info-is_static_method = meth_info-is_class.
          ENDIF.
        ENDIF.

      ELSE.
        info-called_from = class_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
