***********************************************************************
*
*             ABAP cheat sheet: ABAP object orientation
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntax options and concepts related
*   to ABAP object orientation.
* - Topics covered: Working with objects and components, method redefinition
*   in inheritance, working with interfaces, upcast and downcast, concepts
*   like factory methods, singleton and abstract classes, using events.
* - The CCIMP include (local types tab in ADT) includes multiple local
*   classes to support the example.
* - Artifacts related to this example:
*   - zdemo_abap_objects_interface: Separate global interface to demonstrate
*     working with interfaces
*   - zcl_demo_abap_objects_friend: Another global class used to demonstrate
*     the concept of friendship
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP development tools for Eclipse (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, refer to the
*   notes included in the class as comments or refer to the respective
*   topic in the ABAP Keyword Documentation.
* - Due to the amount of console output, the examples contain numbers
*   (e.g. 1) ..., 2) ..., 3) ...) for the individual example sections.
*   Also, the variable name is displayed in most cases. So to find
*   the relevant output in the console easier and faster, just search
*   for the number/variable name in the console (CTRL+F in the console)
*   or use the debugger.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is intended only to support the ABAP
* cheat sheets. It is not intended for direct use in a production system
* environment. The code examples in the ABAP cheat sheets are primarily
* intended to provide a better explanation and visualization of the
* syntax and semantics of ABAP statements, not to solve concrete
* programming tasks. For production application programs, you should
* always work out your own solution for each individual case. There is
* no guarantee for the correctness or completeness of the code.
* Furthermore, there is no legal responsibility or liability for any
* errors or their consequences that may occur when using the the example
* code.
*
***********************************************************************
"! <p class="shorttext synchronized">ABAP cheat sheet: ABAP object orientation</p>
"! Example to demonstrate concepts related to ABAP object orientation.<br>Choose F9 in ADT to run the class.
CLASS zcl_demo_abap_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_demo_abap_objects_friend.

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun,
      zdemo_abap_objects_interface.
    ALIASES triple FOR zdemo_abap_objects_interface~triple.

    METHODS: hallo_instance_method,
      "Demo method for self-reference me
      me_ref_meth EXPORTING e1 TYPE string e2 TYPE string.

    DATA: another_string TYPE string VALUE `I'm just a public string.`.

    CLASS-METHODS:
      hallo_static_method.

    CLASS-DATA: string        TYPE string,
                public_string TYPE string VALUE `I'm a string from a friend's public section. I'm accessible anyway.`.

  PROTECTED SECTION.
    CLASS-DATA: protected_string TYPE string VALUE `I'm a string from a friend's protected section.`.

  PRIVATE SECTION.

    CLASS-DATA:
      private_string TYPE string VALUE `I'm a string from a friend's private section.`.

ENDCLASS.



CLASS zcl_demo_abap_objects IMPLEMENTATION.


  METHOD hallo_instance_method.
    string = |Hallo { sy-uname }. | &&
             |I'm an instance method of class zcl_demo_abap_objects.|.
  ENDMETHOD.


  METHOD hallo_static_method.
    string = |Hallo { sy-uname }. | &&
             |I'm a static method of class zcl_demo_abap_objects.|.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |ABAP Cheat Sheet Example: ABAP Object Orientation\n\n| ).
    out->write( |Working with objects and components\n\n| ).
    out->write( |1) Declaring reference variables\n\n| ).

    "To create an object, a reference variable must be declared. This
    "variable is also necessary for accessing objects, i. e. objects
    "are not directly accessed but only via references that "point to"
    "those objects. And this reference is stored in the reference
    "variables. The example below demonstrate multiple reference
    "variables that are created using statements with TYPE REF TO.
    "LIKE is also possible. You can also create a type with
    "TYPE REF TO.

    DATA: ref1a TYPE REF TO local_class,
          ref1b TYPE REF TO local_class,
          ref1c LIKE ref1a.

    TYPES: ref_type TYPE REF TO local_class.
    DATA: ref1d TYPE ref_type.

    IF  ref1a IS INITIAL
    AND ref1b IS INITIAL
    AND ref1c IS INITIAL
    AND ref1d IS INITIAL.
      out->write( `The declared reference variables are initial.` ).
    ELSE.
      out->write( `One or more of the declared reference ` &&
                    `variables are not initial.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `2) Creating objects` ) ).

    "You create an object in the memory of an application by using the
    "instance operator NEW. In doing so, a new instance of a
    "class is created and the "address" of the instance is put into the
    "reference variable. The # sign means to use the type (TYPE REF TO)
    "of the reference variable. You can also omit the explicit
    "declaration of a reference variable by declaring a new reference
    "variable inline. In this case, the name of the class must be
    "placed after NEW. As an alternative to the NEW operator, you can
    "also use the older CREATE OBJECT statements.

    DATA ref2a TYPE REF TO local_class.

    ref2a = NEW #( ).
    DATA(ref2b) = NEW local_class( ).

    "NEW replaces the following statement
    CREATE OBJECT ref2a.

    IF ref2a IS INSTANCE OF local_class
    AND ref2b IS INSTANCE OF local_class.
      out->write( `ref2a and ref2b point to instances ` &&
                    `of the class local_class.` ).
    ELSE.
      out->write( `One or more of the reference variables ` &&
      `do not point to instances of the class local_class.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `3) Assigning object references` ) ).

    "Without an assignment, the reference variable is empty.
    "To assign or copy reference variable, use the assignment operator
    "=. In the example below, both reference variables have the same
    "type.

    DATA: ref3a TYPE REF TO local_class,
          ref3b TYPE REF TO local_class.

    ref3a = ref3b.

    IF ref3a = ref3b.
      out->write( `ref3b has been assigned to ref3a.` ).
    ELSE.
      out->write( `ref3b has not been assigned to ref3a.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `4) Overwriting object references` ) ).

    "An object reference is overwritten when a new object is created
    "with a reference variable already pointing to an instance.
    "The class is implemented in a way that the number of instances
    "that are created is counted. In this example, the output is
    "just to visualize that the first ref4 is indeed overwritten.

    DATA ref4 TYPE REF TO local_class.

    ref4 = NEW #( ).

    out->write( data = ref4->no_of_instances name = `ref4->no_of_instances` ).
    out->write( |\n| ).

    ref4 = NEW #( ).

    out->write( data = ref4->no_of_instances name = `ref4->no_of_instances` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `5) Keeping references variables in internal tables` ) ).

    "The following code shows that the reference variable is
    "overwritten in the course of the loop multiple times.
    "Since the reference variables are stored in an internal table, the
    "current state of the object is not lost once the reference
    "variable is overwritten. The difference in the retained state of
    "the object is visible in the timestamp und uuid field. The values
    "are created in the constructor when an instance is instantiated.

    DATA: ref5  TYPE REF TO local_class,
          itab5 TYPE TABLE OF REF TO local_class.

    DO 3 TIMES.
      ref5 = NEW #( ).
      itab5 = VALUE #( BASE itab5 ( ref5 ) ).
    ENDDO.

    out->write( data = itab5 name = `itab5` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `6) Clearing object references` ) ).

    "Use CLEAR statements to explicitly clear a reference variable.
    "Since objects use up space in the memory, they should be cleared
    "if they are no longer needed. Actually, the garbage collector
    "takes over this task automatically, i. e. all objects without any
    "reference are cleared and the memory space is released.

    DATA ref6 TYPE REF TO local_class.

    ref6 = NEW #( ).

    CLEAR ref6.

    IF ref6 IS INITIAL.
      out->write( `ref6 is initial.` ).
    ELSE.
      out->write( `ref6 is not initial.` ).
    ENDIF.

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `7) Accessing and using attributes` ) ).

    "Instance attributes are accessed using the object component
    "selector -> via a reference variable. Visible static attributes
    "are accessed using the class component selector => via the class
    "name. You can also declare data objects and types by referring
    "to those attributes.

    DATA ref7 TYPE REF TO local_class.
    ref7 = NEW #( ).

    "Instance + static attribute from individual objects.
    DATA(obj_instance_attr) = ref7->num_inst.
    DATA(obj_static_attr_obj) = ref7->num_stat.

    "Static attributes
    DATA(class_static_attr) = local_class=>num_stat.

    "Data objects and types whose type definitions can be based on
    "static class attributes
    DATA  some_int       LIKE local_class=>num_stat.
    DATA  some_other_int TYPE local_class=>type_i.
    TYPES int_type       TYPE local_class=>type_i.

    out->write( data = obj_instance_attr name = `obj_instance_attr` ).
    out->write( |\n| ).
    out->write( data = obj_static_attr_obj name = `obj_static_attr_obj` ).
    out->write( |\n| ).
    out->write( data = class_static_attr name = `class_static_attr` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `8) Calling static and instance methods` ) ).

    "Similar to accessing attributes, instance methods are called
    "using -> via a reference variable. Static methods are called
    "using => via the class name. When used within the class in which
    "it is declared, the static method can also be called without
    "class_name=>.... You might also see method calls with CALL
    "METHOD statements which are not used here. When methods are
    "called, the parameters must be specified within the parentheses.
    "If methods are within the class where they are called, a class
    "specification is not needed. In the example below, the methods
    "have no parameters defined, hence, there is no specification
    "within the parentheses. The methods just change the value of
    "a public static variable.

    "Instance methods
    DATA(ref8) = NEW zcl_demo_abap_objects( ).
    ref8->hallo_instance_method( ).

    out->write( data = string name = `string` ).
    out->write( |\n| ).

    "Static methods
    lcl_demo=>hallo_static_ext( ).

    out->write( data = lcl_demo=>string name = `lcl_demo=>string` ).
    out->write( |\n| ).

    "If methods are within the class where they are called,
    "the class name can be omitted.
    zcl_demo_abap_objects=>hallo_static_method( ).

    hallo_static_method( ).

    out->write( data = string name = `string` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `9) Calling methods: Examples` &&
                  ` with importing parameters` ) ).

    "The example shows method calls. The methods used have only one or
    "two importing parameters.
    "One importing parameter:
    "- Note that you export the values to the method (which has
    "  importing parameters), hence, the method call includes
    "  EXPORTING.
    "- If the method has only one importing parameter, you can omit the
    "  explicit assignment of the value to the parameter and just specify
    "  the value that you want to pass. The specification of EXPORTING
    "  can be omitted, too.
    "- Hence, the first three method calls do all the same.
    "Two importing parameters:
    "- All mandatory parameters must be specified.
    "- Also here, the specification of EXPORTING can be omitted.
    "- The last method includes an optional parameter. In this case, it
    "  is of type i. Hence, its value remains initial ('0') since it is
    "  not specified.
    "To keep the code lean, only static methods are covered.

    "Method with one importing parameter.
    lcl_demo=>powers_of_two( 4 ).

    out->write( data = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).
    out->write( |\n| ).

    lcl_demo=>powers_of_two( i_pow = 5 ).

    out->write( data = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).
    out->write( |\n| ).

    lcl_demo=>powers_of_two( EXPORTING i_pow = 6 ).

    out->write( data = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).
    out->write( |\n| ).

    "Method with two importing parameters
    lcl_demo=>addition( i_add1 = 1 i_add2 = 4 ).

    out->write( data = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).
    out->write( |\n| ).

    lcl_demo=>addition_optional( i_add_mand = 1 ).

    out->write( data = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `10) Calling methods: Examples ` &&
                  `with exporting parameters` ) ).

    "Note: The methods have exporting parameters defined in the signature,
    "hence, when calling the method, the ABAP word IMPORTING must be used to
    "address the values.
    "In the first method call below, the variable that holds the imported
    "value is declared inline. It receives the type automatically.
    "The second method below has two importing parameters and one exporting
    "parameter (which is actually the result of a calculation). If a method
    "has, for example, importing and exporting parameters but you do not
    "want to take the exporting parameters into your program, you can write
    "the method call as though the method had only importing parameters.
    "Likewise, if a method has a single obligatory importing parameter and
    "several optional parameters and you do not want to specify the optional
    "parameters, you can write the method call as if the method had only one
    "importing parameter.

    lcl_demo=>exporting_hallo( IMPORTING text = DATA(hallo) ).

    lcl_demo=>subtraction( EXPORTING i_sub1 = 10 i_sub2 = 7
                           IMPORTING e_sub_result = DATA(subtraction_result) ).

    out->write( data = hallo name = `hallo` ).
    out->write( |\n| ).
    out->write( data = subtraction_result name = `subtraction_result` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `11) Calling methods: Example with changing parameter` ) ).

    "Changing parameters define one or multiple parameters that can
    "be both imported and exported. They should be reserved for
    "changing an existing local variable and value.

    DATA num TYPE decfloat34 VALUE '144'.

    lcl_demo=>square_root( CHANGING i_sqr = num ).

    out->write( data = num name = `num` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `12) Calling methods: Examples with returning parameters` ) ).

    "Methods having a returning parameter are called functional methods.
    "Returning parameters are preferable to exporting parameters since they
    "not only make the call shorter, they also allow method chaining and
    "they do not require the use of temporary variables because they can be
    "used in conjunction with other statements.
    "Functional methods can be called directly from within various
    "expressions (e. g. logical expressions with IF) without temporarily
    "storing values.
    "The use of receiving parameters is only possible for standalone method
    "calls and not for functional method calls.
    "The code below also includes an example for method chaining. Here, the
    "global class cl_abap_random_int is used with which random integers can
    "be created.

    DATA(mult_result) = lcl_demo=>multiplication( i_mult1 = 4
                                                   i_mult2 = 5 ).

    out->write( data = mult_result name = `mult_result` ).
    out->write( |\n| ).

    "Comparing a method having exporting parameters doing the same.
    lcl_demo=>multiplication_exp_param( EXPORTING i_multa = 5
                                                  i_multb = 6
                                        IMPORTING e_mult_result = DATA(mult_res_exp) ).

    out->write( data = mult_res_exp name = `mult_res_exp` ).
    out->write( |\n| ).

    "Example with a logical expression
    IF lcl_demo=>multiplication( i_mult1 = 5 i_mult2 = 3 ) < 20.
      out->write( |The value is lower than 20.| ).
    ELSE.
      out->write( |The value is greater than 20.| ).
    ENDIF.

    out->write( |\n| ).

    "Receiving parameter
    lcl_demo=>multiplication( EXPORTING i_mult1 = 10
                                        i_mult2 = 11
                              RECEIVING r_mult_result = DATA(res_received) ).

    out->write( data = res_received name = `res_received` ).
    out->write( |\n| ).

    "Example for method chaining using a global class.
    DATA(random_no1) = cl_abap_random_int=>create( )->get_next( ).

    "Specifying the optional min and max importing parameters.
    DATA(random_no2) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                   min  = 1
                                                   max  = 10 )->get_next( ).

    "Using method chaining as above saves the extra declaration
    "of variables.
    DATA(ref_randnom_no) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                       min  = 20
                                                       max  = 30 ).

    DATA(random_no3) = ref_randnom_no->get_next( ).

    out->write( data = random_no1 name = `random_no1` ).
    out->write( |\n| ).
    out->write( data = random_no2 name = `random_no2` ).
    out->write( |\n| ).
    out->write( data = random_no3 name = `random_no3` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `13) Calling methods: Examples with error handling` ) ).

    "The examples show two method calls for a method that includes a
    "raising parameter. For this method, a class-based exception is
    "specified. The exception is raised for the second method
    "call. The third method call just gives
    "a rough idea on raising exceptions: The current time is checked
    "and if it is currently a certain time of the day, an exception
    "is raised.

    "Method with raising parameter (class-based exception)
    DATA(div_result1) = lcl_demo=>division( i_div1 = 5
                                            i_div2 = 2 ).

    IF lcl_demo=>string IS INITIAL.
      out->write( data = div_result1 name = `div_result1` ).
    ELSE.
      out->write( |Calculation error: { lcl_demo=>string }| ).
    ENDIF.

    out->write( |\n| ).

    DATA(div_result2) = lcl_demo=>division( i_div1 = 1 i_div2 = 0 ).
    IF lcl_demo=>string IS INITIAL.
      out->write( data = div_result2 name = `div_result2` ).
    ELSE.
      out->write( |Calculation error: { lcl_demo=>string }| ).
    ENDIF.

    out->write( |\n| ).

    "Method with RAISING addition (class-based exceptions)
    TRY.
        lcl_demo=>check_daytime(
          EXPORTING time = cl_abap_context_info=>get_system_time( )
          IMPORTING greetings = DATA(greets) ).
      CATCH cx_afternoon.
        DATA(subrc) = 11.
      CATCH cx_night.
        subrc = 33.
    ENDTRY.

    out->write( data = greets name = `greets` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `14) Constructors` ) ).

    "Constructors cannot be explicitly called like other methods.
    "The examples demonstrate instance and static constructors.
    "The first three method calls show instance constructors. The
    "implementation of the instance constructor includes several things.
    "Among them, getting a time stamp and a uuid, counting the number of
    "instances, updating a string, and carrying out a division. The output
    "also shows the effect of calling the static constructor. Check the
    "string in the field stat_text and see that it has not changed compared
    "to the instance attribute in_text. The third method call shows that an
    "instance cannot be created if an error occurs in the instance
    "constructor method (initial reference variable).
    "The effect of the static constructor is demonstrated by the fourth
    "method call. The value of the variable stat_text has not changed
    "compared to the other method calls before when outputting it. The
    "value of variable stat_number only changes when calling this particular
    "method (it explicitly changes the value of the variable).

    "Instance constructor
    TRY.
        DATA(ref14a) = NEW lcl_constructors( num1 = 10 num2 = 5 ).
      CATCH cx_sy_zerodivide INTO DATA(error).
        out->write( data = error->get_text( ) name = `error->get_text( )` ).
    ENDTRY.

    out->write( data = ref14a name = `ref14a` ).
    out->write( |\n| ).

    TRY.
        DATA(ref14b) = NEW lcl_constructors( num1 = 18 num2 = 6 ).
      CATCH cx_sy_zerodivide INTO error.
        out->write( data = error->get_text( ) name = `error->get_text( )` ).
    ENDTRY.

    out->write( data = ref14b name = `ref14b` ).
    out->write( |\n| ).

    TRY.
        DATA(ref14c) = NEW lcl_constructors( num1 = 1 num2 = 0 ).
      CATCH cx_sy_zerodivide INTO error.
        out->write( |Error with ref14c: { error->get_text( ) }| ).
    ENDTRY.

    out->write( data = ref14c name = `ref14c` ).
    out->write( |\n| ).

    "Static constructor
    lcl_constructors=>add_1( ).

    out->write( data = lcl_constructors=>stat_text name = `lcl_constructors=>stat_text` ).
    out->write( |\n| ).
    out->write( data = lcl_constructors=>stat_number name = `lcl_constructors=>stat_number` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `15) Parameters: Generic types` ) ).

    "The use of generic types in method signatures is particularly relevant
    "for dynamic programming. The code shows various examples of parameters
    "typed with DATA and ANY TABLE. In the method implementation, all values
    "of the variables are stored in a data reference variable that is
    "displayed.

    DATA(int) = 4.

    lcl_demo=>generic_data( EXPORTING i_data = int ).

    out->write( data = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).
    out->write( |\n| ).

    DATA strtab TYPE TABLE OF string.

    strtab = VALUE #( ( `I'm a ` ) ( `string table.` ) ).

    lcl_demo=>generic_data( EXPORTING i_data = strtab ).

    out->write( data = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).
    out->write( |\n| ).

    DATA int_tab TYPE TABLE OF i.

    int_tab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).

    DATA c_tab TYPE TABLE OF c.

    c_tab = VALUE #( ( 'a' ) ( 'b' ) ( 'c' ) ).

    lcl_demo=>generic_tab( EXPORTING i_anytab = int_tab ).

    out->write( data = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).
    out->write( |\n| ).

    lcl_demo=>generic_tab( EXPORTING i_anytab = c_tab ).

    out->write( data = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).
    out->write( |\n| ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `16) Inheritance: Method redefinition` ) ).

    "The example demonstrates inheritance in a very rudimentary way.
    "Class 1 is the superclass of class 2 that inherits from class 1. The
    "same is true for class 2 and class 3. Class 3 is defined with the
    "addition FINAL, so another class cannot inherit from this one. All
    "classes implement or redefine respectively a certain method. In this
    "case, it is a method that adapts a string. The redefined methods
    "access the method of the superclass by specifying super->....

    "Class 1
    DATA(ref_inh1) = NEW lcl_class1( ).

    DATA(first_string) = ref_inh1->get_string( ).

    "Class 2
    DATA(ref_inh2) = NEW lcl_class2a( ).

    DATA(second_string) = ref_inh2->get_string( ).

    "Class 3
    DATA(ref_inh3) = NEW lcl_class3a( ).

    DATA(third_string) = ref_inh3->get_string( ).

    out->write( data = first_string name = `first_string` ).
    out->write( |\n| ).
    out->write( data = second_string name = `second_string` ).
    out->write( |\n| ).
    out->write( data = third_string name = `third_string` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `17) Polymorphism and Casting` ) ).

    "The ref_pol1 object reference variable is created and points to class
    "lcl_class1, i. e. the superclass. The ref_pol2 object reference
    "variable points to class lcl_class2, i. e. the subclass of lcl_class1.
    "At this stage, both the static type and dynamic type of the object
    "reference variable ref_pol1 are the same. Then, the object reference
    "variable with the type of the subclass is assigned to this reference
    "variable, i. e. an upcast is triggered. The output shows the outcome of
    "a method call using this object reference variable before and after the
    "upcast. The first method call before the upcast demonstrates that the
    "method from the superclass is called. The second method call after the
    "upcast shows the polymorphism concept since the method call happens via
    "the same object reference variable as before. However, at this stage,
    "the reference variable points to another object, i. e. the dynamic type
    "of the reference variable is now lcl_class2. Hence, the redefined
    "method in the subclass having the same name as the method in the
    "superclass is called. It is also shown that the casting might be done
    "when creating the object.

    DATA(ref_pol1) = NEW lcl_class1( ).

    DATA(ref_pol2) = NEW lcl_class2a( ).

    DATA(str1) = ref_pol1->get_string( ).

    "Upcast
    ref_pol1 = ref_pol2.

    DATA(str2) = ref_pol1->get_string( ).

    "The casting might be done when creating the object
    DATA ref_pol_super TYPE REF TO lcl_class1.

    ref_pol_super = NEW lcl_class2a( ).

    out->write( data = str1 name = `str1` ).
    out->write( |\n| ).
    out->write( data = str2 name = `str2` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18a) Downcast` ) ).

    "In this example, the possibility of downcasts are checked, i. e. the
    "assignment of a more generic object reference variable to a specific
    "one. At the beginning, an internal table is just created for displaying
    "purposes. The classes lcl_class2a and lcl_class2b are both subclasses
    "of lcl_class1. Various objects with reference to these subclasses are
    "created providing a "name" for the objects. Here, some of the objects
    "are created separately, some are directly declared when adding them to
    "the table. An internal table with reference to the superclass
    "lcl_class1 is created. All of the objects are then inserted into this
    "internal table. In doing so, an implicit upcast takes place here (it is
    "basically the assignment of an object reference variable pointing to
    "the subclass to a variable pointing to a superclass). As a next step,
    "all objects in the internal table are looped across. In each iteration,
    "checks are implemented to find out if downcasts are possible. First, a
    "check is implemented using a TRY ... ENDTRY block. This statement checks if
    "an object reference variable of lcl_class1 can be cast down to one of
    "lcl_class2a. If it is possible, a message is written into a dedicated
    "field of the display table. Plus, a method is called that is only
    "available in lcl_class2a. Note: The method just returns a random
    "number. The return value is written to the display table, too. If the
    "downcast is not possible, a message is written to the table, too. The
    "second check is implemented in a similar way, however, the check is
    "implemented using the predicate expression IS INSTANCE OF. Using this
    "syntax, the code gets leaner while achieving the same as using a TRY ...
    "ENDTRY block without handling the cx_sy_move_cast_error error
    "separately.

    "Creating an internal table for displaying purposes
    TYPES: BEGIN OF dc_check_struc,
             object_name TYPE string,
             a_check     TYPE string,
             b_check     TYPE string,
             a_number    TYPE i,
             b_number    TYPE i,
           END OF dc_check_struc.

    DATA dc_check TYPE TABLE OF dc_check_struc.

    "Creating internal table to hold various objects
    DATA: obj_itab TYPE TABLE OF REF TO lcl_class1.

    "Creating various objects ...
    DATA(oref1) = NEW lcl_class2a( `Object A1` ).
    DATA(oref2) = NEW lcl_class2a( `Object A2` ).

    "... and adding them to the internal table.
    "Some of the objects are directly declared when assigning them.
    obj_itab = VALUE #( ( oref1 )
                        ( oref2 )
                        ( NEW lcl_class2a( `Object A3` ) )
                        ( NEW lcl_class2b( `Object B1` ) )
                        ( NEW lcl_class2b( `Object B2` ) )
                        ( NEW lcl_class2b( `Object B3` ) ) ).

    "Looping across all objects in the internal table.
    LOOP AT obj_itab ASSIGNING FIELD-SYMBOL(<fs1>).
      "Adding an entry for the display table.
      "Here, only the name of the object.
      dc_check = VALUE #( BASE dc_check
          ( object_name = <fs1>->get_obj_name( ) ) ).

      "First check if downcasts are possible using TRY ENDTRY block.
      TRY.
          "lcl_class1 to be cast down to lcl_class2a
          DATA(o_dc_a) = CAST lcl_class2a( <fs1> ).

          "If downcast works, write a message into a table field.
          "Plus, return the number received via the method available
          "in lcl_class2a only.
          dc_check[ object_name = <fs1>->get_obj_name( ) ]-a_check =
            `Downcast works.`.
          dc_check[ object_name = <fs1>->get_obj_name( ) ]-a_number =
             o_dc_a->get_number_2a( ).

        CATCH cx_sy_move_cast_error.
          "If downcast does not work, write a message into a table field.
          dc_check[ object_name = <fs1>->get_obj_name( ) ]-a_check =
            `Downcast does not work.`.
      ENDTRY.

      "Second check if downcasts are possible using IS INSTANCE OF
      IF <fs1> IS INSTANCE OF lcl_class2b.
        "If downcast works, write a message into a table field.
        "Plus, return the number received via the method available
        "in lcl_class2b only.
        DATA(o_dc_b) = CAST lcl_class2b( <fs1> ).
        dc_check[ object_name = <fs1>->get_obj_name( ) ]-b_check =
          `Downcast works.`.
        dc_check[ object_name = <fs1>->get_obj_name( ) ]-b_number =
          o_dc_b->get_number_2b( ).
      ELSE.
        "If downcast does not work, write a message into a table field.
        dc_check[ object_name = <fs1>->get_obj_name( ) ]-b_check =
          `Downcast does not work.`.
      ENDIF.
    ENDLOOP.

    out->write( data = dc_check name = `dc_check` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `18b) Excursion RTTI: Downcasts and Method Chaining` ) ).

    "Downcasts particularly play, for example, a role in the context of
    "retrieving type information using RTTI. Method chaining is handy
    "because it reduces the lines of code in this case.
    "The example contains the retrieval of type information for a
    "structure (structure components).
    "Due to the method chaining in the second example, the three
    "statements in the first example are reduced to one statement.

    DATA struct4cast TYPE zdemo_abap_carr.

    DATA(rtti_a) = cl_abap_typedescr=>describe_by_data( struct4cast ).
    DATA(rtti_b) = CAST cl_abap_structdescr( rtti_a ).
    DATA(rtti_c) = rtti_b->components.

    out->write( data = rtti_c name = `rtti_c` ).
    out->write( |\n| ).

    DATA(rtti_d) = CAST cl_abap_structdescr(
      cl_abap_typedescr=>describe_by_data( struct4cast )
          )->components.

    out->write( data = rtti_d name = `rtti_d` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `19) Interfaces` ) ).

    "Addressing instance interface components using interface reference variable
    DATA ref_if1 TYPE REF TO zdemo_abap_objects_interface.
    DATA ref_if2 TYPE REF TO zdemo_abap_objects_interface.

    "Object reference variable for a class implementing the interface
    DATA ref_cl1 TYPE REF TO zcl_demo_abap_objects.

    "An interface variable can contain references to objects of classes
    "that implement the corresponding interface.
    "Creating an object
    ref_cl1 = NEW zcl_demo_abap_objects( ).

    "Assigning the object reference to the interface object variable
    ref_if1 = ref_cl1.

    "This can also be done directly, i. e. directly creating an object to
    "which the interface reference variable points
    ref_if2 = NEW zcl_demo_abap_objects( ).

    "Instance method via the interface reference variable (i_ref->meth( ))
    DATA(inst_intf_meth_via_iref1) = ref_if1->double( 5 ).
    DATA(inst_intf_meth_via_iref2) = ref_if2->double( 10 ).

    "Instance attribute via the interface reference variable (i_ref->attr)
    DATA(inst_intf_attr_via_iref) = ref_if1->in_str.

    "Addressing instance components using the class reference variable
    "is also possible but it's not the recommended way
    "c_ref->intf~meth
    DATA(inst_intf_meth_via_cref) = ref_cl1->zdemo_abap_objects_interface~double( 20 ).

    "c_ref->intf~attr
    DATA(inst_intf_attr_via_cref) = ref_cl1->zdemo_abap_objects_interface~in_str.

    "Addressing static interface components
    "Static methods
    "class=>intf~meth( )
    DATA(stat_intf_meth1) = zcl_demo_abap_objects=>zdemo_abap_objects_interface~halve( 10 ).

    "Since we are in this very class here, the class name can be dropped.
    DATA(stat_intf_meth2) = zdemo_abap_objects_interface~halve( 100 ).

    "Just for the record: Static methods can be called via reference variables, too.
    DATA(stat_intf_meth3) = ref_if2->halve( 50 ).
    DATA(stat_intf_meth4) = ref_cl1->zdemo_abap_objects_interface~halve( 70 ).

    "Static attributes
    "class=>intf~attr
    DATA(stat_intf_attr1) = zcl_demo_abap_objects=>zdemo_abap_objects_interface~stat_str.
    DATA(stat_intf_attr2) = zdemo_abap_objects_interface~stat_str.
    "Accessing static attribute via reference variable
    DATA(stat_intf_attr3) = ref_if2->stat_str.

    "Constants
    "Can be accessed directly using this pattern: intf=>const
    DATA(intf_const1) = zdemo_abap_objects_interface=>const_intf.
    "Other options are possible
    DATA(intf_const2) = zcl_demo_abap_objects=>zdemo_abap_objects_interface~const_intf.
    DATA(intf_const3) = ref_if2->const_intf.

    out->write( data = inst_intf_attr_via_iref name = `inst_intf_attr_via_iref` ).
    out->write( |\n| ).
    out->write( data = inst_intf_meth_via_iref1 name = `inst_intf_meth_via_iref1` ).
    out->write( |\n| ).
    out->write( data = inst_intf_meth_via_iref2 name = `inst_intf_meth_via_iref2` ).
    out->write( |\n| ).
    out->write( data = inst_intf_attr_via_cref name = `inst_intf_attr_via_cref` ).
    out->write( |\n| ).
    out->write( data = inst_intf_meth_via_cref name = `inst_intf_meth_via_cref` ).
    out->write( |\n| ).

    out->write( data = stat_intf_attr1 name = `stat_intf_attr1` ).
    out->write( |\n| ).
    out->write( data = stat_intf_meth1 name = `stat_intf_meth1` ).
    out->write( |\n| ).
    out->write( data = stat_intf_meth2 name = `stat_intf_meth2` ).
    out->write( |\n| ).
    out->write( data = stat_intf_attr2 name = `stat_intf_attr2` ).
    out->write( |\n| ).
    out->write( data = stat_intf_meth3 name = `stat_intf_meth3` ).
    out->write( |\n| ).
    out->write( data = stat_intf_meth4 name = `stat_intf_meth4` ).

    out->write( data = intf_const1 name = `intf_const1` ).
    out->write( |\n| ).
    out->write( data = intf_const2 name = `intf_const2` ).
    out->write( |\n| ).
    out->write( data = intf_const3 name = `intf_const3` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `20) Singleton` ) ).

    "The demonstrates an implementation of the singleton design pattern.
    "A static method allows access to the only object of the class.
    "An instance is tried to be created three times. The method is
    "implemented in a way that prevents the creation of more than one
    "instance of the class. Hence, the result of all three method call shows
    "the same values (the time stamp that is set initially and the number of
    "instances that is always one).

    DATA: obj1 TYPE REF TO lcl_singleton,
          obj2 LIKE obj1.

    "Getting an instance of the class lcl_singleton
    obj1 = lcl_singleton=>get_instance( ).

    "Setting a time stamp
    obj1->set_timestamp( ).

    "Getting time stamp and the overall number of instances of the class
    DATA(timestamp) = obj1->get_timestamp( ).
    DATA(no_of_instances) = lcl_singleton=>no_of_instances.

    out->write( data = timestamp name = `timestamp` ).
    out->write( |\n| ).
    out->write( data = no_of_instances name = `no_of_instances` ).
    out->write( |\n| ).

    "Trying to get another instance
    obj2 = lcl_singleton=>get_instance( ).

    "Getting time stamp and the overall number of instances of the class
    timestamp =  obj2->get_timestamp( ).
    no_of_instances = lcl_singleton=>no_of_instances.

    out->write( data = timestamp name = `timestamp` ).
    out->write( |\n| ).
    out->write( data = no_of_instances name = `no_of_instances` ).
    out->write( |\n| ).

    "Trying to get another instance
    DATA(obj3) = lcl_singleton=>get_instance( ).

    "Getting time stamp and the overall number of instances of the class
    timestamp =  obj3->get_timestamp( ).
    no_of_instances = lcl_singleton=>no_of_instances.

    out->write( data = timestamp name = `timestamp` ).
    out->write( |\n| ).
    out->write( data = no_of_instances name = `no_of_instances` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `21) Factory method in an abstract class` ) ).

    "The example demonstrates a factory method in an abstract class. An
    "instance is tried to be created two times. The factory method is
    "implemented in a way that prevents the creation of an instance if a
    "certain condition is not met. In this simplistic example, the creation
    "is only allowed if the value '1' is passed to the factory method. The
    "second instance creation fails on purpose.

    out->write( `First try: inst_1` ).
    out->write( |\n| ).
    out->write( |\n| ).

    TRY.
        DATA(inst_1) = lcl_abstract=>factory_method( 1 ).
        DATA(str_1) = inst_1->return_string( `inst_1` ).
        out->write( data = str_1 name = `str_1` ).
      CATCH cx_sy_ref_is_initial INTO DATA(error1).
        out->write( |Error message: { error1->get_text( ) }| ).
    ENDTRY.

    out->write( |\n| ).
    out->write( data = lcl_abstract=>message name = `lcl_abstract=>message` ).
    out->write( |\n| ).

    out->write( `Second try: inst_2` ).
    out->write( |\n| ).

    TRY.
        DATA(inst_2) = lcl_abstract=>factory_method( 2 ).
        DATA(str_2) = inst_2->return_string( `inst_2` ).
        out->write( data = str_2 name = `str_2` ).
      CATCH cx_sy_ref_is_initial INTO DATA(error2).
        out->write( |Error message: { error2->get_text( ) }| ).
    ENDTRY.

    out->write( |\n| ).
    out->write( |\n| ).
    out->write( data = lcl_abstract=>message name = `lcl_abstract=>message` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `22) Friendship: Accessing components of friends` ) ).

    "Classes can grant friendship to other classes and interfaces to enable
    "the access to protected and private components. However, the friendship
    "is not reciprocal. If class a grants friendship to class b, class b
    "must also explicitly grant friendship to class a if the components
    "should be made accessible also the other way round. In this simple example,
    "this class has a class declared as a friend. To visualize the concept,
    "strings are available in the public, protected and private section
    "here. The befriended class can access the strings not only in the public section
    "but of all other sections, too. The strings are stored in a string table. The content
    "of this string table is retrieved from the befriended class via a method.

    DATA(string_table) = zcl_demo_abap_objects_friend=>get_strings( ).

    out->write( data = string_table name = `string_table` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `23) Self-reference me` ) ).

    "This example demonstrates the use of the self-reference 'me' in an
    "instance method. The method implementation includes a variable of type
    "string that has the same name as a variable that is declared in the
    "public section of the class. The method has two exporting parameters to
    "include both the value of the local variable and the value of the
    "equally named variable from the public section. The latter one is
    "referred to using the self-reference me within the method implementation.

    DATA(ref_var) = NEW zcl_demo_abap_objects( ).

    ref_var->me_ref_meth( IMPORTING e1 = DATA(string_without_me)
                                    e2 = DATA(string_with_me) ).

    out->write( data = string_without_me name = `string_without_me` ).
    out->write( |\n| ).
    out->write( data = string_with_me name = `string_with_me` ).

**********************************************************************

    out->write( zcl_demo_abap_aux=>heading( `24) Events` ) ).

    "The example covers the use of instance events. Event handler methods
    "are registered for a particular instance. Events are raised in a method
    "based on the daytime. Various event handler methods are implemented
    "which return a string.

    DATA(ref_events) = NEW lcl_events( ).

    "Registering event handler methods.
    SET HANDLER: ref_events->morning_greets
                 ref_events->afternoon_greets
                 ref_events->evening_greets
                 ref_events->night_greets
                 FOR ref_events.

    "Calling method that raises an event
    ref_events->greetings( ).

    out->write( data = ref_events->greets name = `ref_events->greets` ).

  ENDMETHOD.


  METHOD me_ref_meth.
    DATA another_string TYPE string VALUE `I'm a local string.`.
    "e1 gets assigned the local string.
    e1 = another_string.
    "e2 gets assigned the variable from the public section.
    e2 = me->another_string.
  ENDMETHOD.


  METHOD triple.
    zdemo_abap_objects_interface~in_str = `The result of calling triple (i. e. zdemo_abap_objects_interface~triple) is: `.
    r_triple = i_op * 3.
  ENDMETHOD.


  METHOD zdemo_abap_objects_interface~double.
    zdemo_abap_objects_interface~in_str = `The result of calling zdemo_abap_objects_interface~double is: `.
    r_double = i_op * 2.
  ENDMETHOD.


  METHOD zdemo_abap_objects_interface~halve.
    zdemo_abap_objects_interface~stat_str = `The result of calling zdemo_abap_objects_interface~halve is: `.
    r_halve = i_op / 2.
  ENDMETHOD.
ENDCLASS.
