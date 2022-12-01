***********************************************************************
*
*           ABAP cheat sheet: ABAP object orientation
*
* -------------------------- PURPOSE ----------------------------------
* - Example to demonstrate various syntactical options and concepts related
*   to ABAP object orientation as outlined in the related ABAP cheat sheet.
* - The CCIMP include (local types tab in ADT) includes multiple local
*   classes to support the example.
* - A separate global interface is used to demonstrate working with
*   interfaces: zdemo_abap_objects_interface
* - Another global class is used to demonstrate the concept of friendship:
*   zcl_demo_abap_objects_friend
* - Topics covered: working with objects and components, method redefinition
*   in inheritance, working with interfaces, upcast and downcast, concepts
*   like factory methods, singleton and abstract classes, using events
*
* ----------------------- GETTING STARTED -----------------------------
* - Open the class with the ABAP Development Tools (ADT).
* - Choose F9 to run the class.
* - Check the console output.
* - To understand the context and the ABAP syntax used, check the notes
*   included in the class as comments or refer to the respective topic
*   in the ABAP Keyword Documentation.
* - Due to the amount of output in the console, the examples include
*   numbers (e. g. 1) ..., 2) ..., 3) ...) for the individual example
*   sections. Plus, the variable name is displayed in most cases. Hence,
*   to easier and faster find the relevant output in the console, just
*   search in the console for the number/variable name (STRG+F in the
*   console) or use the debugger.
*
* ----------------------------- NOTE -----------------------------------
* The code presented in this class is only meant for supporting the ABAP
* cheat sheets. It is not intended for direct use in a
* production system environment. The code examples in the ABAP cheat
* sheets are primarily intended to provide a better explanation and
* visualization of the syntax and semantics of ABAP statements and not to
* solve concrete programming tasks. For production application programs,
* a dedicated solution should therefore always be worked out for each
* individual case. There is no guarantee for either the correctness or
* the completeness of the code. In addition, there is no legal
* responsibility or liability for possible errors or their consequences
* which occur through the use of the example code.
*
***********************************************************************
"! <p class="shorttext synchronized">ABAP cheat sheet: ABAP Object Orientation</p>
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



CLASS ZCL_DEMO_ABAP_OBJECTS IMPLEMENTATION.


  METHOD hallo_instance_method.
    string = |Hallo { sy-uname }. | &&
             |I'm an instance method of class zcl_demo_abap_objects.|.
  ENDMETHOD.


  METHOD hallo_static_method.
    string = |Hallo { sy-uname }. | &&
             |I'm a static method of class zcl_demo_abap_objects.|.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA(output) = NEW zcl_demo_abap_display( out ).

    output->display( `Demo: ABAP Object Orientation` ).
    output->display( `Working with objects and components` ).
    output->display( `1) Declaring reference variables` ).

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
      output->display( `The declared reference variables are initial.` ).
    ELSE.
      output->display( `One or more of the declared reference ` &&
                    `variables are not initial.` ).
    ENDIF.

    output->next_section( `2) Creating objects` ).

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

    "Alternative syntax.
    CREATE OBJECT ref2a.


    IF ref2a IS INSTANCE OF local_class
    AND ref2b IS INSTANCE OF local_class.
      output->display( `ref2a and ref2b point to instances ` &&
                    `of the class local_class.` ).
    ELSE.
      output->display( `One or more of the reference variables ` &&
      `do not point to instances of the class local_class.` ).
    ENDIF.


    output->next_section( `3) Assigning object references` ).

    "Without an assignment, the reference variable is empty.
    "To assign or copy reference variable, use the assignment operator
    "=. In the example below, both reference variables have the same
    "type.

    DATA: ref3a TYPE REF TO local_class,
          ref3b TYPE REF TO local_class.

    ref3a = ref3b.

    IF ref3a = ref3b.
      output->display( `ref3b has been assigned to ref3a.` ).
    ELSE.
      output->display( `ref3b has not been assigned to ref3a.` ).
    ENDIF.

    output->next_section( `4) Overwriting object references` ).

    "An object reference is overwritten when a new object is created
    "with a reference variable already pointing to an instance.
    "The class is implemented in a way that the number of instances
    "that are created is counted. In this example, the output is
    "just to visualize that the first ref4 is indeed overwritten.

    DATA ref4 TYPE REF TO local_class.

    ref4 = NEW #( ).

    output->display( input = ref4->no_of_instances name = `ref4->no_of_instances` ).

    ref4 = NEW #( ).

    output->display( input = ref4->no_of_instances name = `ref4->no_of_instances` ).

    output->next_section( `5) Keeping references variables in internal tables` ).

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

    output->display( input = itab5 name = `itab5` ).

    output->next_section( `6) Clearing object references` ).

    "Use CLEAR statements to explicitly clear a reference variable.
    "Since objects use up space in the memory, they should be cleared
    "if they are no longer needed. Actually, the garbage collector
    "takes over this task automatically, i. e. all objects without any
    "reference are cleared and the memory space is released.

    DATA ref6 TYPE REF TO local_class.

    ref6 = NEW #( ).

    CLEAR ref6.

    IF ref6 IS INITIAL.
      output->display( `ref6 is initial.` ).
    ELSE.
      output->display( `ref6 is not initial.` ).
    ENDIF.

    output->next_section( `7) Accessing and using attributes` ).

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

    output->display( input = obj_instance_attr name = `obj_instance_attr` ).
    output->display( input = obj_static_attr_obj name = `obj_static_attr_obj` ).
    output->display( input = class_static_attr name = `class_static_attr` ).

    output->next_section( `8) Calling static and instance methods` ).

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

    output->display( input = string name = `string` ).

    "Static methods
    lcl_demo=>hallo_static_ext( ).

    output->display( input = lcl_demo=>string name = `lcl_demo=>string` ).

    "If methods are within the class where they are called,
    "the class name can be omitted.
    zcl_demo_abap_objects=>hallo_static_method( ).

    hallo_static_method( ).

    output->display( input = string name = `string` ).

    output->next_section( `9) Calling methods: Examples` &&
                  ` with importing parameters` ).

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

    output->display( input = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).

    lcl_demo=>powers_of_two( i_pow = 5 ).

    output->display( input = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).

    lcl_demo=>powers_of_two( EXPORTING i_pow = 6 ).

    output->display( input = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).

    "Method with two importing parameters
    lcl_demo=>addition( i_add1 = 1 i_add2 = 4 ).

    output->display( input = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).

    lcl_demo=>addition_optional( i_add_mand = 1 ).

    output->display( input = lcl_demo=>calc_result name = `lcl_demo=>calc_result` ).

    output->next_section( `10) Calling methods: Examples ` &&
                  `with exporting parameters` ).

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

    output->display( input = hallo name = `hallo` ).
    output->display( input = subtraction_result name = `subtraction_result` ).

    output->next_section( `11) Calling methods: Example with changing parameter` ).

    "Changing parameters define one or multiple parameters that can
    "be both imported and exported. They should be reserved for
    "changing an existing local variable and value.

    DATA num TYPE decfloat34 VALUE '144'.

    lcl_demo=>square_root( CHANGING i_sqr = num ).

    output->display( input = num name = `num` ).

    output->next_section( `12) Calling methods: Examples with returning parameters` ).

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

    output->display( input = mult_result name = `mult_result` ).

    "Comparing a method having exporting parameters doing the same.
    lcl_demo=>multiplication_exp_param( EXPORTING i_multa = 5
                                                  i_multb = 6
                                        IMPORTING e_mult_result = DATA(mult_res_exp) ).

    output->display( input = mult_res_exp name = `mult_res_exp` ).

    "Example with a logical expression
    IF lcl_demo=>multiplication( i_mult1 = 5 i_mult2 = 3 ) < 20.
      output->display( |The value is lower than 20.| ).
    ELSE.
      output->display( |The value is greater than 20.| ).
    ENDIF.

    "Receiving parameter
    lcl_demo=>multiplication( EXPORTING i_mult1 = 10
                                        i_mult2 = 11
                              RECEIVING r_mult_result = DATA(res_received) ).

    output->display( input = res_received name = `res_received` ).

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

    output->display( input = random_no1 name = `random_no1` ).
    output->display( input = random_no2 name = `random_no2` ).
    output->display( input = random_no3 name = `random_no3` ).

    output->next_section( `13) Calling methods: Examples with error handling` ).

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
      output->display( input = div_result1 name = `div_result1` ).
    ELSE.
      output->display( |Calculation error: { lcl_demo=>string }| ).
    ENDIF.

    DATA(div_result2) = lcl_demo=>division( i_div1 = 1 i_div2 = 0 ).
    IF lcl_demo=>string IS INITIAL.
      output->display( input = div_result2 name = `div_result2` ).
    ELSE.
      output->display( |Calculation error: { lcl_demo=>string }| ).
    ENDIF.

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

    output->display( input = greets name = `greets` ).

    output->next_section( `14) Constructors` ).

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
        output->display( input = error->get_text( ) name = `error->get_text( )` ).
    ENDTRY.

    output->display( input = ref14a name = `ref14a` ).

    TRY.
        DATA(ref14b) = NEW lcl_constructors( num1 = 18 num2 = 6 ).
      CATCH cx_sy_zerodivide INTO error.
        output->display( input = error->get_text( ) name = `error->get_text( )` ).
    ENDTRY.

    output->display( input = ref14b name = `ref14b` ).

    TRY.
        DATA(ref14c) = NEW lcl_constructors( num1 = 1 num2 = 0 ).
      CATCH cx_sy_zerodivide INTO error.
        output->display( |Error with ref14c: { error->get_text( ) }| ).
    ENDTRY.

    output->display( input = ref14c name = `ref14c` ).

    "Static constructor
    lcl_constructors=>add_1( ).

    output->display( input = lcl_constructors=>stat_text name = `lcl_constructors=>stat_text` ).
    output->display( input = lcl_constructors=>stat_number name = `lcl_constructors=>stat_number` ).

    output->next_section( `15) Parameters: Generic types` ).

    "The use of generic types in method signatures is particularly relevant
    "for dynamic programming. The code shows various examples of parameters
    "typed with DATA and ANY TABLE. In the method implementation, all values
    "of the variables are stored in a data reference variable that is
    "displayed.

    DATA(int) = 4.

    lcl_demo=>generic_data( EXPORTING i_data = int ).

    output->display( input = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).

    DATA strtab TYPE TABLE OF string.

    strtab = VALUE #( ( `I'm a ` ) ( `string table.` ) ).

    lcl_demo=>generic_data( EXPORTING i_data = strtab ).

    output->display( input = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).

    DATA int_tab TYPE TABLE OF i.

    int_tab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).

    DATA c_tab TYPE TABLE OF c.

    c_tab = VALUE #( ( 'a' ) ( 'b' ) ( 'c' ) ).

    lcl_demo=>generic_tab( EXPORTING i_anytab = int_tab ).

    output->display( input = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).

    lcl_demo=>generic_tab( EXPORTING i_anytab = c_tab ).

    output->display( input = lcl_demo=>some_data->* name = `lcl_demo=>some_data->*` ).

    output->next_section( `16) Inheritance: Method redefinition` ).

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

    output->display( input = first_string name = `first_string` ).
    output->display( input = second_string name = `second_string` ).
    output->display( input = third_string name = `third_string` ).

    output->next_section( `17) Polymorphism and Casting` ).

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

    output->display( input = str1 name = `str1` ).
    output->display( input = str2 name = `str2` ).

    output->next_section( `18) Downcast` ).

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

    output->display( input = dc_check name = `dc_check` ).

    output->next_section( `19) Working with interfaces` ).

    "The code demonstrates multiple things when working with interfaces.
    "1. Accessing static methods and attributes using the interface
    "component selector ~.
    "2. Accessing instance methods and attributes via class references.
    "3. Accessing instance methods and attributes via class references and
    "using an alias name.
    "4. Accessing instance methods and attributes via interface references.
    "In this case, the object component selector is used to access the
    "components without the interface name. Before making use of interface
    "references, an interface reference variable must be created. Interfaces
    "cannot be instantiated, i. e. an object cannot be created, however,
    "interface references can point to the objects of any class that
    "includes the interface so that interface components (and only them) can
    "be accessed via the variable. The assignment of an object reference to
    "an interface references called upcast (or widening cast).
    "The other way round, i. e. the assignment of an interface reference to
    "an object reference, is also possible. This concept is called downcast
    "(or narrowing cast). However, this assignment can be problematic since
    "a successful assignment is dependent on whether the object the
    "interface reference points to is actually an object of the implementing
    "class. If this is not the case, a runtime error occurs. A downcast can
    "be done using the casting operator CAST. In older code, you might
    "stumble on the operator ?= for downcasting. To prevent a runtime
    "error, you can implement a check using the statement IS INSTANCE OF.

    "Accessing static method and attribute
    DATA(res_halve) = zdemo_abap_objects_interface~halve( 10 ).

    DATA(intro_static) = zdemo_abap_objects_interface~stat_str.

    "Access instance method and attribute ...
    "... via class reference.
    DATA(ref_cl) = NEW zcl_demo_abap_objects( ).

    DATA(res_double1) = ref_cl->zdemo_abap_objects_interface~double( 5 ).

    DATA(intro_inst1) = ref_cl->zdemo_abap_objects_interface~in_str.

    "Using alias name
    DATA(res_triple) = ref_cl->triple( 100 ).

    DATA(intro_inst4) = ref_cl->zdemo_abap_objects_interface~in_str.

    "... via interface reference.
    DATA ref_if2 TYPE REF TO zdemo_abap_objects_interface.

    DATA ref_cl2 TYPE REF TO zcl_demo_abap_objects.

    "Upcast (1): Convert object references to an interface reference
    ref_if2 = NEW zcl_demo_abap_objects( ).

    DATA(res_double2) = ref_if2->double( 10 ).

    DATA(intro_inst2) = ref_if2->in_str.

    ref_cl2 = NEW #( ).

    "Upcast (2)
    ref_if2 = ref_cl2.

    DATA(res_upcast) = ref_if2->double( 50 ).

    DATA(intro_inst_up) = ref_if2->in_str.

    "Downcast
    DATA(ref_cl_down) = NEW zcl_demo_abap_objects( ).

    IF ref_if2 IS INSTANCE OF zcl_demo_abap_objects.
      ref_cl_down = CAST #( ref_if2 ).

      "Old syntax:
      "ref_if2 ?=  ref_cl_down .

      DATA(res_downcast) = ref_cl_down->zdemo_abap_objects_interface~double( 100 ).

      DATA(intro_inst_down) = ref_cl_down->zdemo_abap_objects_interface~in_str.
    ELSE.
      output->display( `Downcast not possible.` ).
    ENDIF.

    output->display( input = intro_static name = `intro_static` ).
    output->display( input = res_halve name = `res_halve` ).
    output->display( input = intro_inst1 name = `intro_inst1` ).
    output->display( input = res_double1 name = `res_double1` ).
    output->display( input = intro_inst4 name = `intro_inst4` ).
    output->display( input = res_triple name = `res_triple` ).
    output->display( input = intro_inst2 name = `intro_inst2` ).
    output->display( input = res_double2 name = `res_double2` ).
    output->display( input = intro_inst_up name = `intro_inst_up` ).
    output->display( input = res_upcast name = `res_upcast` ).
    output->display( input = intro_inst_down name = `intro_inst_down` ).
    output->display( input = res_downcast name = `res_downcast` ).

    output->next_section( `20) Using a factory method in a singleton class` ).

    "The example demonstrates a factory method in a singleton class. An
    "instance is tried to be created three times. The factory method is
    "implemented in a way that prevents the creation of more than one
    "instance of the class. Hence, the result of all three method call shows
    "the same values (the time stamp that is set initially and the number of
    "instances that is always one).

    DATA: obj1 TYPE REF TO lcl_singleton,
          obj2 LIKE obj1.

    "Getting an instance of the class lcl_singleton
    obj1 = lcl_singleton=>factory_method( ).

    "Setting a time stamp
    obj1->set_timestamp( ).

    "Getting time stamp and the overall number of instances of the class
    DATA(timestamp) = obj1->get_timestamp( ).
    DATA(no_of_instances) = lcl_singleton=>no_of_instances.

    output->display( input = timestamp name = `timestamp` ).
    output->display( input = no_of_instances name = `no_of_instances` ).

    "Trying to get another instance
    obj2 = lcl_singleton=>factory_method( ).

    "Getting time stamp and the overall number of instances of the class
    timestamp =  obj2->get_timestamp( ).
    no_of_instances = lcl_singleton=>no_of_instances.

    output->display( input = timestamp name = `timestamp` ).
    output->display( input = no_of_instances name = `no_of_instances` ).

    "Trying to get another instance
    DATA(obj3) = lcl_singleton=>factory_method( ).

    "Getting time stamp and the overall number of instances of the class
    timestamp =  obj2->get_timestamp( ).

    no_of_instances = lcl_singleton=>no_of_instances.

    output->display( input = timestamp name = `timestamp` ).
    output->display( input = no_of_instances name = `no_of_instances` ).

    output->next_section( `21) Factory method in an abstract class` ).

    "The example demonstrates a factory method in an abstract class. An
    "instance is tried to be created two times. The factory method is
    "implemented in a way that prevents the creation of an instance if a
    "certain condition is not met. In this simplistic example, the creation
    "is only allowed if the value '1' is passed to the factory method. The
    "second instance creation fails on purpose.

    output->display( `First try: inst_1` ).

    TRY.
        DATA(inst_1) = lcl_abstract=>factory_method( 1 ).
        DATA(str_1) = inst_1->return_string( `inst_1` ).
        output->display( input = str_1 name = `str_1` ).
      CATCH cx_sy_ref_is_initial INTO DATA(error1).
        output->display( |Error message: { error1->get_text( ) }| ).
    ENDTRY.

    output->display( input = lcl_abstract=>message name = `lcl_abstract=>message` ).

    output->display( `Second try: inst_2` ).

    TRY.
        DATA(inst_2) = lcl_abstract=>factory_method( 2 ).
        DATA(str_2) = inst_2->return_string( `inst_2` ).
        output->display( input = str_2 name = `str_2` ).
      CATCH cx_sy_ref_is_initial INTO DATA(error2).
        output->display( |Error message: { error2->get_text( ) }| ).
    ENDTRY.

    output->display( input = lcl_abstract=>message name = `lcl_abstract=>message` ).

    output->next_section( `22) Friendship: Accessing components of friends` ).

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

    output->display( input = string_table name = `string_table` ).

    output->next_section( `23) Self-reference me` ).

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

    output->display( input = string_without_me name = `string_without_me` ).
    output->display( input = string_with_me name = `string_with_me` ).

    output->next_section( `24) Events` ).

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

    output->display( input = ref_events->greets name = `ref_events->greets` ).

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
