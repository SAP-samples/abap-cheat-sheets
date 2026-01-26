"! <p class="shorttext"><strong>Proxy</strong>
"! <br/>ABAP cheat sheet example class</p>
"!
"! <p>The example class demonstrates the proxy design pattern.<br/>
"! Choose F9 in ADT to run the class.</p>
"!
"! <h2>Note</h2>
"! <ul><li><strong>Global class</strong> (<em>Global Class</em> tab in ADT): Serves as the client that makes use
"! of local classes to demonstrate the design pattern. Largely, the declarations and
"! implementations in the CCIMP include are relevant for the conceptual considerations.</li>
"! <li><strong>CCIMP include</strong> (<em>Local Types</em> tab in ADT): Contains various local classes/interfaces
"! to demonstrate the design pattern.</li>
"! <li>See the <strong>disclaimer</strong> in the ABAP Doc comment of class {@link zcl_demo_abap_oodp_aux}.</li></ul>
CLASS zcl_demo_abap_oodp_proxy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_demo_abap_oodp_proxy IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Proxy` ).

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `User that is allowed to get user info` ).
    TRY.
        DATA(oref_power_user) = NEW lcl_proxy( username = `power_user` password = `abc123` ).
        DATA(info_a) = oref_power_user->lif_user_info~get_user_info( user_id = `A0001` ).
        DATA(info_b) = oref_power_user->lif_user_info~get_user_info( user_id = `A0002` ).

        out->write( info_a ).
        out->write( info_b ).
      CATCH lcx_error INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `User that is not allowed to get user info` ).
    TRY.
        DATA(oref_other_user) = NEW lcl_proxy( username = `other_user` password = `some_pw` ).
        DATA(info_c) = oref_other_user->lif_user_info~get_user_info( user_id = `A0003` ).
        DATA(info_d) = oref_other_user->lif_user_info~get_user_info( user_id = `A0004` ).

        out->write( info_c ).
        out->write( info_d ).
      CATCH lcx_error INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Wrong credentials` ).
    TRY.
        DATA(oref_typo) = NEW lcl_proxy( username = `power_user` password = `abc1234` ).
        DATA(info_e) = oref_typo->lif_user_info~get_user_info( user_id = `A0005` ).
        DATA(info_f) = oref_typo->lif_user_info~get_user_info( user_id = `A0006` ).

        out->write( info_e ).
        out->write( info_f ).
      CATCH lcx_error INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

**********************************************************************

    zcl_demo_abap_oodp_aux=>set_example_divider( out  = out text = `Log` ).
    DATA(log) = lcl_proxy=>get_log( ).
    out->write( log ).
  ENDMETHOD.
ENDCLASS.
