<a name="top"></a>

# Generative AI

- [Generative AI](#generative-ai)
  - [Information and Documentation](#information-and-documentation)
  - [Using the ABAP AI SDK](#using-the-abap-ai-sdk)

This ABAP cheat sheet provides references to detailed information on *Generative AI in ABAP Cloud* and explores [released](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenreleased_api_glosry.htm) ABAP classes available in the *ABAP AI SDK powered by Intelligent Scenario Lifecycle Management*.

## Information and Documentation

- Blogs:
  - [The Power of Joule for Developers, ABAP AI Capabilities](https://community.sap.com/t5/technology-blogs-by-sap/the-power-of-joule-for-developers-abap-ai-capabilities/ba-p/14019716)
  - [Unleash Your Inner ABAP Rockstar with Joule for developers , ABAP AI capabilities](https://community.sap.com/t5/technology-blogs-by-sap/unleash-your-inner-abap-rockstar-with-joule-for-developers-abap-ai/ba-p/14019607)
  - [Joule speaks ABAP!](https://community.sap.com/t5/technology-blogs-by-sap/joule-speaks-abap/ba-p/14018226)
- SAP Help documentation and frequently asked questions
  - [Joule for Developers, ABAP AI Capabilities](https://help.sap.com/docs/abap-ai?locale=en-US)
  - [Generative AI in ABAP Cloud](https://help.sap.com/docs/abap-ai/generative-ai-in-abap-cloud/generative-ai-in-abap-cloud?locale=en-US)
  - [ABAP AI Strategy Frequently Asked Questions](https://www.sap.com/documents/2024/11/6c0a3705-e77e-0010-bca6-c68f7e60039b.html)
- Tutorial on GitHub
  - [RAP120 - Build SAP Fiori Apps with ABAP Cloud and Joule](https://github.com/SAP-samples/abap-platform-rap120)
- Devtoberfest session
  - [Boost your Coding Efficiency: Explore Joule’s ABAP Developer Capabilities](https://www.youtube.com/live/W1B8CWprDFM?si=lnkSGyPHp8lw4bQD)

<p align="right"><a href="#top">⬆️ back to top</a></p>
  
## Using the ABAP AI SDK

- This section explores released ABAP classes within the ABAP AI SDK for interacting with large language models (LLMs) in custom implementations.
- Find more information in [Developing Your Own AI-Enabled Applications](https://help.sap.com/docs/ABAP_AI/c7f5ef43ab274d078baf22f995fd2161/27c5d27b480043f0a9fd8e46ae8275a2.html?locale=en-US).

> [!NOTE]
> - The ABAP AI SDK is integrated with the *Intelligent Scenario Lifecycle Management (ISLM)*. Before using the ABAP AI SDK, administrative tasks outlined in the [documentation](https://help.sap.com/docs/ABAP_AI/c7f5ef43ab274d078baf22f995fd2161/339bd7a66c8545159cec357ce7f183d4.html?locale=en-US) have to be performed.
> - As a prerequisite to using the ABAP AI SDK, you have to create intelligent scenarios (ABAP repository objects containing various features to enable, for example, the instantiation of the completion API) and intelligent scenario models (defining, for example, which LLM is used).
> - The code snippets use the intelligent scenario name `ZDEMO_ABAP_INT_SCEN`. Assume that an example intelligent scenario model with the name `ZDEMO_ABAP_INT_SCEN_MODEL` exists, which includes a prompt template ID named `ZDEMO_PROMPT_TEMPLATE`. 
> - [Disclaimer](./README.md#%EF%B8%8F-disclaimer)

Method calls that create an instance of the ISLM completion API, use a prompt as string, and retrieve the LLM answer:

```abap
TRY.
    FINAL(ai_api) = cl_aic_islm_compl_api_factory=>get( )->create_instance( 'ZDEMO_ABAP_INT_SCEN' ).
    FINAL(result) = ai_api->execute_for_string( `Tell me a joke.` ).
    FINAL(completion) = result->get_completion( ).
  CATCH cx_aic_api_factory cx_aic_completion_api INTO FINAL(error).
    FINAL(error_text) = error->get_text( ).
ENDTRY.
```

The following example class includes various calls exploring the APIs. Note that more methods are available, and that demo intelligent scenario and prompt template names are used in the example. Check the class and SAP Help Portal documentation. The example includes: 
- Creating an instance of the ISLM completion API
- Calling the LLM completion API with a prompt as string
- Setting parameters
- Retrieving information regarding the result
- Calling the LLM completion API with a prompt as message list
- Calling the prompt library API to use prompt templates

```abap
CLASS zcl_demo_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS islm_scenario TYPE aic_islm_scenario_id=>type VALUE 'ZDEMO_ABAP_INT_SCEN'.
    CONSTANTS prompt_template TYPE aic_islm_prompt_template_id=>type VALUE 'ZDEMO_PROMPT_TEMPLATE'.
    DATA error TYPE REF TO cx_root.
    DATA ai_api TYPE REF TO if_aic_completion_api.
    DATA error_text TYPE string.
    DATA llm_result TYPE REF TO if_aic_completion_api_result.
    DATA llm_answer TYPE string.
ENDCLASS.


CLASS zcl_demo_abap IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

*&---------------------------------------------------------------------*
*& Creating an instance of the ISLM completion API
*&---------------------------------------------------------------------*

    out->write( |1) Creating an instance of the ISLM completion API\n| ).

    TRY.
        ai_api = cl_aic_islm_compl_api_factory=>get( )->create_instance( islm_scenario ).
      CATCH cx_aic_api_factory INTO error.
        error_text = error->get_text( ).
        out->write( error_text ).
    ENDTRY.

    IF error IS INITIAL.
      out->write( `An instance of the ISLM completion API has been created.` ).
    ELSE.
      RETURN.
    ENDIF.

    out->write( |\n| ).
    out->write( repeat( val = `_` occ = 70 ) ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Calling the LLM completion API with a prompt as string
*&---------------------------------------------------------------------*

    out->write( |2) Calling the LLM completion API with a prompt as string \n\n| ).

    TRY.
        llm_result = ai_api->execute_for_string( `Tell me a joke.` ).
        llm_answer = llm_result->get_completion( ).
        out->write( data = llm_answer name = `llm_answer` ).
      CATCH cx_aic_completion_api INTO error.
        out->write( error->get_text( ) ).
    ENDTRY.

    out->write( |\n| ).
    out->write( repeat( val = `_` occ = 70 ) ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Setting parameters
*&---------------------------------------------------------------------*

    out->write( |3) Setting parameters \n\n| ).

    TRY.
        "Creating another instance of the ISLM completion API
        ai_api = cl_aic_islm_compl_api_factory=>get( )->create_instance( islm_scenario ).

        "Setting parameters such as maximum tokens or temperature
        FINAL(params) = ai_api->get_parameter_setter( ).
        params->set_maximum_tokens( 500 ).
        params->set_temperature( '0.5' ). "The value must be between 0 and 1

        llm_result = ai_api->execute_for_string( `What is ABAP?` ).
        llm_answer = llm_result->get_completion( ).
        out->write( data = llm_answer name = `llm_answer` ).
      CATCH cx_aic_api_factory cx_aic_completion_api INTO error.
        error_text = error->get_text( ).
        out->write( error_text ).
    ENDTRY.

    out->write( |\n| ).
    out->write( repeat( val = `_` occ = 70 ) ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Retrieving information regarding the result
*&---------------------------------------------------------------------*

    out->write( |4) Retrieving information regarding the result \n\n| ).

    TRY.
        "Creating another instance of the ISLM completion API
        ai_api = cl_aic_islm_compl_api_factory=>get( )->create_instance( islm_scenario ).
        llm_result = ai_api->execute_for_string( `What is an internal table in ABAP?` ).
        llm_answer = llm_result->get_completion( ).

        "Number of tokens of the LLM output
        FINAL(completion_token_count) = llm_result->get_completion_token_count( ).
        "Number of tokens of the LLM input
        FINAL(prompt_token_count) = llm_result->get_prompt_token_count( ).
        "Number of tokens of the LLM input and output
        FINAL(total_token_count) = llm_result->get_total_token_count( ).
        "Runtime of LLM call in milliseconds
        FINAL(runtime) = llm_result->get_runtime_ms( ).

        out->write( data = llm_answer name = `llm_answer` ).
        out->write( |\n| ).
        out->write( data = completion_token_count name = `completion_token_count` ).
        out->write( data = prompt_token_count name = `prompt_token_count` ).
        out->write( data = total_token_count name = `total_token_count` ).
        out->write( data = runtime name = `runtime` ).

      CATCH cx_aic_api_factory cx_aic_completion_api INTO error.
        error_text = error->get_text( ).
        out->write( error_text ).
    ENDTRY.

    out->write( |\n| ).
    out->write( repeat( val = `_` occ = 70 ) ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Calling the LLM completion API with a prompt as message list
*&---------------------------------------------------------------------*

    out->write( |5) Calling the LLM completion API with a prompt as message list \n\n| ).

    TRY.
        "Creating another instance of the ISLM completion API
        ai_api = cl_aic_islm_compl_api_factory=>get( )->create_instance( islm_scenario ).

        "Creating a message container instance and adding messages
        FINAL(message_container) = ai_api->create_message_container( ).
        message_container->set_system_role( `You are a professional translator` ).
        message_container->add_user_message( `Can you translate German into English?` ).
        message_container->add_assistant_message( `Yes` ).
        message_container->add_user_message( `Translate the following German sentence into English: "Entschuldigung, wie komme ich zum Bahnhof?"` ).
        llm_answer = ai_api->execute_for_messages( message_container )->get_completion( ).

        "Retrieving the messages added to the message container
        FINAL(messages) = message_container->get_messages( ).
        out->write( data = llm_answer name = `llm_answer` ).
        out->write( |\n| ).
        out->write( data = messages name = `messages` ).
      CATCH cx_aic_api_factory cx_aic_completion_api INTO error.
        error_text = error->get_text( ).
        out->write( error_text ).
    ENDTRY.

    out->write( |\n| ).
    out->write( repeat( val = `_` occ = 70 ) ).
    out->write( |\n| ).

*&---------------------------------------------------------------------*
*& Calling the prompt library API to use prompt templates
*&---------------------------------------------------------------------*

    out->write( |6) Calling the prompt library API to use prompt templates \n\n| ).

    TRY.
        "Creating another instance of the ISLM completion API
        ai_api = cl_aic_islm_compl_api_factory=>get( )->create_instance( islm_scenario ).

        FINAL(prompt_temp) = cl_aic_islm_prompt_tpl_factory=>get( )->create_instance( islm_scenario = islm_scenario
                                                                                      template_id   = prompt_template ).

        "Retrieving the prompt from the template
        "Note that, depending on the prompt template setup, there may be input parameters
        "('parameters' importing parameter) in the following method call to be assigned.
        FINAL(prompt) = prompt_temp->get_prompt( ).
        FINAL(msg_container) = ai_api->create_message_container( ).
        msg_container->set_system_role( prompt ).
        msg_container->add_user_message( `... some user message ...` ).
        llm_answer = ai_api->execute_for_messages( msg_container )->get_completion( ).

        out->write( data = llm_answer name = `llm_answer` ).
      CATCH cx_aic_api_factory cx_aic_completion_api cx_aic_prompt_template INTO error.
        error_text = error->get_text( ).
        out->write( error_text ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```