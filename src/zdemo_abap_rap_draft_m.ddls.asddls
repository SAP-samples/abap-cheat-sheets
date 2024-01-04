@AccessControl.authorizationCheck: #NOT_REQUIRED

@ObjectModel.semanticKey: ['id']
@UI: {
  headerInfo: {
  typeName: 'Calculation',
  typeNamePlural: 'Calculations',
  title: { type: #STANDARD, value: 'id' }
  }
}

define root view entity ZDEMO_ABAP_RAP_DRAFT_M
  as select from zdemo_abap_tabca
{

      @UI.facet: [
        {
          id:       'calc',
          purpose:  #STANDARD,
          type:     #IDENTIFICATION_REFERENCE,
          label:    'Calculation',
          position: 10
        }]

      @EndUserText.label: 'Calculation ID'
      @UI: { lineItem: [ { importance: #HIGH, position: 10,
                           label: 'Calculation ID' },
                       { type: #FOR_ACTION, dataAction: 'delete_all',
                         label: 'Delete All Persisted Calculations' } ],
             fieldGroup: [ { qualifier: 'Fieldgroup:HeaderItems',
                             position: 10 } ] }
  key id,
      @UI: { lineItem:       [ { importance: #HIGH, position: 20,
                                 label: '1st Operand' } ],
             identification: [ { position: 20,
                                 label: '1st Operand' } ],
             fieldGroup:     [ { qualifier: 'CaluclationItems',
                                 position: 10 } ] }
      num1,
      @UI: { lineItem:       [ { importance: #HIGH, position: 30,
                                 label: 'Operator' } ],
             identification: [ { position: 30, label: 'Operator' } ],
             fieldGroup:     [ { qualifier: 'CaluclationItems',
                                 position: 20 } ] }
      arithm_op,
      @UI: { lineItem:       [ { importance: #HIGH, position: 40,
                                 label: '2nd Operand' } ],
             identification: [ { position: 40,
                                 label: '2nd Operand' } ],
             fieldGroup:     [ { qualifier: 'CaluclationItems',
                                 position: 30 } ] }
      num2,
      @UI: { lineItem:       [ { importance: #HIGH, position: 50,
                                 label: 'Result' } ],
             identification: [ { position: 50, label: 'Result' } ],
             fieldGroup:     [ { qualifier: 'CaluclationItems',
                                 position: 40 } ] }
      calc_result,
      @UI: { hidden: true }
      @Semantics.systemDateTime.lastChangedAt: true
      crea_date_time,
      @EndUserText.label: 'Last Changed At'
      @UI: { fieldGroup:     [ { qualifier: 'Fieldgroup:HeaderItems',
                                 position: 20 } ] }
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      lchg_date_time
}
