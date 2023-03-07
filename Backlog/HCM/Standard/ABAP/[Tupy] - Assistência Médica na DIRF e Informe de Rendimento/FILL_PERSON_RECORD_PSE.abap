  METHOD if_hrpaybr_dirf1~fill_person_record_pse.
    CONSTANTS: lc_pse(3)   TYPE c VALUE 'PSE'.
    CONSTANTS: lc_opse(4)  TYPE c VALUE 'OPSE'.
    CONSTANTS: lc_tpse(4)  TYPE c VALUE 'TPSE'.
    CONSTANTS: lc_dtpse(5) TYPE c VALUE 'DTPSE'.
    CONSTANTS: lc_rdtpse(6) TYPE c VALUE 'RDTPSE'.
    CONSTANTS: lc_0002(4) TYPE c VALUE '0002'.
    CONSTANTS: lc_0021(4) TYPE c VALUE '0021'.
    CONSTANTS: lc_0167(4) TYPE c VALUE '0167'.
    CONSTANTS: lc_0397(4) TYPE c VALUE '0397'.
    CONSTANTS: lc_0465(4) TYPE c VALUE '0465'.
    CONSTANTS: lc_sub0001(4) TYPE c VALUE '0001'.

    DATA: lt_p0002 TYPE TABLE OF p0002.
    DATA: lt_p0021 TYPE TABLE OF p0021.
    DATA: lt_p0167 TYPE TABLE OF p0167.
    DATA: lt_p0397 TYPE TABLE OF p0397.
    DATA: lt_p0465 TYPE TABLE OF p0465.

    DATA: ls_companies  TYPE hrpaybr_s_dirf_2010_comp_pse.
    DATA: ls_holder     TYPE hrpaybr_s_dirf_2010_holder_pse.
    DATA: ls_dependents TYPE hrpaybr_s_dirf_2016_dtpse.
    DATA: ls_dep_reimb TYPE hrpadbr_s_dirf_2016_rtpse.

* Leitura do infotipo 0002
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr     = iv_pernr
        infty     = lc_0002
        begda     = iv_begda
        endda     = iv_endda
* IMPORTING
*     SUBRC     =
      TABLES
        infty_tab = lt_p0002.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

* Leitura do infotipo 0021
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr     = iv_pernr
        infty     = lc_0021
        begda     = iv_begda
        endda     = iv_endda
* IMPORTING
*     SUBRC     =
      TABLES
        infty_tab = lt_p0021.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

* Leitura do infotipo 0167
    CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
      pernr     = iv_pernr
      infty     = lc_0167
      begda     = iv_begda
      endda     = iv_endda
* IMPORTING
*     SUBRC     =
    TABLES
      infty_tab = lt_p0167.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

* Leitura do infotipo 0397
    CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
      pernr     = iv_pernr
      infty     = lc_0397
      begda     = iv_begda
      endda     = iv_endda
* IMPORTING
*     SUBRC     =
    TABLES
      infty_tab = lt_p0397.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

* Leitura do infotipo 0465
    CALL FUNCTION 'HR_READ_INFOTYPE'
   EXPORTING
     pernr     = iv_pernr
     infty     = lc_0465
     begda     = iv_begda
     endda     = iv_endda
* IMPORTING
*     SUBRC     =
   TABLES
     infty_tab = lt_p0465.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SORT lt_p0167 BY pernr infty subty begda ASCENDING.
    SORT lt_p0002 BY begda DESCENDING.
    TRY.
        DATA(ls_p0002) = lt_p0002[ 1 ].
        DATA(ls_p0465) = lt_p0465[ pernr = iv_pernr subty = lc_sub0001
].

* Monta registro
        cs_record-health_care-rec_id = lc_pse.

        ls_companies-company-rec_id  = lc_opse.
        ls_companies-company-cnpj    = '47657971000172'.   "CNPJ de
        " operadora do plano de saúde
        ls_companies-company-name    = 'Operadora Teste'. "Nome da
        " operadora
        ls_companies-company-rec_ans = '123456'. "Registro da Operadora
        " na Agência Nacional de Saúde

*   Monta registro TPSE (Titular)
        ls_holder-holder-rec_id = lc_tpse.
        ls_holder-holder-cpf    = ls_p0465-cpf_nr. "CPF Titular
        ls_holder-holder-name   = ls_p0002-cname.  "Nome Titular
        ls_holder-holder-amount = '1550'. "Valor pago pelo titular

*   Monta registro DTPSE (Dependentes)
        ls_dependents-dependent-rec_id = lc_dtpse.
        ls_dependents-dependent-cpf    = '00175721009'. "CPF
        ls_dependents-dependent-dbirth = '19700208'.   "Data de
        " Nascimento Dependente
        ls_dependents-dependent-depty  = '08'. "Tipo Dependente
        ls_dependents-dependent-amount =  '2440'. "Valor pago relativo
        " ao plano de saúde do dependente.

*   Monta registro RDTPSE (Despesas)
        ls_dep_reimb-rec_id = lc_rdtpse.
        ls_dep_reimb-cpf_cnpj = '47657971000172'. "CNPJ de
        " operadora do plano de saúde
        ls_dep_reimb-comp_name = 'Operadora Teste'. "Nome da
        " operadora
        ls_dep_reimb-curr_year_reimb = '450'.

        INSERT ls_dep_reimb INTO ls_dependents-dep_reimb.
        INSERT ls_dependents INTO ls_holder-dependents.
        INSERT ls_holder INTO ls_companies-holder.
        INSERT ls_companies INTO cs_record-companies.
      CATCH cx_sy_itab_line_not_found.

    ENDTRY.
  ENDMETHOD.