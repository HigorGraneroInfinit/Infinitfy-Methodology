*&---------------------------------------------------------------------*
*& Report  ZPR_END_ENTREGACESTABAS
*&---------------------------------------------------------------------*
*& Nome: GAP_01_ECP_01
*& Tipo: Report
*& Objetivo: atender a distribuição de cesta básica aos colaboradores
*& Data/Hora: Sexta, Setembro 16, 2022 (GMT-3) - 10:15
*& Desenvolvedor: Sérgio Melges (Infinitfy)
*&---------------------------------------------------------------------*
*& Versão 1: Sérgio Melges (Infinitfy) - Inicio Desenvolvimento -
*& PKDK900125
*& Versão 2: ?
*& Versão 3: ?
*&---------------------------------------------------------------------*

REPORT zpr_end_entregacestabas.


*****************
*** INFOTYPES ***
*****************
INFOTYPES: 0000. "Medidas
INFOTYPES: 0001. "Atribuição organizacional
INFOTYPES: 0002. "Dados pessoais
INFOTYPES: 0006. "Endereços
INFOTYPES: 0171. "Inform.Geral Benefícios
INFOTYPES: 0377. "Planos Diversos

***************
*** TABELAS ***
***************
TABLES: pernr.
TABLES: t500p.
TABLES: t001p.
TABLES: t7br0p.
TABLES: t7brap.

************************************
*** Nós de Banco de Dados Lógico ***
************************************
NODES: peras.

***************
***  TYPES  ***
***************
TYPES: BEGIN OF ty_saida,
     empresa TYPE p0001-bukrs,
     filial TYPE t7brap-filia,
     area TYPE p0001-werks,
     descricao_area TYPE t500p-name1,
     subarea TYPE p0001-btrtl,
     descricao_subarea TYPE t001p-btext,
     pernr TYPE p0000-pernr,
     nome_pessoa TYPE p0002-cname,
     periodo_inicio TYPE p0000-begda,
     periodo_final TYPE p0000-endda,
     rua TYPE p0006-stras,
     numero TYPE p0006-hsnmr,
     complemento TYPE p0006-posta,
     bairro TYPE p0006-ort02,
     cidade TYPE p0006-ort01,
     uf TYPE p0006-state,
     pais TYPE p0006-land1,
     cep TYPE p0006-pstlz,
END OF ty_saida.

*************************
***  INTERNAL TABLES  ***
*************************
DATA: it_saida TYPE TABLE OF ty_saida.

*****************
*** CONSTANTS ***
*****************
CONSTANTS: gc_benef_cod_end_cest TYPE tvarvc-name VALUE
'ZHR_BENEF_COD_END_CEST'.
CONSTANTS: gc_benef_cod_benef TYPE tvarvc-name VALUE
'ZHR_BENEF_COD_BENEF'.

***************
*  VARIÁVEIS  *
***************
DATA: gv_cod_cesta TYPE p0006-subty.
DATA: gv_cod_benef TYPE p0377-subty.

* Initialization
INITIALIZATION.
  "Período de Análise - Outro Período
  pnptimed = 'I'.

* START-OF-SELECTION
START-OF-SELECTION.
  FREE: it_saida.

  IF pnpbegda IS INITIAL OR pnpendda IS INITIAL.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e01.
    LEAVE LIST-PROCESSING.
  ENDIF.
  PERFORM: zf_selecionar_tvarv.

GET peras.
  PERFORM: zf_selecionar_dados.

  PERFORM: zf_processar_dados.

* END-OF-SELECTION
END-OF-SELECTION.
  PERFORM: zf_exibir_alv.

*--------------------------------------------------------*
*               Form  ZF_SELECIONAR_TVARV                *
*--------------------------------------------------------*
*   SELECIONA OS DADOS DA TVARV                          *
*--------------------------------------------------------*
FORM zf_selecionar_tvarv.
  SELECT SINGLE low
  FROM tvarvc
  INTO gv_cod_cesta WHERE name EQ gc_benef_cod_end_cest.

  SELECT SINGLE low
  FROM tvarvc
  INTO gv_cod_benef WHERE name EQ gc_benef_cod_benef.
ENDFORM.

*--------------------------------------------------------*
*               Form  ZF_SELECIONAR_DADOS                *
*--------------------------------------------------------*
*   SELECIONA OS DADOS A SEREM EXIBIDOS PELO RELATÓRIO   *
*--------------------------------------------------------*
FORM zf_selecionar_dados.
  DATA: ls_address1 TYPE addr1_val.

  "Leitura dos Infotipos
  rp_provide_from_last p0000 space pn-begda pn-endda.
  rp_provide_from_last p0001 space pn-begda pn-endda.
  rp_provide_from_last p0002 space pn-begda pn-endda.
  rp_provide_from_last p0171 space pn-begda pn-endda.
  rp_provide_from_last p0377 gv_cod_benef pn-begda pn-endda.
  rp-read-t001p p0001-werks p0001-btrtl space.
  rp_provide_from_last p0006 gv_cod_cesta pn-begda pn-endda.

  DATA(lv_usar_filial) = xsdbool( sy-subrc <> 0 ).

  " Busca o código da filial
  SELECT SINGLE brap~filia
       FROM t7br0p AS brop
       INNER JOIN t7brap AS brap ON brap~grpbr = brop~grpbr
       INTO @DATA(lv_filial)
       WHERE brop~werks = @p0001-werks AND brop~btrtl = @p0001-btrtl.

  IF lv_usar_filial = abap_true.
    CALL FUNCTION
  'J_1BREAD_BRANCH_DATA'
      EXPORTING
        branch                  =
  lv_filial
        bukrs                   =
  p0001-bukrs
     IMPORTING
*                                          ADDRESS                 =
*                                          BRANCH_DATA             =
*                                          CGC_NUMBER              =
       address1                =
  ls_address1
     EXCEPTIONS
       branch_not_found        = 1
       address_not_found       = 2
       company_not_found       = 3
       OTHERS                  = 4
              .
    IF sy-subrc = 0.
      p0006-stras = ls_address1-name1.
      p0006-hsnmr = ls_address1-addrnumber.
      p0006-posta = ls_address1-house_num2.
      p0006-ort02 = ls_address1-city2.
      p0006-ort01 = ls_address1-city1.
      p0006-state = ls_address1-region.
      p0006-land1 = ls_address1-po_box_cty.
      p0006-pstlz = ls_address1-post_code1.
    ENDIF.

  ENDIF.

  INSERT VALUE #( empresa = p0001-bukrs
                  filial = lv_filial
                  area = p0001-werks
                  descricao_area = cl_hr_t500p=>read( p0001-werks
)-name1
                  subarea = p0001-btrtl
                  descricao_subarea = t001p-btext
                  pernr = p0000-pernr
                  nome_pessoa = p0002-cname
                  periodo_inicio = pn-begda
                  periodo_final = pn-endda
                  rua = p0006-stras
                  numero = p0006-hsnmr
                  complemento = p0006-posta
                  bairro = p0006-ort02
                  cidade = p0006-ort01
                  uf = p0006-state
                  pais = p0006-land1
                  cep = p0006-pstlz
     ) INTO TABLE it_saida.

ENDFORM.

*--------------------------------------------------------*
*               Form  ZF_PROCESSAR_DADOS                 *
*--------------------------------------------------------*
*         PROCESSA AS INFORMAÇÕES DO RELATÓRIO           *
*--------------------------------------------------------*
FORM zf_processar_dados.

ENDFORM.

*--------------------------------------------------------*
*                 Form  ZF_EXIBIR_ALV                    *
*--------------------------------------------------------*
*              EXIBE OS DADOS DOS RELATÓRIO              *
*--------------------------------------------------------*
FORM zf_exibir_alv.
  DATA: lt_fieldcat TYPE lvc_t_fcat.
  DATA: lw_layout   TYPE lvc_s_layo.
  DATA: lw_saida TYPE ty_saida.

  lw_layout-zebra = abap_true.
  lw_layout-cwidth_opt = abap_true.

* Criação da tabela de fieldcat
  CALL FUNCTION 'STRALAN_FIELDCAT_CREATE'
    EXPORTING
      is_structure = lw_saida
    IMPORTING
      et_fieldcat  = lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = lw_layout
      it_fieldcat_lvc    = lt_fieldcat
    TABLES
      t_outtab           = it_saida
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc NE 0.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e01.
  ENDIF.

ENDFORM.