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

***************
*** TABELAS ***
***************
TABLES: pa0000.

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

***************************************
*** PARAMETROS DE SELEÇÃO DE DADOS  ***
***************************************
SELECTION-SCREEN: BEGIN OF BLOCK b_main WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_data FOR pa0000-begda NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b_main.

* START-OF-SELECTION
START-OF-SELECTION.
  PERFORM: zf_selecionar_dados,
           zf_processar_dados.

* END-OF-SELECTION
END-OF-SELECTION.
  PERFORM: zf_exibir_alv.

*--------------------------------------------------------*
*               Form  ZZ_SELECIONAR_DADOS                  *
*---------------------------------------------d-----------*
*   SELECIONA OS DADOS A SEREM EXIBIDOS PELO RELATÓRIO   *
*--------------------------------------------------------*
FORM zf_selecionar_dados.
  SELECT pernr
  FROM pa0000
  INTO TABLE @DATA(it_results)
  WHERE pa0000~begda >= @p_data-low AND pa0000~endda <= @p_data-high.

ENDFORM.

*--------------------------------------------------------*
*               Form  ZZ_PROCESSAR_DADOS                  *
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