*&---------------------------------------------------------------------*
*& Report  ZPR_END_ENTREGACESTABAS
*&---------------------------------------------------------------------*
*& Nome: GAP_01_ECP_01
*& Tipo: Report
*& Objetivo: atender a distribuição de cesta básica aos colaboradores
*& Data/Hora: Segunda, Fevereiro 27, 2023 (GMT-3) - 08:00
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
DATA: it_saida TYPE TABLE OF ty_saida ##NEEDED.

*****************
*** CONSTANTS ***
*****************
CONSTANTS: gc_benef_cod_end_cest TYPE tvarvc-name VALUE 'ZHR_BENEF_COD_END_CEST' ##NEEDED.
CONSTANTS: gc_benef_cod_benef TYPE tvarvc-name VALUE 'zhr_benef_cod_benef' ##NEEDED.
CONSTANTS: gc_stat_ativo TYPE p0000-stat2 VALUE '3' ##NEEDED.
CONSTANTS: gc_extensao_excel TYPE string VALUE '.xls' ##NEEDED.
CONSTANTS: gc_nome_arquivo TYPE string VALUE 'REL_ENT_CESTBASICA' ##NEEDED.

***************
*  VARIÁVEIS  *
***************
DATA: gv_cod_cesta TYPE p0006-subty ##NEEDED.
DATA: gv_cod_benef TYPE p0377-subty ##NEEDED.
DATA: gv_path      TYPE string ##NEEDED.

***************************************
*** PARAMETROS DE SELEÇÃO DE DADOS  ***
***************************************
SELECTION-SCREEN: BEGIN OF BLOCK b_oper WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_alv RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND rd1.
SELECTION-SCREEN COMMENT 4(3) text-rb1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_xls RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 4(5) text-rb2.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_file TYPE rlgrap-filename MODIF ID fle.
SELECTION-SCREEN: END OF BLOCK b_oper.

* AT SELECTION-SCREEN OUTPUT
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 EQ 'FLE'.
      screen-active = COND #( WHEN p_xls = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

* AT SELECTION-SCREEN
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = gv_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    CLEAR: p_file, gv_path.
  ELSE.
    gv_path = |{ gv_path }\\{ gc_nome_arquivo }{ gc_extensao_excel }|.
    p_file = replace( val = gv_path sub = '\\' with = '\' ).
  ENDIF.


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
  ELSEIF p_xls = abap_true AND p_file IS INITIAL.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e03.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM: zf_selecionar_tvarv.

GET peras.
  PERFORM: zf_selecionar_dados.

* END-OF-SELECTION
END-OF-SELECTION.
  IF p_alv = abap_true.
    PERFORM zf_exibir_alv.
  ELSEIF p_xls = abap_true.
    PERFORM: zf_salvar_excel.
  ENDIF.

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
  
  " Verifica se durante o mês de processamento do benefício, 
  " o funcionário possui pelo menos um dia de atividade registrada
  " e o subtipo da cesta básica para inclusão da pessoa no relatório
  IF p0000-stat2 = gc_stat_ativo AND p0377 IS NOT INITIAL.
    DATA(lv_usar_filial) = xsdbool( sy-subrc <> 0 ).

    " Busca o código da filial
    SELECT SINGLE brap~filia
         FROM t7br0p AS brop
         INNER JOIN t7brap AS brap ON brap~grpbr = brop~grpbr
         INTO @DATA(lv_filial)
         WHERE brop~werks = @p0001-werks AND brop~btrtl = @p0001-btrtl.

    " Caso não seja encontrado no infotipo P0006 o subtipo da tabela TVARV,
    " utiliza o endereço da filial
    IF lv_usar_filial = abap_true.
      CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
        EXPORTING
          branch                  = lv_filial
          bukrs                   = p0001-bukrs
       IMPORTING
         address1                = ls_address1
       EXCEPTIONS
         branch_not_found        = 1
         address_not_found       = 2
         company_not_found       = 3
         OTHERS                  = 4.

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
                    descricao_area = cl_hr_t500p=>read( p0001-werks )-name1
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
  ENDIF.
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
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
  ENDIF.

ENDFORM.

*--------------------------------------------------------*
*                 Form  ZF_SALVAR_EXCEL                  *
*--------------------------------------------------------*
*        GERA O ARQUIVO EXCEL E SALVA LOCALMENTE         *
*--------------------------------------------------------*
FORM zf_salvar_excel.
  TYPES: BEGIN OF ly_header,
      column TYPE char40,
       END OF ly_header.
  TYPES: ls_header TYPE TABLE OF ly_header WITH EMPTY KEY.

  DATA(lt_header) = VALUE ls_header(
  ( CONV ly_header-column( text-c01 ) ) "Empresa
  ( CONV ly_header-column( text-c02 ) ) "Filial
  ( CONV ly_header-column( text-c03 ) ) "Área
  ( CONV ly_header-column( text-c04 ) ) "Descrição Área
  ( CONV ly_header-column( text-c05 ) ) "Subárea
  ( CONV ly_header-column( text-c06 ) ) "Descrição Subárea
  ( CONV ly_header-column( text-c07 ) ) "Pernr
  ( CONV ly_header-column( text-c08 ) ) "Nome Pessoa
  ( CONV ly_header-column( text-c09 ) ) "Período Início
  ( CONV ly_header-column( text-c10 ) ) "Período Final
  ( CONV ly_header-column( text-c11 ) ) "Rua
  ( CONV ly_header-column( text-c12 ) ) "Número
  ( CONV ly_header-column( text-c13 ) ) "Complemento
  ( CONV ly_header-column( text-c14 ) ) "Bairro
  ( CONV ly_header-column( text-c15 ) ) "Cidade
  ( CONV ly_header-column( text-c16 ) ) "UF
  ( CONV ly_header-column( text-c17 ) ) "PAÍS
  ( CONV ly_header-column( text-c18 ) ) "CEP
 ).

  gv_path = COND #( WHEN gv_path IS INITIAL THEN p_file ELSE gv_path ).
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = gv_path
      filetype                = 'ASC'
      write_field_separator   = abap_true
    TABLES
      data_tab                = it_saida
      fieldnames              = lt_header
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.