*----------------------------------------------------------------------*
*                             T-SYSTEMS                                *
*----------------------------------------------------------------------*
* Client.....: COPA ENERGIA                                            *
* Author.....: Vitor Yuri Kazuma Ueda                                  *
* Date.......: 05/01/2023                                              *
* Description: Gestão do Saldo de Fundo de Marketing                   *
* Project....:                                                         *
* GAP........: Projeto Fundo Marketing                                 *
*----------------------------------------------------------------------*
* Change log                                                           *
*----------------------------------------------------------------------*
* Author:  Vitor Yuri Kazuma Ueda                      Date:05/01/2023 *
* Request:  ERDK963045                                                 *
* Description: Projeto Fundo Marketing                                 *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Report  ZSDMKT_RELATORIO_FUNDO_MKT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdmkt_relatorio_fundo_mkt.


*&---------------------------------------------------------------------*
*&      Tabelas
*&---------------------------------------------------------------------*
TABLES: vbrk, vbak.

*&---------------------------------------------------------------------*
*&      Estruturas
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_vb,
         vbeln_va TYPE vbak-vbeln,                               "Documento de Vendas"
         erdat    TYPE vbak-erdat,                               "Data de Criação"
         vkorg    TYPE vbak-vkorg,                               "Organização de Vendas"
         vtweg    TYPE vbak-vtweg,                               "Canal de Distribuição"
         spart    TYPE vbak-spart,                               "Setor de Atividade"
         posnr    TYPE vbap-posnr,                               "Item do Documento de Vendas"
         matnr    TYPE vbap-matnr,                               "Número Material"
         vgbel    TYPE vbrp-vgbel,                               "Nº documento do documento de referência"
         vbeln_vf TYPE vbrk-vbeln ,                              "Documento de faturamento
         bukrs    TYPE vbrk-bukrs,                               "Empresa"
         zuonr    TYPE vbrk-zuonr,                               "Nº Atribuição"
         fksto    TYPE vbrk-fksto,                               "Fatura Estornada"
       END OF ty_vb,

       BEGIN OF ty_1bnf,
         docnum TYPE j_1bnflin-docnum,                       "Nº do Document
         itmnum TYPE j_1bnflin-itmnum,                       "Nº do Item do
         refkey TYPE j_1bnflin-refkey,                       "Quantidade"
         menge  TYPE j_1bnflin-menge,                        "Quantidade"
         netwr  TYPE j_1bnflin-netwr,                       "Valor Liquido"
         docdat TYPE j_1bnfdoc-docdat,                       "Data Documento"
         nfnum  TYPE j_1bnfdoc-nfnum,                        "Nº Nota Físcal
       END OF ty_1bnf,

       BEGIN OF ty_alv,
         doc_vendas          TYPE vbak-vbeln,
         data_criacao_vendas TYPE vbak-erdat,
         org_vendas          TYPE vbak-vkorg,
         canal_distribuicao  TYPE vbak-vtweg,
         setor_atividade     TYPE vbak-spart,
         num_material        TYPE vbap-matnr,
         quantidade          TYPE j_1bnflin-menge,
         doc_faturamento     TYPE j_1bnflin-docnum,
         data_criacao_fat    TYPE j_1bnfdoc-docdat,
         empresa             TYPE bseg-bukrs,
         doc_estornado       TYPE vbrk-fksto,
         data_criacao_ext    TYPE datum,
         doc_contabil        TYPE bseg-belnr,
         exercicio           TYPE bseg-gjahr,
*         doc_cont_extorno    TYPE
         doc_compensacao     TYPE bseg-augbl,
         data_compensacao    TYPE bseg-augdt,
         num_documento       TYPE j_1bnfdoc-docnum,
         data_criacao_doc    TYPE j_1bnfdoc-docdat,
         num_nota_fiscal     TYPE j_1bnfdoc-nfnum,
         valor_liquido       TYPE j_1bnflin-netwr,
       END OF ty_alv.

*&---------------------------------------------------------------------*
*&      CONSTANTES
*&---------------------------------------------------------------------*
DATA: c_ven(3)         TYPE c VALUE 'VEN',
      c_fat(3)         TYPE c VALUE 'FAT',
      c_0(1)           TYPE c VALUE '0',
      c_1(1)           TYPE c VALUE '1',
      c_periodo_maximo TYPE i VALUE 31.

*&---------------------------------------------------------------------*
*&      RANGES
*&---------------------------------------------------------------------*
DATA: gr_vbeln      TYPE RANGE OF vbak-vbeln,
      gr_columns    TYPE REF TO   cl_salv_columns_table,
      gr_column     TYPE REF TO   cl_salv_column_table,
      gr_functions  TYPE REF TO   cl_salv_functions_list,
      gr_display    TYPE REF TO   cl_salv_display_settings,
      gr_selections TYPE REF TO   cl_salv_selections.

*&---------------------------------------------------------------------*
*&      TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: gt_vb   TYPE TABLE OF          ty_vb,
      gt_bkpf TYPE STANDARD TABLE OF bkpf,
      gt_bseg TYPE STANDARD TABLE OF bseg,
      gt_1bnf TYPE TABLE OF          ty_1bnf,
      gt_alv  TYPE TABLE OF          ty_alv,
      gt_salv TYPE REF TO            cl_salv_table.

*&---------------------------------------------------------------------*
*&      WORKAREA
*&---------------------------------------------------------------------*
DATA: gwa_alv      LIKE LINE OF gt_alv.

*&---------------------------------------------------------------------*
*&      OBJETOS
*&---------------------------------------------------------------------*
DATA: go_alv       TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container.

*&---------------------------------------------------------------------*
*&      TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_vbelnv FOR vbak-vbeln MODIF ID ven,      "Documento de Vendas"
                s_vbelnf FOR vbrk-vbeln MODIF ID fat,      "Documento de Faturamento"
                s_erdat  FOR vbak-erdat NO-EXTENSION.      "Data Criação"

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modo_agrupamento.

INITIALIZATION.

START-OF-SELECTION.
  IF s_vbelnv IS INITIAL AND
    s_vbelnf IS INITIAL AND
    s_erdat IS INITIAL.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e00.
    LEAVE LIST-PROCESSING.
  ELSEIF s_erdat-high - s_erdat-low > c_periodo_maximo.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e02.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM f_seleciona_dados.

END-OF-SELECTION.
  IF gt_vb IS INITIAL.
    MESSAGE s208(00) DISPLAY LIKE sy-abcde+4(1) WITH text-e01.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM f_monta_saida.

*IF gt_alv[] IS NOT INITIAL.
  PERFORM f_alv.
*ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_MODO_AGRUPAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_modo_agrupamento .

  LOOP AT SCREEN.

    IF s_vbelnv IS NOT INITIAL.
      IF screen-group1 = c_ven.
        screen-input = c_1.
      ENDIF.

      IF screen-group1 = c_fat.
        screen-input = c_0.
      ENDIF.
    ENDIF.

    IF s_vbelnf IS NOT INITIAL.
      IF screen-group1 = c_ven.
        screen-input = c_0.
      ENDIF.

      IF screen-group1 = c_fat.
        screen-input = c_1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

  IF s_vbelnv IS NOT INITIAL.

    SELECT vbak~vbeln AS vbeln_va,                              "Documento de Vendas"
           vbak~erdat,                                          "Data de Criação"
           vbak~vkorg,                                          "Organização de Vendas"
           vbak~vtweg,                                          "Canal de Distribuição"
           vbak~spart,                                          "Setor de Atividade"
           vbap~posnr,                                          "Item do Documento de Vendas"
           vbap~matnr,                                          "Número Material"
           vbrp~vgbel,                                          "Nº documento do documento de referência"
           vbrk~bukrs,                                          "Empresa"
           vbrk~zuonr,                                          "Nº Atribuição"
           vbrk~fksto                                           "Fatura Estornada"
      FROM vbak
      INNER JOIN vbap ON  vbap~vbeln EQ vbak~vbeln
                      AND vbap~erdat EQ vbak~erdat
      LEFT  JOIN vbrp ON  vbrp~aubel EQ vbak~vbeln
                      AND vbrp~erdat EQ vbak~erdat
      LEFT  JOIN vbrk ON  vbrk~vbeln EQ vbrp~vbeln
                      AND vbrk~erdat EQ vbrp~erdat
      INTO CORRESPONDING FIELDS OF TABLE @gt_vb
    WHERE vbak~vbeln IN @s_vbelnv[]
    AND vbak~erdat IN @s_erdat[].

    IF gt_vb[] IS INITIAL.
      FREE: gt_vb[].
    ELSE.
      gr_vbeln = VALUE #( FOR lwa_vbeln IN gt_vb ( sign = 'I' option = 'EQ' low = lwa_vbeln(10) ) ).

      SELECT bkpf~bukrs                                     "Empresa"
             bkpf~belnr                                     "Nº Documento de um Documento Contábil"
             bkpf~gjahr                                     "Exercício"
             bkpf~awkey                                     "Chave Referência"
        FROM bkpf
        INTO CORRESPONDING FIELDS OF TABLE gt_bkpf[]
      WHERE awkey IN gr_vbeln.

      IF gt_bkpf IS NOT INITIAL.

        SELECT bseg~bukrs                                   "Empresa"
               bseg~belnr                                   "Nº Documento de um Documento Contábil"
               bseg~gjahr                                   "Exercício"
               bseg~buzei                                   "Nº Linha de Lançamento no Documento Contábil"
               bseg~augbl                                   "Nº Documento de Compensação"
               bseg~augdt                                   "Data de Compensação"
          FROM bseg
          INTO CORRESPONDING FIELDS OF TABLE gt_bseg[]
          FOR ALL ENTRIES IN gt_bkpf[]
        WHERE belnr EQ gt_bkpf-belnr.
        IF gt_bseg[] IS INITIAL.
          FREE: gt_bseg[].
        ENDIF.

      ELSE.
        FREE: gt_bkpf[].
      ENDIF.

      SELECT j_1bnflin~docnum,                              "Nº do Documento"
             j_1bnflin~itmnum,                              "Nº do Item do Documento"
             j_1bnflin~refkey,                              "Referência ao documento de origem"
             j_1bnflin~menge,                               "Quantidade"
             j_1bnflin~netwr,                               "Valor Liquido"
             j_1bnfdoc~docdat,                              "Data Documento"
             j_1bnfdoc~nfnum                                "Nº Nota Físcal"
        FROM j_1bnflin
        INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum EQ j_1bnflin~docnum
        INTO CORRESPONDING FIELDS OF TABLE @gt_1bnf[]
      WHERE j_1bnflin~refkey IN @gr_vbeln[].
      IF gt_1bnf[] IS INITIAL.
        FREE: gt_1bnf[].
      ENDIF.
    ENDIF.
  ENDIF.

  IF s_vbelnf IS NOT INITIAL.

    SELECT vbrp~vbeln AS vbeln_vf,                                        "Documento de Faturamento"
           vbrp~posnr,                                        "Item do Documento de Faturamento"
           vbrp~vgbel,                                        "Nº Documento de Referência"
           vbrp~aubel AS vbeln_va,                                        "Documento de vendas
           vbak~erdat,                                        "Data de Criação"
           vbak~vkorg,                                        "Organização de Vendas"
           vbak~vtweg,                                        "Canal de Distribuição"
           vbak~spart,                                        "Setor de Atividade"
           vbap~matnr,                                        "Nº Material"
           vbrk~bukrs,                                        "Empresa"
           vbrk~zuonr,                                        "Nº Atribuição"
           vbrk~fksto                                         "Fatura está estornada"
      FROM vbrp
      INNER JOIN vbak ON  vbak~vbeln EQ vbrp~aubel
                      AND vbak~erdat EQ vbrp~erdat
      LEFT JOIN  vbap ON  vbap~vgbel EQ vbak~vbeln
                      AND vbap~erdat EQ vbak~erdat
      INNER JOIN vbrk ON  vbrk~vbeln EQ vbrp~vbeln
                      AND vbrk~erdat EQ vbrp~erdat
      INTO CORRESPONDING FIELDS OF TABLE @gt_vb[]
    WHERE vbrp~vbeln IN @s_vbelnf[]
    AND vbak~erdat IN @s_erdat[].

    IF gt_vb[] IS INITIAL.
      FREE: gt_vb[].
    ELSE.
      gr_vbeln = VALUE #( FOR lwa_vbeln IN gt_vb ( sign = 'I' option = 'EQ' low = lwa_vbeln(10) ) ).

      SELECT bkpf~bukrs                                    "Empresa"
             bkpf~belnr                                    "Nº documento de um documento contábil"
             bkpf~gjahr                                    "Exercicio"
             bkpf~awkey                                    "Chave Referência"
        FROM bkpf
        INTO CORRESPONDING FIELDS OF TABLE gt_bkpf[]
      WHERE bkpf~awkey IN gr_vbeln[].

      IF gt_bkpf[] IS NOT INITIAL.

        SELECT bseg~bukrs                                  "Empresa"
               bseg~belnr                                  "Nº documento de um documento contábil"
               bseg~gjahr                                  "Exercício"
               bseg~buzei                                  "Nº linha de lançamento no documento contábil"
               bseg~augbl                                  "Nº documento de compensação"
               bseg~augdt                                  "Data de compensação"
          FROM bseg
          INTO CORRESPONDING FIELDS OF TABLE gt_bseg[]
          FOR ALL ENTRIES IN gt_bkpf
        WHERE bseg~belnr EQ gt_bkpf-belnr.

      ELSE.
        FREE: gt_bseg[].
      ENDIF.

      SELECT j_1bnflin~docnum,                              "Nº do Documento"
             j_1bnflin~itmnum,                              "Nº do Item do Documento"
             j_1bnflin~refkey,                              "Referência ao documento de origem"
             j_1bnflin~menge,                               "Quantidade"
             j_1bnflin~netwr,                               "Valor Liquido"
             j_1bnfdoc~docdat,                              "Data Documento"
             j_1bnfdoc~nfnum                                "Nº Nota Físcal"
        FROM j_1bnflin
        INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum EQ j_1bnflin~docnum
        INTO CORRESPONDING FIELDS OF TABLE @gt_1bnf[]
      WHERE j_1bnflin~refkey IN @gr_vbeln[].
      IF gt_1bnf[] IS INITIAL.
        FREE: gt_1bnf[].
      ENDIF.
    ENDIF.
  ENDIF.

  IF s_erdat IS NOT INITIAL.
    SELECT vbak~vbeln AS vbeln_va,                              "Documento de Vendas"
           vbak~erdat,                                          "Data de Criação"
           vbak~vkorg,                                          "Organização de Vendas"
           vbak~vtweg,                                          "Canal de Distribuição"
           vbak~spart,                                          "Setor de Atividade"
           vbap~posnr,                                          "Item do Documento de Vendas"
           vbap~matnr,                                          "Número Material"
           vbrp~vgbel,                                          "Nº documento do documento de referência"
           vbrk~vbeln AS vbeln_vf,                              "Documento de faturamento
           vbrk~bukrs,                                          "Empresa"
           vbrk~zuonr,                                          "Nº Atribuição"
           vbrk~fksto                                           "Fatura Estornada"
      FROM vbak
      INNER JOIN vbap ON  vbap~vbeln EQ vbak~vbeln
                      AND vbap~erdat EQ vbak~erdat
      LEFT  JOIN vbrp ON  vbrp~aubel EQ vbak~vbeln
                      AND vbrp~erdat EQ vbak~erdat
      LEFT  JOIN vbrk ON  vbrk~vbeln EQ vbrp~vbeln
                      AND vbrk~erdat EQ vbrp~erdat
      INTO CORRESPONDING FIELDS OF TABLE @gt_vb
    WHERE vbak~erdat IN @s_erdat[].

    IF gt_vb[] IS INITIAL.
      FREE: gt_vb[].
    ELSE.
      gr_vbeln = VALUE #( FOR lwa_vbeln IN gt_vb
                         LET doc_faturamento = lwa_vbeln-vbeln_vf IN
                         ( sign = 'I' option = 'EQ' low = doc_faturamento ) ).

      SELECT bkpf~bukrs                                    "Empresa"
             bkpf~belnr                                    "Nº documento de um documento contábil"
             bkpf~gjahr                                    "Exercicio"
             bkpf~awkey                                    "Chave Referência"
        FROM bkpf
        INTO CORRESPONDING FIELDS OF TABLE gt_bkpf[]
      WHERE bkpf~awkey IN gr_vbeln[].

      IF gt_bkpf[] IS NOT INITIAL.

        SELECT bseg~bukrs                                  "Empresa"
               bseg~belnr                                  "Nº documento de um documento contábil"
               bseg~gjahr                                  "Exercício"
               bseg~buzei                                  "Nº linha de lançamento no documento contábil"
               bseg~augbl                                  "Nº documento de compensação"
               bseg~augdt                                  "Data de compensação"
          FROM bseg
          INTO CORRESPONDING FIELDS OF TABLE gt_bseg[]
          FOR ALL ENTRIES IN gt_bkpf
        WHERE bseg~belnr EQ gt_bkpf-belnr.

      ELSE.
        FREE: gt_bseg[].
      ENDIF.

      SELECT j_1bnflin~docnum,                              "Nº do Documento"
             j_1bnflin~itmnum,                              "Nº do Item do Documento"
             j_1bnflin~refkey,                              "Referência ao documento de origem"
             j_1bnflin~menge,                               "Quantidade"
             j_1bnflin~netwr,                               "Valor Liquido"
             j_1bnfdoc~docdat,                              "Data Documento"
             j_1bnfdoc~nfnum                                "Nº Nota Físcal"
        FROM j_1bnflin
        INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum EQ j_1bnflin~docnum
        INTO CORRESPONDING FIELDS OF TABLE @gt_1bnf[]
      WHERE j_1bnflin~refkey IN @s_erdat.
      IF gt_1bnf[] IS INITIAL.
        FREE: gt_1bnf[].
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_saida.

  SORT: gt_vb[]   BY vbeln_va vbeln_vf  erdat  vgbel,
        gt_bkpf[] BY awkey  belnr,
        gt_bseg[] BY belnr  bukrs  gjahr,
        gt_1bnf[] BY refkey docnum itmnum docdat.

  LOOP AT gt_vb[] INTO DATA(lwa_vb).

    gwa_alv-doc_vendas           = lwa_vb-vbeln_va.
    gwa_alv-data_criacao_vendas  = lwa_vb-erdat.
    gwa_alv-org_vendas           = lwa_vb-vkorg.
    gwa_alv-canal_distribuicao   = lwa_vb-vtweg.
    gwa_alv-setor_atividade      = lwa_vb-spart.
    gwa_alv-num_material         = lwa_vb-matnr.
    gwa_alv-doc_estornado        = lwa_vb-fksto.

    READ TABLE gt_bkpf[] INTO DATA(lwa_bkpf) WITH KEY awkey = lwa_vb-vbeln_va BINARY SEARCH.
    IF gt_bkpf[] IS NOT INITIAL.

      READ TABLE gt_bseg[] INTO DATA(lwa_bseg) WITH KEY belnr = lwa_bkpf-belnr BINARY SEARCH.
      IF gt_bseg[] IS NOT INITIAL.

        gwa_alv-empresa          = lwa_bseg-bukrs.
        gwa_alv-doc_contabil     = lwa_bseg-belnr.
        gwa_alv-exercicio        = lwa_bseg-gjahr.
        gwa_alv-doc_compensacao  = lwa_bseg-augbl.
        gwa_alv-data_compensacao = lwa_bseg-augdt.

      ENDIF.
    ENDIF.

    READ TABLE gt_1bnf INTO DATA(lwa_1bnf) WITH KEY refkey = lwa_vb-vbeln_va BINARY SEARCH.
    IF gt_1bnf[] IS NOT INITIAL.

      gwa_alv-quantidade         = lwa_1bnf-menge.
      gwa_alv-doc_faturamento    = lwa_1bnf-docnum.
      gwa_alv-data_criacao_fat   = lwa_1bnf-docdat.
      gwa_alv-num_documento      = lwa_1bnf-docnum.
      gwa_alv-data_criacao_doc   = lwa_1bnf-docdat.
      gwa_alv-num_nota_fiscal    = lwa_1bnf-nfnum.
      gwa_alv-valor_liquido      = lwa_1bnf-netwr.

    ENDIF.

    APPEND gwa_alv TO gt_alv.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table    = gt_salv
    CHANGING
      t_table         = gt_alv[] ) .

  gr_functions = gt_salv->get_functions( ).
  gr_functions->set_all( abap_true ).

  gr_columns = gt_salv->get_columns( ).
  gr_columns->set_optimize( abap_true ).

  gr_display = gt_salv->get_display_settings( ).
  gr_display->set_striped_pattern( cl_salv_display_settings=>true ).

  gt_salv->display( ).

ENDFORM.