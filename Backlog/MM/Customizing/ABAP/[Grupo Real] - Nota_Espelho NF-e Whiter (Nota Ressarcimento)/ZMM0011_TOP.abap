*&---------------------------------------------------------------------*
*& Include          ZMM0011_TOP
*&---------------------------------------------------------------------*

*---> Tabelas
TABLES: t001, rbkp, lfa1.

*---> Types.
TYPES: BEGIN OF ty_rbkp,
         belnr  TYPE rbkp-belnr,
         gjahr  TYPE rbkp-gjahr,
         bukrs  TYPE rbkp-bukrs,
         lifnr  TYPE rbkp-lifnr,
         rbstat TYPE rbkp-rbstat,
         xblnr  TYPE rbkp-xblnr,
         rmwwr  TYPE rbkp-rmwwr,
         wmwst1 TYPE rbkp-wmwst1,
       END OF ty_rbkp.

TYPES: BEGIN OF ty_rseg,
         belnr TYPE rseg-belnr,
         gjahr TYPE rseg-gjahr,
         buzei TYPE rseg-buzei,
         matnr TYPE rseg-matnr,
         werks TYPE rseg-werks,
         ebeln TYPE rseg-ebeln,
         ebelp TYPE rseg-ebelp,
         menge TYPE rseg-menge,
         wrbtr TYPE rseg-wrbtr,
       END OF ty_rseg.

TYPES: BEGIN OF ty_1bnfdoc,
         docnum TYPE j_1bnfdoc-docnum,
         doctyp TYPE j_1bnfdoc-doctyp,
         direct TYPE j_1bnfdoc-direct,
         parvw  TYPE j_1bnfdoc-parvw,
         parid  TYPE j_1bnfdoc-parid,
         cancel TYPE j_1bnfdoc-cancel,
         nfenum TYPE j_1bnfdoc-nfenum,
         series TYPE j_1bnfdoc-series,
       END OF ty_1bnfdoc.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         stcd1 TYPE lfa1-stcd1,
         stcd3 TYPE lfa1-stcd3,
         adrnr TYPE lfa1-adrnr,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         mfrpn TYPE mara-mfrpn,
       END OF ty_mara.

TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF ty_t001.

TYPES: BEGIN OF ty_t001w,
         werks      TYPE t001w-werks,
         adrnr      TYPE t001w-adrnr,
         j_1bbranch TYPE t001w-j_1bbranch,
       END OF ty_t001w.

TYPES: BEGIN OF ty_adrc,
         addrnumber TYPE adrc-addrnumber,
         name1      TYPE adrc-name1,
         city1      TYPE adrc-city1,
         city2      TYPE adrc-city2,
         post_code1 TYPE adrc-post_code1,
         street     TYPE adrc-street,
         house_num1 TYPE adrc-house_num1,
         house_num2 TYPE adrc-house_num2,
         region     TYPE adrc-region,
       END OF ty_adrc.

*---> Tabelas internas.
DATA: gt_rbkp      TYPE STANDARD TABLE OF ty_rbkp,
      gt_rseg      TYPE STANDARD TABLE OF ty_rseg,
      gt_1bnfdoc   TYPE STANDARD TABLE OF ty_1bnfdoc,
      gt_lfa1      TYPE STANDARD TABLE OF ty_lfa1,
      gt_mara      TYPE STANDARD TABLE OF ty_mara,
      gt_saida     TYPE STANDARD TABLE OF zstmm_cockpit_devol_danfe,
      gt_t001      TYPE STANDARD TABLE OF ty_t001,
      gt_t001w     TYPE STANDARD TABLE OF ty_t001w,
      gt_adrc_forn TYPE STANDARD TABLE OF ty_adrc,
      gt_adrc_cent TYPE STANDARD TABLE OF ty_adrc,
      gt_produtos  TYPE STANDARD TABLE OF zstmm_cockpit_df_info_produtos.

*---> Estruturas.
DATA: gwa_emitente    TYPE zstmm_cockpit_df_emitente,
      gwa_destiatario TYPE zstmm_cockpit_df_destiatario,
      gwa_totais_nfe  TYPE zstmm_cockpit_df_totais_nfe,
      gwa_info_comp   TYPE zstmm_cockpit_df_info_comp.

*---> Objetos ALV.
DATA: go_alv       TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container,
      gt_fieldcat  TYPE lvc_t_fcat,
      gwa_layout   TYPE lvc_s_layo,
      gwa_variant  TYPE disvariant.

*---> Variavéis.
DATA: gv_nf TYPE rbkp-xblnr.

*---> Constantes.
CONSTANTS: c_a             TYPE c LENGTH 1  VALUE 'A',
           c_c             TYPE c LENGTH 1  VALUE 'C',
           c_i             TYPE c LENGTH 1  VALUE 'I',
           c_1             TYPE c LENGTH 1  VALUE '1',
           c_w             TYPE c LENGTH 1  VALUE 'W',
           c_e             TYPE c LENGTH 1  VALUE 'E',
           c_s             TYPE c LENGTH 1  VALUE 'S',
           c_j             TYPE c LENGTH 1  VALUE 'J',
           c_eq            TYPE c LENGTH 2  VALUE 'EQ',
           c_lf            TYPE c LENGTH 2  VALUE 'LF',
           c_icm1          TYPE c LENGTH 4  VALUE 'ICM1',
           c_ipi1          TYPE c LENGTH 4  VALUE 'IPI1',
           c_ipi2          TYPE c LENGTH 4  VALUE 'IPI2',
           c_ics1          TYPE c LENGTH 4  VALUE 'ICS1',
* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 24.01.2023 - v1[GAP 902/903 Item (4)] - DS4K908183
           c_ics2          TYPE c LENGTH 4  VALUE 'ICS2',
* <<<--- END <<< changed by FBRITO(Felype Brito) 24.01.2023 - v1[GAP 902/903 Item (4)] - DS4K908183
           c_back          TYPE c LENGTH 4  VALUE 'BACK',
           c_canc          TYPE c LENGTH 4  VALUE 'CANC',
           c_exit          TYPE c LENGTH 4  VALUE 'EXIT',
           c_zcont_alv     TYPE c LENGTH 9  VALUE 'ZCONT_ALV',
           c_bt_pdf        TYPE sy-ucomm VALUE 'BT_PDF',
* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
           c_bt_pdf_ressar TYPE sy-ucomm VALUE 'BT_PDF_RESSAR',
* <<<--- END <<< changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
           c_est_alv       TYPE dd02l-tabname VALUE 'ZSTMM_COCKPIT_DEVOL_DANFE'.

*---> Tela de seleção.
SELECTION-SCREEN BEGIN OF BLOCK b_01 WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS: s_bukrs FOR t001-bukrs OBLIGATORY,
                  s_lifnr FOR lfa1-lifnr OBLIGATORY,
                  s_belnr FOR rbkp-belnr NO INTERVALS NO-EXTENSION,
                  s_gjahr FOR rbkp-gjahr NO INTERVALS NO-EXTENSION.
* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN: BEGIN OF LINE.
    PARAMETERS rb_merc RADIOBUTTON GROUP grp1 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(35) TEXT-024.
  SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: BEGIN OF LINE.
    PARAMETERS rb_resar RADIOBUTTON GROUP grp1.
    SELECTION-SCREEN COMMENT 3(35) TEXT-025.
  SELECTION-SCREEN: END OF LINE.
* <<<--- END <<< changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183

SELECTION-SCREEN END OF BLOCK b_01.