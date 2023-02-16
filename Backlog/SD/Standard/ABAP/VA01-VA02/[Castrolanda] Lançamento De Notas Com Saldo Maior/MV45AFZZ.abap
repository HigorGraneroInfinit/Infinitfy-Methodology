***INCLUDE MV45AFZZ .

************************************************************************
*                                                                      *
* This include is reserved for user modifications                      *
*                                                                      *
* Forms for sales document processing                                  *
*                                                                      *
* The name of modification modules should begin with 'ZZ'.             *
*                                                                      *
************************************************************************

*---------------------------------------------------------------------*
*       FORM ZZEXAMPLE                                                *
*---------------------------------------------------------------------*
*       text......................................                    *
*---------------------------------------------------------------------*
*FORM ZZEXAMPLE.

*  ...

*ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM USEREXIT_DELETE_DOCUMENT                                 *
*---------------------------------------------------------------------*
*       This userexit can be used to delete data in additional tables *
*       when a sales document is deleted.                             *
*                                                                     *
*      This form is called in dialog at the end of form BELEG_LOESCHEN*
*      just before form BELEG_SICHERN is performed to delete the      *
*      datas on the database.                                         *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_delete_document.
  "Monitor de carga/pedido de frete
*  INCLUDE zsdi_0049 IF FOUND.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_FIELD_MODIFICATION                              *
*---------------------------------------------------------------------*
*       This userexit can be used to modify the attributes of         *
*       screen fields.                                                *
*       This form is processed for each field in the screen.          *
*                                                                     *
*       The use of the fields screen-group1 to screen-group4 is:      *
*                                                                     *
*       Screen-group1: Automatic modification contolles by transaction*
*                      MFAW.                                          *
*       Screen-group2: Contents 'LOO' for steploop-fields.            *
*       Screen-group3: Used for modififaction, which are dependent on *
*                      control tables or other fix information.       *
*       Screen-group4: Unused                                         *
*                                                                     *
*       For field mofifications, which are dependent on the document  *
*       status, you can use the status field in the workareas         *
*       XVBAP for item status and XVBUK for header status.            *
*                                                                     *
*       This form is called from module FELDAUSWAHL.                  *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_field_modification.

* CASE SCREEN-GROUP3.
*   WHEN '900'.
*     IF VBAK-VBTYP NE IF_SD_DOC_CATEGORY=>INQUIRY.
*       SCREEN-ACTIVE = 0.
*     ENDIF.
* ENDCASE.

* CASE SCREEN-NAME.
*   WHEN 'VBAK-VBELN'.
*     SCREEN-ACTIVE = 0.
* ENDCASE.

  CASE screen-name.
    WHEN 'KUAGV-KUNNR'.
      IF kuagv-kunnr IS NOT INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.

    WHEN 'KUWEV-KUNNR'.

      IF kuwev-kunnr IS NOT INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.

  ENDCASE.

  INCLUDE zsdi_0108 IF FOUND.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBAK                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument header workaerea VBAK.                               *
*                                                                     *
*       SVBAK-TABIX = 0:  Create header                               *
*       SVBAK-TABIX > 0:  Change header                               *
*                                                                     *
*       This form is called at the end of form VBAK_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_move_field_to_vbak.

*  vbak-zzfield = xxxx-zzfield2.

  INCLUDE zsdi_0048 IF FOUND.
ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBAP                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument item workaerea VBAP                                  *
*                                                                     *
*       SVBAP-TABIX = 0:  Create item                                 *
*       SVBAP-TABIX > 0:  Change item                                 *
*                                                                     *
*       This form is called at the end of form VBAP_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_move_field_to_vbap.

  DATA: lt_class    TYPE STANDARD TABLE OF sclass,
        lt_objtable TYPE STANDARD TABLE OF clobjdat,
        lt_lines    TYPE STANDARD TABLE OF tline,
        lt_range    TYPE rstt_t_range_string,
        lt_data	    TYPE TABLE OF zsds_0116.

  DATA: lt_tpov       TYPE RANGE OF vbak-auart,
        lt_shelf_orgv TYPE RANGE OF vbak-vkorg.

  DATA: ls_shelf_orgv LIKE LINE OF lt_shelf_orgv.

  DATA: l_object     TYPE ausp-objek,
        l_tdname     TYPE thead-tdname,
        l_posnr_agro TYPE kposnr,
        l_posnr_vetr TYPE kposnr.

  CONSTANTS: lc_class_agro    TYPE klah-class     VALUE 'Z_AGROQUIMICOS',
             lc_class_vetr    TYPE klah-class     VALUE 'Z_LOJA_AGROPEC',
             lc_type          TYPE klah-klart     VALUE '001',
             lc_tdid          TYPE thead-tdid     VALUE 'Z003',
             lc_object        TYPE thead-tdobject VALUE 'VBBK',
             lc_tv_rcagro     TYPE rvari_vnam     VALUE 'ZSD_CLASSIF_RECEIT_AGRO',
             lc_tv_veteri     TYPE rvari_vnam     VALUE 'ZSD_CLASSIF_RECEIT_VETR',
             lc_tv_sing       TYPE rsscr_kind     VALUE 'P',
             lc_tv_tpov_agron TYPE rvari_vnam     VALUE 'ZSD_TPOV_REC_AGRON',
             lc_tv_shelf_orgv TYPE rvari_vnam     VALUE 'ZSD_SHELFLIFE_ORGV',
             lc_typ_mult      TYPE rsscr_kind     VALUE 'S',
             lc_sign          TYPE c              VALUE 'I',
             lc_option        TYPE char2          VALUE 'EQ'.

  CONSTANTS: lc_car_rec_vet TYPE clobjdat-atnam VALUE 'IE_RECEITA_VETERINARIA',
             lc_car_rec_agr TYPE clobjdat-atnam VALUE 'IE_RECEITA'.

*VBAP-XXXXX = XXXXXXx

  IF ( vbak-vkorg = 'C135' OR
       vbak-vkorg = 'C136' ).

    CASE vbap-bwtar.
      WHEN 'REVENDA'.
        vbap-ktgrm = 'Z2'.
      WHEN 'PRODUÇÃO'.
        vbap-ktgrm = 'Z1'.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

  " Alerta Receita Veterinária e Receituário Agronômico
  l_object = vbap-matnr.

  DEFINE lm_classif.

    REFRESH: lt_class, lt_objtable.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
        class              = &1
        classtype          = lc_type
        object             = l_object
      TABLES
        t_class            = lt_class
        t_objectdata       = lt_objtable
      EXCEPTIONS
        no_classification  = 1
        no_classtypes      = 2
        invalid_class_type = 3
        OTHERS             = 4.

  END-OF-DEFINITION.

  lm_classif lc_class_vetr.

  IF lt_objtable[] IS NOT INITIAL.

    LOOP AT lt_objtable ASSIGNING FIELD-SYMBOL(<ls_objes>)
                        WHERE atnam = lc_car_rec_vet.
      IF <ls_objes>-ausp1 EQ 'SIM' OR <ls_objes>-ausp1 EQ 'S'.
        MESSAGE i001(zsd_receituario) WITH vbap-posnr.
      ENDIF.
    ENDLOOP.

  ENDIF.

  SELECT sign
         opti
         low
         high
    INTO TABLE lt_tpov
    FROM tvarvc
   WHERE type EQ 'S'
     AND name EQ lc_tv_tpov_agron.

  IF sy-subrc   EQ 0
 AND vbak-auart IN lt_tpov.

    lm_classif lc_class_agro.

    IF lt_objtable[] IS NOT INITIAL.

      LOOP AT lt_objtable TRANSPORTING NO FIELDS
                          WHERE atnam = lc_car_rec_agr
                            AND ( ausp1 = 'SIM'
                             OR ausp1 = 'S' ).
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.

        l_tdname = vbap-vbeln.

        REFRESH lt_lines.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            id                      = lc_tdid
            language                = sy-langu
            name                    = l_tdname
            object                  = lc_object
            archive_handle          = 0
          TABLES
            lines                   = lt_lines
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF ( sy-subrc <> 0 )
        OR ( sy-subrc = 0 AND
             lt_lines[] IS INITIAL ).

          FREE lt_data[].

          CALL FUNCTION 'ZSDF_GET_ITEM_RECEITUARIO_OV'
            TABLES
              t_data = lt_data.

          READ TABLE lt_data
            WITH KEY matnr = vbap-matnr
                     TRANSPORTING NO FIELDS.

          IF sy-subrc IS NOT INITIAL.

            MESSAGE i003(zsd_receituario) WITH vbap-matnr.

            CALL FUNCTION 'ZSDF_SET_ITEM_RECEITUARIO_OV'
              EXPORTING
                i_matnr = vbap-matnr.

          ENDIF.

        ELSE.

          LOOP AT lt_lines TRANSPORTING NO FIELDS
                            WHERE tdline IS NOT INITIAL.
            EXIT.
          ENDLOOP.

          IF sy-subrc <> 0.

            FREE lt_data[].

            CALL FUNCTION 'ZSDF_GET_ITEM_RECEITUARIO_OV'
              TABLES
                t_data = lt_data.

            READ TABLE lt_data
              WITH KEY matnr = vbap-matnr
                       TRANSPORTING NO FIELDS.

            IF sy-subrc IS NOT INITIAL.

              MESSAGE i003(zsd_receituario) WITH vbap-matnr.

              CALL FUNCTION 'ZSDF_SET_ITEM_RECEITUARIO_OV'
                EXPORTING
                  i_matnr = vbap-matnr.

            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZF_TVARV'
    EXPORTING
      i_name    = lc_tv_shelf_orgv
      i_type    = lc_typ_mult
    IMPORTING
      et_range  = lt_range
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_range ASSIGNING FIELD-SYMBOL(<ls_range>).
      ls_shelf_orgv-sign   = lc_sign.
      ls_shelf_orgv-option = lc_option.
      ls_shelf_orgv-low    = <ls_range>-low.
      APPEND ls_shelf_orgv TO lt_shelf_orgv.
      CLEAR ls_shelf_orgv.
    ENDLOOP.
  ENDIF.

  IF lt_shelf_orgv[] IS NOT INITIAL.

    IF vbak-vkorg IN lt_shelf_orgv
   AND vbap-mvgr3 IS INITIAL
   AND vbak-bsark NE 'ZS3G'.

      IF vbak-bsark EQ 'NEOG'.
        vbap-mvgr3 = '01'.

      ELSEIF vbak-bsark IS INITIAL.
        vbap-mvgr3 = '01'.

      ENDIF.
    ENDIF.

  ENDIF.

  "WF01{ - SPRO Força de Vendas
  INCLUDE zsdi_0109 IF FOUND.
  "WF01}
ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBEP                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument schedule line workaerea VBEP                         *
*                                                                     *
*       SVBEP-TABIX = 0:  Create schedule line                        *
*       SVBEP-TABIX > 0:  Change schedule line                        *
*                                                                     *
*       This form is called at the end of form VBEP_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_move_field_to_vbep.

*  VBEP-zzfield = xxxx-zzfield2.

***>>>> 8000006884, OV ZVCO - Venda a Ordem
***>>>> 8000007017, GERAÇÃO DE OV EM DUPLICIDADE - ZRME
****  DATA: l_pliq    LIKE ekpo-menge,
****        l_ntgew   LIKE mara-ntgew,
****        l_brgew   LIKE mara-brgew,
****        vg_ordzeco TYPE vbak-vbeln,
****        vg_rfmng  TYPE vbfa-rfmng,
****        vg_meins  TYPE vbfa-meins,
****        vg_abstk  TYPE vbuk-abstk,
****        vg_gbstk  TYPE vbuk-gbstk,
****        vg_fkimg  TYPE vbrp-fkimg,
****        vg_vrkme  TYPE vbrp-vrkme,
****        vg_menge  LIKE ekpo-menge.
****
**** IF ( vbak-auart = 'ZECO' OR
****      vbak-auart = 'ZREE' ).
****    SELECT SINGLE fkimg vrkme INTO (vg_fkimg, vg_vrkme) FROM vbrp
****      WHERE vbeln = vbap-vgbel
****        AND posnr = vbap-vgpos.
****
******* >> Procurando se houve ordens de ZECO.
****    SELECT vbeln rfmng meins INTO (vg_ordzeco, vg_rfmng, vg_meins) FROM vbfa
****      WHERE vbelv = vbap-vgbel
****        AND vbtyp_n = 'C'
****        AND vbtyp_v = 'M'
****        AND posnv = vbap-vgpos.
****      SELECT SINGLE abstk gbstk INTO (vg_abstk, vg_gbstk) FROM vbuk
****         WHERE vbeln = vg_ordzeco.
****      IF vg_abstk <> 'C'.
****        vg_fkimg = vg_fkimg - vg_rfmng.
****      ENDIF.
****    ENDSELECT.
******* >> Fim Procurando se houve ordens de ZECO.
****
*****    IF rv45a-kwmeng = 0 OR
*****       rv45a-kwmeng > vg_fkimg.
****    IF vg_fkimg > 0.
****      vbep-cmeng  = vg_fkimg.
****      vbep-lmeng  = vg_fkimg.
****      cvbep-wmeng = vg_fkimg.
****      vbep-wmeng  = vg_fkimg.
****      vg_menge = vg_fkimg.
****      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
****        EXPORTING
****          i_matnr              = vbap-matnr
****          i_in_me              = vbap-vrkme
****          i_out_me             = vbap-gewei
****          i_menge              = vg_menge
****        IMPORTING
****          e_menge              = l_pliq
****        EXCEPTIONS
****          error_in_application = 1
****          error                = 2
****          OTHERS               = 3.
****      IF sy-subrc = 0.
****        vbap-ntgew = l_pliq.
****        SELECT SINGLE ntgew brgew FROM mara INTO (l_ntgew, l_brgew)
****            WHERE matnr = vbap-matnr.
****        vbap-ntgew = l_pliq.
****        vbap-brgew = l_pliq / l_ntgew * l_brgew.
****      ELSE.
****        SELECT SINGLE ntgew brgew FROM mara INTO (l_ntgew, l_brgew)
****            WHERE matnr = vbap-matnr.
****        vbap-ntgew = ( l_ntgew * vg_rfmng ) / 1000.
****        vbap-brgew = ( l_brgew * vg_rfmng ) / 1000.
****      ENDIF.
****    ELSE.
****      MESSAGE s000(zsd) WITH 'Item já fornecido Integralmente: ' vbap-posnr
****                  DISPLAY LIKE 'I'.
****    ENDIF.
****  ENDIF.

***<<<< Fim 8000006884, OV ZVCO - Venda a Ordem

  "Suzuki/Nova - 02/09/2021 - Quant. disp. Remessa venda futura.
  INCLUDE zsdi_0099 IF FOUND.

  "Higor - 18/01/2023 - Já existem notas de crédito
  INCLUDE ZSDI_0121 IF FOUND.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_MOVE_FIELD_TO_VBKD                              *
*---------------------------------------------------------------------*
*       This userexit can be used to move some fields into the sales  *
*       dokument business data workaerea VBKD                         *
*                                                                     *
*       SVBKD-TABIX = 0:  Create data                                 *
*       SVBKD-TABIX > 0:  Change data                                 *
*                                                                     *
*       This form is called at the end of form VBKD_FUELLEN.          *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_move_field_to_vbkd.

  DATA: lt_zz_callstack TYPE abap_callstack.

*  VBKD-zzfield = xxxx-zzfield2.

  INCLUDE zsdi_00021 IF FOUND.

  IF vbkd-zlsch = ''.
    SELECT SINGLE zwels
             INTO vbkd-zlsch
             FROM knb1
            WHERE kunnr = vbak-kunnr
              AND bukrs = vbak-bukrs_vf.
  ENDIF.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack = lt_zz_callstack.

  " Processamento via LTMC
  LOOP AT lt_zz_callstack TRANSPORTING NO FIELDS WHERE mainprogram CS '/1LT/'.
    EXIT.
  ENDLOOP.

  IF sy-subrc IS INITIAL.
    IF vbkd-bstkd IS NOT INITIAL.
      CONDENSE vbkd-bstkd NO-GAPS.
      vbkd-valdt = vbkd-bstkd+6(4) && vbkd-bstkd+3(2) && vbkd-bstkd(2).
      CLEAR vbkd-bstkd.
    ENDIF.
  ENDIF.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_NUMBER_RANGE                                    *
*---------------------------------------------------------------------*
*       This userexit can be used to determine the numberranges for   *
*       the internal document number.                                 *
*                                                                     *
*       US_RANGE_INTERN - internal number range                       *
*                                                                     *
*       This form is called from form BELEG_SICHERN                   *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_number_range USING us_range_intern.

* Example: Numer range from TVAK like in standard
* US_RANGE_INTERN = TVAK-NUMKI.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_PRICING_PREPARE_TKOMK                           *
*---------------------------------------------------------------------*
*       This userexit can be used to move additional fields into the  *
*       communication table which is used for pricing:                *
*                                                                     *
*       TKOMK for header fields                                       *
*                                                                     *
*       This form is called from form PREISFINDUNG_VORBEREITEN.       *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_pricing_prepare_tkomk.

  DATA l_zzrepvenda         TYPE kunnr.
  DATA l_classif_preco      TYPE zsdt_0004-cd_classif_preco.
  DATA l_zzgrp_kvgr2        TYPE komk-zzgrp_kvgr2.
  DATA lw_xvbpa             LIKE xvbpa.
  FIELD-SYMBOLS: <lfs_komk> LIKE tkomk.


  INCLUDE zsdi_0056 IF FOUND.

  CLEAR: l_zzrepvenda, l_classif_preco.

*** Tipologia de Cliente/Grupo de Cliente 1
  tkomg-kvgr1 = tkomk-kvgr1 = vbak-kvgr1.

*** Tipologia de Cliente/Grupo de Cliente 2
  tkomg-kvgr2 = tkomk-kvgr2 = vbak-kvgr2.

*** Tipologia de Cliente/Grupo de Cliente 2
  tkomg-kvgr3 = tkomk-kvgr3 = vbak-kvgr3.

*** Código Safra
  tkomg-zzcd_safra = tkomk-zzcd_safra = vbak-zzcd_safra.

*** Classificação do Preço
* Este campo está relacionado com a cultura principal da safra,
* cadastrado na tabela ZSDT_0005.
  IF vbak-zzcd_atividade_princ IS NOT INITIAL.
    SELECT SINGLE cd_classif_preco
             INTO l_classif_preco
             FROM zsdt_0005
            WHERE cd_cultura       = vbak-zzcd_cultura
              AND cd_cultura_princ = vbak-zzcd_cultura_princ.
  ENDIF.

  tkomg-cd_classif_preco = tkomk-cd_classif_preco = l_classif_preco.

*** Representante de venda
  READ TABLE xvbpa INTO lw_xvbpa
               WITH KEY parvw = 'Z2'.
  IF sy-subrc = 0.
    IF lw_xvbpa-lifnr <> ''.
      l_zzrepvenda = lw_xvbpa-lifnr.
    ELSEIF lw_xvbpa-kunnr <> ''.
      l_zzrepvenda = lw_xvbpa-kunnr.
    ENDIF.
  ENDIF.

  tkomg-zzrepvenda = tkomk-zzrepvenda = l_zzrepvenda.

* Grupo de tipologia de clientes
  IF vbak-kvgr2 <> ''.
    SELECT SINGLE grp_kvgr2
             INTO l_zzgrp_kvgr2
             FROM zsdt_0090
            WHERE kvgr2 = vbak-kvgr2.
  ENDIF.

  tkomg-zzgrp_kvgr2 = tkomk-zzgrp_kvgr2 = l_zzgrp_kvgr2.

*** Atualização da tabela de registros de condições
  IF tkomk[] IS NOT INITIAL.
    READ TABLE tkomk INDEX 1 ASSIGNING <lfs_komk>. "nível cabeçalho (index 1)
    IF <lfs_komk> IS ASSIGNED AND
       sy-subrc = 0.
      <lfs_komk>-kvgr1            = vbak-kvgr1.
      <lfs_komk>-kvgr2            = vbak-kvgr2.
      <lfs_komk>-kvgr3            = vbak-kvgr3.
      <lfs_komk>-zzcd_safra       = vbak-zzcd_safra.
      <lfs_komk>-zzrepvenda       = l_zzrepvenda.
      <lfs_komk>-zzgrp_kvgr2      = l_zzgrp_kvgr2.
      <lfs_komk>-cd_classif_preco = l_classif_preco.
      <lfs_komk>-zzhp1            = vbap-prodh(2).
      <lfs_komk>-zzhp2            = vbap-prodh(5).
      <lfs_komk>-zzhp3            = vbap-prodh(9).
    ENDIF.
  ENDIF.

* Marca variável global para execução de formula "RV64A909".
  CALL FUNCTION 'ZSDF_UTIL_CHECK_VAR_GLOBAL'
    EXPORTING
      i_flag = abap_true
      i_read = abap_false.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_PRICING_PREPARE_TKOMP                           *
*---------------------------------------------------------------------*
*       This userexit can be used to move additional fields into the  *
*       communication table which is used for pricing:                *
*                                                                     *
*       TKOMP for item fields                                         *
*                                                                     *
*       This form is called from form PREISFINDUNG_VORBEREITEN.       *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_pricing_prepare_tkomp.
  DATA: l_div     TYPE kwmeng,
        l_div_aux TYPE kwmeng.
  DATA: l_result TYPE netwr_ap.
  "BEGIN 13.07.2022 13:04:51 - RODRIGOMS - DEMANDA: RS-1318592, Melhorias SAP - AJUSTE BASE DE CALCULO DESCONTO FRETE
  zcl_parametros=>get_single_value_zxxparam(
  EXPORTING
           iv_modulo = 'SD'
           iv_param1 = 'MV45AFZZ'
           iv_param2 = 'DESCONTO'
           iv_param3 = 'ZDVO'
           iv_param4 = 'ATIVA_VALIDACAO'
  IMPORTING
    ev_value = DATA(l_ativa_validacao) ).
  IF l_ativa_validacao IS NOT INITIAL AND xkomv[] IS NOT INITIAL.

    zcl_parametros=>get_values_zxxparam(
      EXPORTING
          iv_modulo = 'SD'
          iv_param1 = 'MV45AFZZ'
          iv_param2 = 'DESCONTO'
          iv_param3 = 'ZDVO'
          iv_param4 = 'CENTRO'
      IMPORTING
          er_values = DATA(rl_centro) ).

    DATA: tl_konm TYPE TABLE OF konm.
    SELECT a~knumh a~kopos a~klfn1 a~kstbm a~kbetr
      INTO CORRESPONDING FIELDS OF TABLE tl_konm
      FROM konm AS a
      INNER JOIN a925 AS b
      ON a~knumh EQ b~knumh
      WHERE b~kappl EQ 'V'
        AND b~kschl EQ 'ZDVO'
        AND b~vkorg EQ 'C130'
        AND b~spart EQ 'RA'
        AND b~datab LE sy-datum
        AND b~datbi GE sy-datum
        AND b~werks IN rl_centro[].
    IF sy-subrc IS INITIAL.
      SORT tl_konm BY kstbm DESCENDING.
      "Vou somar a quantidade dos itens para verificar se tem desconto
      DATA: l_kwmeng TYPE kwmeng,
            l_kbetr  TYPE kbetr.
      LOOP AT xvbap INTO DATA(wl_xvbap) WHERE werks IN rl_centro[].
        l_kwmeng = l_kwmeng + wl_xvbap-kwmeng.
      ENDLOOP.

      LOOP AT tl_konm ASSIGNING FIELD-SYMBOL(<fs_konm>) WHERE kstbm <= l_kwmeng  . "Preciso buscar qual o valor da escala
        l_kbetr = <fs_konm>-kbetr.
        EXIT.
      ENDLOOP.

      "Se tenho um valor da escala vou atribuir para todos os itens da ZDVO
      IF l_kbetr IS NOT INITIAL.
        CLEAR: wl_xvbap ,l_div.
        l_div = '1000.000'.
        LOOP AT xvbap ASSIGNING FIELD-SYMBOL(<fs_xvbap>) WHERE werks IN rl_centro[].
          READ TABLE xkomv ASSIGNING FIELD-SYMBOL(<fs_xkomv>) WITH KEY kschl = 'ZDVO' kposn = <fs_xvbap>-posnr .
          IF sy-subrc IS INITIAL.
            <fs_xkomv>-kbetr = l_kbetr.
            IF vbap-kwmeng IS NOT INITIAL.
              <fs_xkomv>-kwert   = ( ( <fs_xvbap>-kwmeng / l_div ) * <fs_xkomv>-kbetr ).
              <fs_xkomv>-kwert_k = <fs_xkomv>-kwert.
            ENDIF.
            "Preciso atualizar o valor na linha do item
            READ TABLE xkomv INTO DATA(wl_ztac) WITH KEY kschl = 'ZTAC' kposn = <fs_xvbap>-posnr .
            IF sy-subrc IS INITIAL.
              READ TABLE xkomv INTO DATA(wl_ztao) WITH KEY kschl = 'ZTAO' kposn = <fs_xvbap>-posnr .
              IF sy-subrc IS INITIAL.
                READ TABLE xkomv INTO DATA(wl_zprm) WITH KEY kschl = 'ZPRM' kposn = <fs_xvbap>-posnr .
                IF wl_ztac-kawrt IS NOT INITIAL.
*                  <fs_xvbap>-netwr = ( ( wl_ztac-kawrt + wl_ztao-kawrt ) * ( wl_zprm-kbetr / 100 ) ) + <fs_xkomv>-kwert.
                  <fs_xvbap>-netwr = ( wl_ztac-kawrt + wl_ztao-kawrt  ) + <fs_xkomv>-kwert.
                  DATA: l_valor1 TYPE netwr_ap,
                        l_valor2 TYPE netwr_ap,
                        l_valor3 TYPE netwr_ap.
                  l_valor1 = <fs_xvbap>-netwr.
                  l_div_aux = '1.000'.
                  l_valor2 = <fs_xvbap>-kwmeng / l_div_aux.

                  TRY.
                      l_valor3 = l_valor1 / l_valor2.
                    CATCH cx_sy_zerodivide.
                  ENDTRY.
                  <fs_xvbap>-netpr = l_valor3.

                  CLEAR l_result.
                  IF <fs_xvbap>-posnr EQ vbap-posnr.
                    vbap-netwr = <fs_xvbap>-netwr.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT xvbap ASSIGNING <fs_xvbap> WHERE werks IN rl_centro[].
          READ TABLE xkomv ASSIGNING <fs_xkomv> WITH KEY kschl = 'ZDVO' kposn = <fs_xvbap>-posnr .
          IF sy-subrc IS INITIAL.
            CLEAR:<fs_xkomv>-kbetr,
            <fs_xkomv>-kwert,
            <fs_xkomv>-kwert_k.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  "END 13.07.2022 13:04:59 - RODRIGOMS

  "BEGIN 09.11.2022 08:06:32 - RODRIGOMS - DEMANDA: RS-1408290, Melhorias SAP - Desconto Fidelidade por atividade e hierarquia
  zcl_parametros=>get_single_value_zxxparam(
         EXPORTING
             iv_modulo = 'SD'
             iv_param1 = 'RV64A906'
             iv_param2 = 'DESCONTO_FIDELIDADE'
             iv_param3 = 'ATIVA_DESATIVA'
             IMPORTING
               ev_value = DATA(l_ativa_desativa) ).
  IF ( l_ativa_desativa IS NOT INITIAL ) AND ( sy-tcode EQ 'VA01' OR sy-tcode EQ 'VA02').
    DATA: wl_zsdt_0181 TYPE zsdt_0181.

    SELECT SINGLE *
      INTO wl_zsdt_0181
      FROM zsdt_0181
      WHERE kunnr     EQ vbak-kunnr
        AND atividade EQ vbak-zzcd_atividade
        AND prodh     EQ vbap-prodh
        AND desconto  EQ abap_true.

    IF sy-subrc IS NOT INITIAL. "O desconto só será aplicado para os cadastrados na SM30
      LOOP AT xkomv ASSIGNING FIELD-SYMBOL(<fs_komv>) WHERE kschl = 'ZFI2'.
        CLEAR: <fs_komv>-kawrt,
               <fs_komv>-kbetr,
               <fs_komv>-kwert,
               <fs_komv>-kawrt_k,
               <fs_komv>-kwert_k.
      ENDLOOP.
    ENDIF.
  ENDIF.
  "END 09.11.2022 08:06:32 - RODRIGOMS

*  tkomp-prsok = abap_true.

  INCLUDE zsdi_0037 IF FOUND. "Industrialização

  IF vbap-matnr <> ''.
    SELECT SINGLE spart
             INTO tkomp-spart
             FROM mara
            WHERE matnr = vbap-matnr.
  ENDIF.

  SELECT SINGLE steuc
           INTO tkomp-steuc
           FROM marc
          WHERE matnr = vbap-matnr
            AND werks = vbap-werks.

*  SELECT SINGLE extwg
*           INTO tkomp-extwg
*           FROM mara
*          WHERE matnr = vbap-matnr.

  tkomp-zzhp1   = tkomp-prodh(2).
  tkomp-zzhp2   = tkomp-prodh(5).
  tkomp-zzhp3   = tkomp-prodh(9).
  tkomp-zzpstyv = tkomp-pstyv.

  IF vbap-stkey = abap_true AND
     vbap-uepos <> ''.
    tkomp-zzmatnr_kit = vbap-upmat.
  ENDIF.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_READ_DOCUMENT                                   *
*---------------------------------------------------------------------*
*       This userexit can be used to read data in additional tables   *
*       when the program reads a sales document.                      *
*                                                                     *
*       This form is called at the end of form BELEG_LESEN.           *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_read_document.

ENDFORM.
*eject

*---------------------------------------------------------------------*
*       FORM USEREXIT_SAVE_DOCUMENT                                   *
*---------------------------------------------------------------------*
*       This userexit can be used to save data in additional tables   *
*       when a document is saved.                                     *
*                                                                     *
*       If field T180-TRTYP contents 'H', the document will be        *
*       created, else it will be changed.                             *
*                                                                     *
*       This form is called at from form BELEG_SICHERN, before COMMIT *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_save_document.

* Example:
* CALL FUNCTION 'ZZ_EXAMPLE'
*      IN UPDATE TASK
*      EXPORTING
*           ZZTAB = ZZTAB.

* Pedidos FRISIA
  INCLUDE zsdi_0018 IF FOUND.

* Monitor de aprovação de ordens
  INCLUDE zsdi_0030 IF FOUND.

* Ticket fábrica Ração (interface nexxus)
  INCLUDE zsdi_0026 IF FOUND.

* Ticket interface SIM3G
  INCLUDE zsdi_0033 IF FOUND.

* Monitor de carga/ped. frete - devolução
  INCLUDE zsdi_0050 IF FOUND.

* Corrige atividade/cultura principal - EGR
  INCLUDE zsdi_0053 IF FOUND.

* Taxas financeiras
  INCLUDE zsdi_0064 IF FOUND.

*SPRO(NM) - 8000033790, [9728] Criação de Validação ZVSE {
* Validação ZVSE
  INCLUDE zsdi_0111 IF FOUND.
*SPRO(NM) - 8000033790, [9728] Criação de Validação ZVSE }


ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM USEREXIT_SAVE_DOCUMENT_PREPARE                           *
*---------------------------------------------------------------------*
*       This userexit can be used for changes or checks, before a     *
*       document is saved.                                            *
*                                                                     *
*       If field T180-TRTYP contents 'H', the document will be        *
*       created, else it will be changed.                             *
*                                                                     *
*       This form is called at the beginning of form BELEG_SICHERN    *
*                                                                     *
*---------------------------------------------------------------------*
FORM userexit_save_document_prepare.

* Determinação movimento QM
  INCLUDE zsdi_0007 IF FOUND.

* Interface FRISIA
  INCLUDE zsdi_0017 IF FOUND.

* Valida preenchimento de campos obrigatórios
  INCLUDE zsdi_0020 IF FOUND.

* Corrige atividade/cultura principal - EGR
  INCLUDE zsdi_0053 IF FOUND.

*  INCLUDE zsdi_0026 IF FOUND."LO22

* Validações genéricas ordem de venda
  INCLUDE zsdi_0079 IF FOUND.

* Suzuki/27/10/2020 - Valida qtde ped.frete
  INCLUDE zsdi_0089 IF FOUND.

  "  Suzuki-19/08/2021 - Venda futura, checa saldo
  INCLUDE zsdi_0097 IF FOUND.

* Exceção BP
  INCLUDE zsdi_0038 IF FOUND.

* NOVA Início alteração - erafaelac - 18.11.2021
  INCLUDE zsdi_0102 IF FOUND.
* NOVA Final  alteração - erafaelac - 18.11.2021

  "Suzuki/NOVA-08/12/2021
  "Chamado 8000007974 - Condição de pagt leite
  INCLUDE zfii_0055 IF FOUND.

  INCLUDE zsdi_0118 IF FOUND.

ENDFORM.
*eject