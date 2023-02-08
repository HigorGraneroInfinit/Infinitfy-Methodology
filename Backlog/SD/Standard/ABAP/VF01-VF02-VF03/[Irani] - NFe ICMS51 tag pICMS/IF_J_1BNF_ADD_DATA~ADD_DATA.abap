  METHOD if_j_1bnf_add_data~add_data.

************ Devolução de Combustível   ***********
    DATA:
      lv_anp      TYPE          mara-anp,
      lv_menge_kg TYPE          j_1bnflin-menge,
      wa_fuel     TYPE          j_1bnffuel,
      lv_tabix    TYPE          sy-tabix,
      lo_tvarv    TYPE REF TO   zclgl_tvarv,
      r_nftype    TYPE RANGE OF j_1bnfdoc-nftype,
      r_cfop      TYPE RANGE OF j_1bnflin-cfop.

    CONSTANTS:
      c_butano TYPE j_1bnffuel-cprodanp VALUE 210203001.
***********************************************************

    DATA: o_tvarv             TYPE REF TO zclgl_tvarv,
          lr_nftype_nopayment TYPE RANGE OF j_1bnfdoc-nftype,
          lr_nftype_export    TYPE RANGE OF j_1bnfdoc-nftype,
          lr_nftype_exportdev TYPE RANGE OF j_1bnfdoc-nftype.
*** TMS 10/10/2018 - BL353 - Ajuste Layout Danfe - Inicio
    DATA lr_nf_form_pgto TYPE RANGE OF vbrk-fkart.
*** TMS 10/10/2018 - BL353 - Ajuste Layout Danfe - Fim

    "Preenchimento de responsavel tecnico
    DATA: ls_resptec TYPE ztsd_resptec,
          ls_vbrp    TYPE vbrpvb.
***Inicio - SH1K916911 SD:COMPRA PERFORMANCE:ATRIBUIÇÃO EXPORTAÇÃO - Inicio***
    TYPES: BEGIN OF ty_lin1,
             docnum     TYPE j_1bnflin-docnum,
             itmnum     TYPE j_1bnflin-itmnum,
             matnr      TYPE j_1bnflin-matnr,
             menge      TYPE j_1bnflin-menge,
             menge_trib TYPE j_1bnflin-menge_trib,
             refkey     TYPE j_1bnflin-refkey,
             xped       TYPE j_1bnflin-xped,
             cancel     TYPE j_1bnfdoc-cancel,
           END OF ty_lin1,

           BEGIN OF ty_nfref,
             guid_header TYPE oblguid16,
             counter     TYPE j_1bcte_ref_counter,
             nfeid       TYPE j_1b_nfe_access_key,
           END OF ty_nfref.


    DATA: wa_export   TYPE j_1bnfe_s_add_badi_export,
          cont        TYPE i,
          ref_cont    TYPE i,
          lt_vbrp_aux TYPE vbrp_tab,
          lt_lin_aux  TYPE TABLE OF ty_lin1,
          vl_vbelv    TYPE j_1bnflin-refkey,
          ls_acckey   TYPE j_1b_nfe_access_key,
          lt_lin1     TYPE TABLE OF ty_lin1,
          lv_propc    TYPE p DECIMALS 3,
          lt_nfref    TYPE ty_nfref.
***Fim- SH1K916911 SD:COMPRA PERFORMANCE:ATRIBUIÇÃO EXPORTAÇÃO - Fim***
*--------------------------------------------------------------------*

    DATA: lv_tolera_valor_export TYPE p LENGTH 5 DECIMALS 2,
          lv_low                 TYPE tvarvc-low,
          lv_valor               TYPE vbap-kwmeng.

    CREATE OBJECT o_tvarv
      EXPORTING
        object    = 'ZNFE'
        separator = abap_undefined
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      o_tvarv->get_seloption( EXPORTING  ref    = 'NFTYPE_NOPAYMENT'
                              IMPORTING  value  = lr_nftype_nopayment
                              EXCEPTIONS error  = 1
                                         OTHERS = 2 ).

      o_tvarv->get_seloption( EXPORTING  ref    = 'NFTYPE_EXPORT'
                              IMPORTING  value  = lr_nftype_export
                              EXCEPTIONS error  = 1
                                         OTHERS = 2 ).

      o_tvarv->get_seloption( EXPORTING  ref    = 'NFTYPE_EXPORTDEV'
                              IMPORTING  value  = lr_nftype_exportdev
                              EXCEPTIONS error  = 1
                                         OTHERS = 2 ).
    ENDIF.

* 01.05.2022 - Inicio -  Irani_4991
*    es_header-modfrete = SWITCH #( is_header-inco1
*                                   WHEN 'CIF' THEN '1'
*                                   WHEN 'FOB' THEN '9'
*                                   ELSE '0'   ). "Frete por conta (modalidade de frete)
    SELECT SINGLE cod_nfe
      INTO es_header-modfrete
      FROM ztbsd_deparafret
      WHERE inco1 EQ is_header-inco1.
* 01.05.2022 - Fim -  Irani_4991

*   Exportação - Devolução .
    IF et_payment[] IS INITIAL.
      IF lr_nftype_exportdev IS NOT INITIAL
      AND is_header-nftype IN lr_nftype_exportdev.
        APPEND INITIAL LINE TO et_payment ASSIGNING FIELD-SYMBOL(<ls_payment_dev>).
        <ls_payment_dev>-t_pag = '90'. "Sem pagamento.
        CLEAR <ls_payment_dev>-v_pag.
      ENDIF.
    ENDIF.

    IF et_payment[] IS INITIAL.

      APPEND INITIAL LINE TO et_payment ASSIGNING FIELD-SYMBOL(<ls_payment>).
      <ls_payment>-counter = lines( et_payment ).

      DATA(lv_itmtyp) = VALUE #( it_nflin[ 1 ]-itmtyp DEFAULT space ).

      IF lr_nftype_nopayment IS NOT INITIAL
     AND ( is_header-nftype IN lr_nftype_nopayment
        OR lv_itmtyp        EQ '02' ) .
        <ls_payment>-t_pag = '90'. "Sem pagamento.
*      <ls_payment>-tp_integra = '2'.
*      <ls_payment>-ind_pag    = '0'.
      ELSE.
        <ls_payment>-v_pag = is_header-nftot. "<ls_payment>-v_pag + ( is_vbrk-netwr + is_vbrk-mwsbk ).

*** TMS 10/10/2018 - BL353 - Ajuste Layout Danfe - Inicio
**      IF ( is_vbrk-zlsch EQ 'V'
**        OR is_vbrk-zlsch EQ 'Y'
**        OR is_vbrk-zlsch EQ 'B') .
**        <ls_payment>-t_pag = '15'.
**      ELSE.
**        <ls_payment>-t_pag = '99'.
**      ENDIF.

*      SELECT SINGLE zlsch
*        INTO @DATA(vl_zlsch)
*        FROM vbkd
*        WHERE vbeln EQ @is_header-docnum.
*      IF sy-subrc EQ 0.
** De para forma de pagamento
        SELECT SINGLE sefaz_xml
          INTO <ls_payment>-t_pag
          FROM ztbsd_sefaz_depr
          WHERE cod_sap EQ is_vbrk-zlsch.
*      ENDIF.

        <ls_payment>-ind_pag = '0'.
*** TMS 10/10/2018 - BL353 - Ajuste Layout Danfe - Fim
*      <ls_payment>-tp_integra = '2'.
      ENDIF.


** Buscar formas de pagamento
      READ TABLE it_nflin ASSIGNING FIELD-SYMBOL(<fs_nflin>) INDEX 1.
      IF sy-subrc EQ 0.

        SELECT SINGLE fkart
          INTO @DATA(vl_fkart_vbrk)
          FROM vbrk
          WHERE vbeln EQ @<fs_nflin>-refkey(10).
        IF sy-subrc EQ 0.

          CREATE OBJECT o_tvarv
            EXPORTING
              object    = 'ZSD_NFE'
              separator = abap_undefined
            EXCEPTIONS
              error     = 1
              OTHERS    = 2.

          IF sy-subrc EQ 0.
*** Exibir mensagem de erro para as formas de pagamentos cadastradas na tabela TVARV
            o_tvarv->get_seloption( EXPORTING  ref    = 'MSG_FORMA_PGTO'
                                    IMPORTING  value  = lr_nf_form_pgto
                                    EXCEPTIONS error  = 1
                                               OTHERS = 2 ).
            IF vl_fkart_vbrk IN lr_nf_form_pgto.
              IF <ls_payment>-t_pag IS INITIAL.
                MESSAGE e003(zsd01).
              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.
    LOOP AT et_item ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-cean = <ls_item>-cean_trib = 'SEM GTIN'.
    ENDLOOP.

*Aba de Exportação
    IF lr_nftype_export IS NOT INITIAL
   AND is_header-nftype IN lr_nftype_export.
      SELECT vbeln, route
        INTO TABLE @DATA(lt_likp)
        FROM likp
         FOR ALL ENTRIES IN @it_vbrp
       WHERE vbeln EQ @it_vbrp-vgbel.

      IF sy-subrc EQ 0.
        DELETE lt_likp WHERE route IS INITIAL.
        IF lt_likp IS NOT INITIAL.

          SELECT *
            INTO TABLE @DATA(lt_tvrab)
            FROM tvrab
             FOR ALL ENTRIES IN @lt_likp
           WHERE route EQ @lt_likp-route.

          IF sy-subrc EQ 0.
            SELECT *
              INTO TABLE @DATA(lt_tvkn)
              FROM tvkn
               FOR ALL ENTRIES IN @lt_tvrab
             WHERE knote EQ @lt_tvrab-knend.

            IF sy-subrc EQ 0.

              SELECT *
                INTO TABLE @DATA(lt_adrc)
                FROM adrc
                 FOR ALL ENTRIES IN @lt_tvkn
               WHERE addrnumber EQ @lt_tvkn-adrnr.

              IF sy-subrc EQ 0.
                es_header-ufembarq   = lt_adrc[ 1 ]-region.
                es_header-xlocembarq = lt_adrc[ 1 ]-city1.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    IF et_traceability[] IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(lt_mch1)
        FROM mch1
         FOR ALL ENTRIES IN @et_traceability
       WHERE charg EQ @et_traceability-nlote(10).

      IF sy-subrc EQ 0.
        LOOP AT et_traceability ASSIGNING FIELD-SYMBOL(<ls_trace>) WHERE dval IS INITIAL.
          READ TABLE it_nflin INTO DATA(ls_lin) WITH KEY docnum = <ls_trace>-docnum
                                                         itmnum = <ls_trace>-itmnum.
          IF sy-subrc EQ 0.
            READ TABLE lt_mch1 INTO DATA(ls_mch1) WITH KEY matnr = ls_lin-matnr
                                                           charg = <ls_trace>-nlote.
            IF sy-subrc EQ 0.
              <ls_trace>-dfab = ls_mch1-hsdat.
              <ls_trace>-dval = ls_mch1-vfdat.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "Preenchimento de responsavel tecnico
    IF lines( it_vbrp ) > 0.
      READ TABLE it_vbrp INTO ls_vbrp INDEX 1.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM ztsd_resptec
          INTO ls_resptec
          WHERE uf = ls_vbrp-wkreg.

        IF sy-subrc IS INITIAL.
          es_tec_resp-cnpj    = ls_resptec-cnpj.
          es_tec_resp-contact = ls_resptec-contact.
          es_tec_resp-email   = ls_resptec-email.
          es_tec_resp-phone   = ls_resptec-phone.
          es_tec_resp-csrt    = ls_resptec-csrt.

          IF ls_resptec-idcsrt IS NOT INITIAL.
            es_tec_resp-idcsrt = ls_resptec-idcsrt.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE it_nflin ASSIGNING <fs_nflin> INDEX 1.
      IF sy-subrc EQ 0.
        SELECT SINGLE regio
          FROM t001w
          INTO @DATA(vl_regio)
          WHERE werks = @<fs_nflin>-bwkey.

        SELECT SINGLE *
          FROM ztsd_resptec
          INTO ls_resptec
          WHERE uf = vl_regio.

        IF sy-subrc IS INITIAL.
          es_tec_resp-cnpj    = ls_resptec-cnpj.
          es_tec_resp-contact = ls_resptec-contact.
          es_tec_resp-email   = ls_resptec-email.
          es_tec_resp-phone   = ls_resptec-phone.
          es_tec_resp-csrt    = ls_resptec-csrt.

          IF ls_resptec-idcsrt IS NOT INITIAL.
            es_tec_resp-idcsrt = ls_resptec-idcsrt.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

***Inicio - SH1K916911 SD:COMPRA PERFORMANCE:ATRIBUIÇÃO EXPORTAÇÃO - Inicio***
    IF is_vbrk-vtweg = '20'.
      SELECT *
        FROM zsdt_ncm_utrib
        INTO TABLE @DATA(lt_ncm_utrib)
        FOR ALL ENTRIES IN @it_nflin
        WHERE steuc = @it_nflin-nbm.

      IF sy-subrc IS INITIAL.

        SORT lt_ncm_utrib BY steuc.

        LOOP AT it_nflin INTO DATA(ls_item).

          READ TABLE lt_ncm_utrib INTO DATA(ls_ncm_trib)
          WITH KEY steuc = ls_item-nbm
          BINARY SEARCH.

          IF sy-subrc IS INITIAL.

            IF ls_item-meins <> ls_ncm_trib-meins_trib.
              IF  ls_ncm_trib-fatora > 0.
                lv_propc = ls_ncm_trib-fatorb / ls_ncm_trib-fatora.

                READ TABLE et_item ASSIGNING FIELD-SYMBOL(<lfs_es_item>)
                WITH KEY itmnum = ls_item-itmnum.

                IF sy-subrc IS INITIAL.
                  <lfs_es_item>-menge_trib = ls_item-menge * lv_propc.
                  <lfs_es_item>-meins_trib = ls_ncm_trib-meins_trib.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.

    IF is_vbrk-fkart = 'ZPRV'.
      lt_vbrp_aux[] = it_vbrp[].
      SORT lt_vbrp_aux BY aubel.
      DELETE ADJACENT DUPLICATES FROM lt_vbrp_aux COMPARING aubel.

      IF lt_vbrp_aux IS NOT INITIAL.
        SELECT *
          INTO TABLE @DATA(lt_vbak)
          FROM vbak
          FOR ALL ENTRIES IN @lt_vbrp_aux
          WHERE vbeln EQ @lt_vbrp_aux-aubel.

        IF sy-subrc IS INITIAL AND lt_vbak IS NOT INITIAL.
          SELECT a~docnum
                 a~itmnum
                 a~matnr
                 a~menge
                 a~menge_trib
                 a~refkey
                 a~xped
                 b~cancel
          INTO TABLE lt_lin1
          FROM j_1bnflin AS a INNER JOIN
               j_1bnfdoc AS b ON
                a~docnum = b~docnum
          AND   b~direct = 1
          AND   b~nftype = 'YA'
          FOR ALL ENTRIES IN lt_vbak
          WHERE xped EQ lt_vbak-bstnk(15).

          IF sy-subrc IS INITIAL.
            DELETE lt_lin1[] WHERE cancel EQ abap_true.
            lt_lin_aux[] = lt_lin1[].
            SORT lt_lin_aux BY docnum.
            DELETE ADJACENT DUPLICATES FROM lt_lin_aux COMPARING docnum.

            IF lt_lin_aux IS NOT INITIAL.

              SELECT *
              INTO TABLE @DATA(lt_active)
              FROM j_1bnfe_active
              FOR ALL ENTRIES IN @lt_lin_aux
              WHERE docnum EQ @lt_lin_aux-docnum.

              IF lt_active[] IS NOT INITIAL.
                SORT lt_active BY docnum.
              ENDIF.
            ENDIF.
          ENDIF.
          IF et_export[] IS INITIAL.
            LOOP AT it_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>).
              cont = cont + 1.
              ref_cont = cont.
              wa_export-exp_seq = cont.
              wa_export-nre     = 0.
              wa_export-ndraw   = 0.
              READ TABLE lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
                                 WITH KEY vbeln = <fs_vbrp>-aubel.
              IF sy-subrc IS INITIAL.
                READ TABLE lt_lin1 ASSIGNING FIELD-SYMBOL(<fs_lin1>)
                                   WITH KEY       xped  = <fs_vbak>-bstnk(15)
                                                  matnr = <fs_vbrp>-matnr.
                IF sy-subrc IS INITIAL.
                  READ TABLE lt_active ASSIGNING FIELD-SYMBOL(<fs_active>)
                                   WITH KEY   docnum  = <fs_lin1>-docnum.

                  IF sy-subrc IS INITIAL.
                    MOVE-CORRESPONDING <fs_active> TO ls_acckey.
                  ENDIF.

                  wa_export-chnfe   = ls_acckey.
                  wa_export-docnum  = <fs_lin1>-docnum.
                  wa_export-itmnum  = <fs_lin1>-itmnum.
                  wa_export-qexport = <fs_vbrp>-fkimg.
                  APPEND wa_export TO et_export.
                ENDIF.
              ENDIF.

              CLEAR ls_acckey.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF is_vbrk-fkart = 'YRRV'.
      lt_vbrp_aux[] = it_vbrp[].
      SELECT vbelv
        INTO TABLE @DATA(lt_vbfa)
        FROM vbfa
        FOR ALL ENTRIES IN @lt_vbrp_aux
        WHERE vbeln = @lt_vbrp_aux-aubel
        AND vbtyp_v = 'M'.

      LOOP AT lt_vbfa INTO DATA(wa_vbfa_aux).
        vl_vbelv = wa_vbfa_aux-vbelv.
      ENDLOOP.

      SELECT a~docnum
             a~itmnum
             a~matnr
             a~menge
             a~menge_trib
             a~refkey
             a~xped
             b~cancel
      INTO TABLE lt_lin1
      FROM j_1bnflin AS a INNER JOIN
           j_1bnfdoc AS b ON
           a~docnum = b~docnum
      WHERE refkey EQ vl_vbelv.

      IF lt_lin1 IS NOT INITIAL.

        SELECT *
        INTO TABLE lt_active
        FROM j_1bnfe_active
        FOR ALL ENTRIES IN lt_lin1
        WHERE docnum EQ lt_lin1-docnum.
      ENDIF.

      IF et_export[] IS INITIAL.
        LOOP AT it_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp_dev>).
          cont = cont + 1.
          wa_export-exp_seq = cont.
          wa_export-nre     = 0.
          wa_export-ndraw   = 0.

          READ TABLE lt_lin1 ASSIGNING FIELD-SYMBOL(<fs_lin1_dev>)
                                   WITH KEY refkey = vl_vbelv.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_active ASSIGNING FIELD-SYMBOL(<fs_active_dev>)
                             WITH KEY   docnum  = <fs_lin1_dev>-docnum.

            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING <fs_active_dev> TO ls_acckey.
              wa_export-chnfe   = ls_acckey.
              wa_export-docnum  = <fs_lin1_dev>-docnum.
              wa_export-itmnum  = <fs_lin1_dev>-itmnum.
              wa_export-qexport = <fs_vbrp_dev>-fkimg.
              APPEND wa_export TO et_export.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
***Fim- SH1K916911 SD:COMPRA PERFORMANCE:ATRIBUIÇÃO EXPORTAÇÃO - Fim***
************************************************************************
* Autor     : Pelissari / SOFICOM                                      *
* Data      : 01.11.2018                                               *
* Descrição : Desenvolvimento para atender preenchimento de campos     *
*             obrigatórios de pagamento quando lançadas notas de       *
*             transferência via produto SOFICOM-CIAP.                  *
************************************************************************

    CONSTANTS: cl_ciap  TYPE /pgtpa/param_par-modulo VALUE 'CIAP',
               cl_cfop  TYPE /pgtpa/param_par-param1 VALUE 'CFOP',
               cl_tpag  TYPE /pgtpa/param_par-param1 VALUE 'TPAG',
               cl_trans TYPE /pgtpa/param_par-param2 VALUE 'TRANSFERENCIA',
               cl_saida TYPE /pgtpa/param_par-param3 VALUE 'SAIDA'.

    DATA: vl_cfop  TYPE j_1bcfop,
          vl_tpag  TYPE j_1bnfe_tpag,
          wa_nflin LIKE LINE OF it_nflin.

    FIELD-SYMBOLS: <fl_payment> TYPE LINE OF j_1bnfe_t_badi_payment_400.

    CALL FUNCTION '/PGTPA/PARAM_BUSCA_VALORES'
      EXPORTING
        i_modulo = cl_ciap
        i_param1 = cl_cfop
        i_param2 = cl_trans
        i_param3 = cl_saida
      IMPORTING
        e_valor  = vl_cfop.

    IF sy-subrc = 0 AND vl_cfop IS NOT INITIAL.

      LOOP AT it_nflin INTO wa_nflin.

        IF wa_nflin-cfop EQ vl_cfop.

          CALL FUNCTION '/PGTPA/PARAM_BUSCA_VALORES'
            EXPORTING
              i_modulo = cl_ciap
              i_param1 = cl_tpag
            IMPORTING
              e_valor  = vl_tpag.

          IF vl_tpag IS NOT INITIAL.

            READ TABLE et_payment ASSIGNING <fl_payment> WITH KEY docnum = wa_nflin-docnum.

            <fl_payment>-t_pag = vl_tpag.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

************************************************************************
* Autor     : Pelissari / SOFICOM                                      *
* Data      : 09.11.2018                                               *
* Descrição : Desenvolvimento para atender preenchimento do campo de   *
*             obrigatórios de pagamento quando lançadas notas fiscais  *
*             complementares via RFC do TDF                            *
************************************************************************
    CONSTANTS: cl_user_rfc TYPE sy-uname VALUE 'RFC_TDF',
               cl_co       TYPE /pgtpa/param_par-modulo VALUE 'CO'.

    IF sy-uname = cl_user_rfc.
      CLEAR: vl_tpag.

      CALL FUNCTION '/PGTPA/PARAM_BUSCA_VALORES'
        EXPORTING
          i_modulo = cl_co
          i_param1 = cl_tpag
        IMPORTING
          e_valor  = vl_tpag.

      IF vl_tpag IS NOT INITIAL.
        LOOP AT it_nflin INTO wa_nflin.
          READ TABLE et_payment ASSIGNING <fl_payment> WITH KEY docnum = wa_nflin-docnum.
          <fl_payment>-t_pag = vl_tpag.
        ENDLOOP.
      ENDIF.
    ENDIF.


    DATA:
      lr_vbeln TYPE RANGE OF lips-vbeln,
      vl_voleh TYPE          lips-voleh,
      vl_volum TYPE          lips-volum.

* Tratamento das Tags de volume transportado
    SELECT SINGLE sortl
      INTO @DATA(vl_sortl)
      FROM kna1
      WHERE kunnr EQ @is_header-parid.

    IF sy-subrc IS INITIAL.

      lr_vbeln   = VALUE #( FOR ls_remessa IN it_vbrp ( sign = 'I' option = 'EQ' low = ls_remessa-vgbel ) ).
      DELETE ADJACENT DUPLICATES FROM lr_vbeln.

      LOOP AT it_vbrp INTO DATA(wa_vbrp).
        IF wa_vbrp-voleh IS NOT INITIAL.
          vl_voleh = wa_vbrp-voleh.
        ENDIF.
        ADD wa_vbrp-volum TO vl_volum.
      ENDLOOP.

      SELECT SINGLE msehl
        INTO @DATA(vl_especie)
        FROM t006a
        WHERE spras EQ @sy-langu
        AND   msehi EQ @vl_voleh.

      APPEND INITIAL LINE TO et_transvol ASSIGNING FIELD-SYMBOL(<fs_transvol>).

      <fs_transvol>-qvol  = vl_volum.
      <fs_transvol>-marca = vl_sortl.
      <fs_transvol>-pesob = is_header-brgew.
      <fs_transvol>-pesol = is_header-ntgew.
      <fs_transvol>-esp   = vl_especie.

      SORT et_transvol BY docnum counter.
      DELETE ADJACENT DUPLICATES FROM et_transvol COMPARING docnum counter.

    ENDIF.

******************* Devolução de Combustível ********************
    CREATE OBJECT lo_tvarv
      EXPORTING
        object    = 'ZMM'
        separator = '_'
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.

    IF sy-subrc = 0.

*      lo_tvarv->get_seloption( EXPORTING  ref    = 'CFOP_DEV_COMB'
*                               IMPORTING  value  = r_cfop
*                               EXCEPTIONS error  = 1
*                                          OTHERS = 2 ).

      lo_tvarv->get_seloption( EXPORTING  ref    = 'NFTYPE_DEV_COMB'
                               IMPORTING  value  = r_nftype
                               EXCEPTIONS error  = 1
                                          OTHERS = 2 ).
    ENDIF.

    SELECT *
          FROM j_1btanpt
          INTO TABLE @DATA(lt_anp).

    IF is_header-nftype IN r_nftype AND is_header-direct EQ 2.
      LOOP AT it_nflin ASSIGNING FIELD-SYMBOL(<fs_item>).
        lv_tabix = sy-tabix.

        SELECT anp
          INTO lv_anp
          FROM mara
          WHERE matnr = <fs_item>-matnr.
        ENDSELECT.

        IF sy-subrc IS INITIAL.

          READ TABLE lt_anp INTO DATA(wa_anp) WITH KEY j_1banp = lv_anp.
          IF sy-subrc IS INITIAL.
*            IF <fs_item>-cfop IN r_cfop.
            IF NOT lv_anp EQ c_butano.
              wa_fuel-docnum    = <fs_item>-docnum.
              wa_fuel-itmnum    = <fs_item>-itmnum.
              wa_fuel-cprodanp  = wa_anp-j_1banp.
              wa_fuel-ufcons    = is_header-regio.

              MODIFY et_fuel FROM wa_fuel INDEX lv_tabix.

            ELSE.

              SELECT *
                INTO @DATA(wa_perc)
                FROM zmm_perc_anp
                WHERE cprodanp = @wa_anp-j_1banp.
              ENDSELECT.

              IF <fs_item>-meins EQ 'KG'.
                lv_menge_kg = <fs_item>-menge.
              ELSE.

                CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
                  EXPORTING
                    i_matnr              = <fs_item>-matnr
                    i_in_me              = <fs_item>-meins
                    i_out_me             = 'KG'
                    i_menge              = <fs_item>-menge
                  IMPORTING
                    e_menge              = lv_menge_kg
                  EXCEPTIONS
                    error_in_application = 1
                    error                = 2
                    OTHERS               = 3.

              ENDIF.

              wa_fuel-docnum    = <fs_item>-docnum.
              wa_fuel-itmnum    = <fs_item>-itmnum.
              wa_fuel-cprodanp  = wa_anp-j_1banp.
              wa_fuel-ufcons    = is_header-regio.
              wa_fuel-p_glp     = wa_perc-p_glp.
              wa_fuel-p_gnn     = wa_perc-p_gnn.
              wa_fuel-p_gni     = wa_perc-p_gni.
              wa_fuel-v_part    = <fs_item>-netwrt / lv_menge_kg.

              MODIFY et_fuel FROM wa_fuel INDEX lv_tabix.

            ENDIF.
*            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

***************************** FIM Devolução de Combustível **************************************

***************************** INICIO Controle de Saldo de Seguro e Frete *******************************
    DATA: lt_ctrl_saldo_insert TYPE STANDARD TABLE OF zsdt_ctrl_saldo,
          lt_xkomv_f           TYPE STANDARD TABLE OF komv,
          lwa_ctrl_saldo       TYPE zsdt_ctrl_saldo,
          lv_tot_forn          TYPE zsdt_ctrl_saldo-kwmeng_f.

    FIELD-SYMBOLS <fs_xkomv> TYPE ANY TABLE.

    IF r_frete_seg_ov_saida IS INITIAL OR r_frete_seg_ov_devol IS INITIAL.
      SELECT sign opti low high
       FROM tvarvc
       INTO TABLE r_frete_seg_ov_saida
       WHERE name = 'ZSD_TP_OV_FRETE_SEGUR_EXP'.

      SELECT sign opti low high
       FROM tvarvc
       INTO TABLE r_frete_seg_ov_devol
       WHERE name = 'ZSD_TP_OV_FRETE_SEGUR_EXP_DEV'.
    ENDIF.

    IF is_vbrk-fkart IN r_frete_seg_ov_saida OR is_vbrk-fkart IN r_frete_seg_ov_devol.
      ASSIGN ('(SAPMV60A)XKOMV[]') TO <fs_xkomv>.

      IF <fs_xkomv> IS ASSIGNED.
        lt_xkomv_f[] = <fs_xkomv>.

        IF lt_xkomv_f IS NOT INITIAL AND it_vbrp IS NOT INITIAL.
          SELECT *
            FROM zsdt_ctrl_saldo
            INTO TABLE @DATA(tl_ctrl_saldo)
            FOR ALL ENTRIES IN @it_vbrp
            WHERE vbeln_o = @it_vbrp-aubel
              AND posnr_o = @it_vbrp-aupos.

          SELECT vbap~vbeln, vbap~posnr, vbap~kwmeng, vbak~knumv
            FROM vbap
            INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
            INTO TABLE @DATA(lt_vbap)
            FOR ALL ENTRIES IN @it_vbrp
            WHERE vbap~vbeln = @it_vbrp-aubel
              AND vbap~posnr = @it_vbrp-aupos.

          IF lt_vbap IS NOT INITIAL.
            SELECT knumv, kposn, stunr, zaehk, kschl, kwert
              FROM prcd_elements
              INTO TABLE @DATA(lt_xkomv_o)
              FOR ALL ENTRIES IN @lt_vbap
              WHERE knumv = @lt_vbap-knumv
                AND kposn = @lt_vbap-posnr
                AND ( kschl = 'Z141'
                 OR   kschl = 'Z142' ).
          ENDIF.

          SELECT low
            UP TO 1 ROWS
            FROM tvarvc
            INTO lv_low
            WHERE name = 'ZSD_TOLERA_VALOR_EXPORT'.
          ENDSELECT.

          IF sy-subrc IS INITIAL.
            lv_tolera_valor_export = lv_low.
          ENDIF.

          LOOP AT it_vbrp INTO DATA(lwa_vbrp).
            CLEAR: lv_tot_forn, lwa_ctrl_saldo.

            LOOP AT tl_ctrl_saldo INTO DATA(lwa_ctrl_saldo_bd) WHERE vbeln_o = lwa_vbrp-aubel
                                                                 AND posnr_o = lwa_vbrp-aupos.
              ADD lwa_ctrl_saldo_bd-kwmeng_f TO lv_tot_forn.
            ENDLOOP.

            READ TABLE lt_vbap INTO DATA(lwa_vbap) WITH KEY vbeln = lwa_vbrp-aubel
                                                            posnr = lwa_vbrp-aupos.
            IF sy-subrc IS INITIAL.
              lwa_ctrl_saldo-kwmeng_o = lwa_vbap-kwmeng.

              READ TABLE lt_xkomv_o INTO DATA(lwa_xkomv_o) WITH KEY knumv = lwa_vbap-knumv
                                                                    kposn = lwa_vbap-posnr
                                                                    kschl = 'Z141'.
              IF sy-subrc IS INITIAL.
                lwa_ctrl_saldo-v_seg_o = lwa_xkomv_o-kwert. " Valor Seguro da Ordem
              ENDIF.

              READ TABLE lt_xkomv_o INTO lwa_xkomv_o WITH KEY knumv = lwa_vbap-knumv
                                                              kposn = lwa_vbap-posnr
                                                              kschl = 'Z142'.
              IF sy-subrc IS INITIAL.
                lwa_ctrl_saldo-v_frete_o = lwa_xkomv_o-kwert. " Valor Frete da Ordem
              ENDIF.

              lv_valor = lwa_vbap-kwmeng - ( lwa_vbap-kwmeng * lv_tolera_valor_export ).
            ENDIF.

            " Valor Seguro da Fatura
            READ TABLE lt_xkomv_f INTO DATA(lwa_xkomv_f) WITH KEY ('KPOSN') = lwa_vbrp-aupos
                                                                  ('KSCHL') = 'Z141'. " Seguro Fatura
            IF sy-subrc = 0.
              lwa_ctrl_saldo-v_seg_f = lwa_xkomv_f-kwert.
            ENDIF.

            " Valor Frete da Fatura
            READ TABLE lt_xkomv_f INTO lwa_xkomv_f WITH KEY ('KPOSN') = lwa_vbrp-aupos
                                                            ('KSCHL') = 'Z142'. " Frete Fatura
            IF sy-subrc = 0.
              lwa_ctrl_saldo-v_frete_f = lwa_xkomv_f-kwert.
            ENDIF.

            IF is_vbrk-fkart IN r_frete_seg_ov_saida.
              IF ( lv_tot_forn + wa_vbrp-fkimg ) >= lv_valor.
                lwa_ctrl_saldo-final = 'X'.
              ENDIF.
            ENDIF.

            lwa_ctrl_saldo-vbeln_o   = lwa_vbrp-aubel.
            lwa_ctrl_saldo-posnr_o   = lwa_vbrp-aupos.
            lwa_ctrl_saldo-vbeln_f   = is_vbrk-vbeln.
            lwa_ctrl_saldo-posnr_f   = lwa_vbrp-posnr.
            lwa_ctrl_saldo-seq       = 1.
            lwa_ctrl_saldo-kwmeng_f  = lwa_vbrp-fkimg.

            " Se for devolução, grava valor negativo
            IF is_vbrk-fkart IN r_frete_seg_ov_devol.
              lwa_ctrl_saldo-kwmeng_f  = lwa_ctrl_saldo-kwmeng_f  * -1.
              lwa_ctrl_saldo-v_seg_f   = lwa_ctrl_saldo-v_seg_f   * -1.
              lwa_ctrl_saldo-v_frete_f = lwa_ctrl_saldo-v_frete_f * -1.
              CLEAR lwa_ctrl_saldo-final.
            ENDIF.

            APPEND lwa_ctrl_saldo TO lt_ctrl_saldo_insert.
          ENDLOOP.

          INSERT zsdt_ctrl_saldo FROM TABLE lt_ctrl_saldo_insert.
        ENDIF.
      ENDIF.
    ENDIF.
**************************** FIM Controle de Saldo de Seguro e Frete **********************************

**************************** INICIO Preenchimento PICMSDEF **********************************
    IF it_vbrp[] IS NOT INITIAL.

      SELECT vbeln, posnr, knumv_ana
        INTO TABLE @DATA(t_vbap)
        FROM vbap
        FOR ALL ENTRIES IN @it_vbrp
        WHERE vbeln = @it_vbrp-aubel
          AND posnr = @it_vbrp-aupos.
      IF sy-subrc IS INITIAL.

        SELECT knumv, kposn, kbetr
          INTO TABLE @DATA(t_prcd)
          FROM prcd_elements
          FOR ALL ENTRIES IN @t_vbap
          WHERE knumv = @t_vbap-knumv_ana
            AND kposn = @t_vbap-posnr
            AND kschl = 'BX13'.

        IF sy-subrc IS INITIAL.

          LOOP AT et_item ASSIGNING FIELD-SYMBOL(<fs_it>).

            READ TABLE it_vbrp INTO DATA(w_vbrp) WITH KEY posnr = <fs_it>-itmnum.
            IF sy-subrc IS INITIAL.

              READ TABLE t_vbap INTO DATA(w_vbap) WITH KEY vbeln = w_vbrp-aubel posnr = w_vbrp-aupos.
              IF sy-subrc IS INITIAL.

                READ TABLE t_prcd INTO DATA(w_prcd) WITH KEY knumv = w_vbap-knumv_ana kposn = w_vbap-posnr.
                IF sy-subrc IS INITIAL.
                  <fs_it>-picmsdef = w_prcd-kbetr.
                ENDIF.

              ENDIF.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.
**************************** FIM Preenchimento PICMSDEF **********************************

* LYY - 27/12/2018 - Início
    INCLUDE zmmi_002 IF FOUND.
* LYY - 27/12/2018 - Fim

  ENDMETHOD.