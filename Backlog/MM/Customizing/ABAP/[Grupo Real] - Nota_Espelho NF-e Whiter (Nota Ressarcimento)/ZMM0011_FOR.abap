*&---------------------------------------------------------------------*
*& Include          ZMM0011_FOR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_busca_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_busca_dados .

  DATA: lr_nfenum TYPE RANGE OF j_1bnfdoc-nfenum,
        lr_docnum TYPE RANGE OF j_1bnflin-docnum,
        lr_series TYPE RANGE OF j_1bnfdoc-series.

  DATA: lwa_nfenum LIKE LINE OF lr_nfenum,
        lwa_series LIKE LINE OF lr_series.

  DATA: lv_posicao TYPE i,
        lv_qtdpos  TYPE i.
************************************************************************

  SELECT belnr
         gjahr
         bukrs
         lifnr
         rbstat
         xblnr
         rmwwr
         wmwst1
    INTO TABLE gt_rbkp
    FROM rbkp
    WHERE bukrs  IN s_bukrs AND
          lifnr  IN s_lifnr AND
          belnr  IN s_belnr AND
          gjahr  IN s_gjahr AND
          rbstat IN ( c_a, c_c ).

  IF gt_rbkp[] IS NOT INITIAL.
    SORT: gt_rbkp BY belnr gjahr bukrs lifnr rbstat.
    DATA(lt_rbkp_fae) = gt_rbkp[].
    SORT: lt_rbkp_fae BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_rbkp_fae COMPARING lifnr.

    SELECT lifnr
           name1
           stcd1
           stcd3
           adrnr
      INTO TABLE gt_lfa1
      FROM lfa1
      FOR ALL ENTRIES IN lt_rbkp_fae
      WHERE lifnr EQ lt_rbkp_fae-lifnr.

    IF gt_lfa1[] IS NOT INITIAL.
      SORT: gt_lfa1 BY lifnr.
      DATA(lt_lfa1_fae) = gt_lfa1[].
      SORT: lt_lfa1_fae BY adrnr.
      DELETE ADJACENT DUPLICATES FROM lt_lfa1_fae COMPARING adrnr.

      SELECT addrnumber
             name1
             city1
             city2
             post_code1
             street
             house_num1
             house_num2
             region
        INTO TABLE gt_adrc_forn
        FROM adrc
        FOR ALL ENTRIES IN lt_lfa1_fae
        WHERE addrnumber EQ lt_lfa1_fae-adrnr.

      IF gt_adrc_forn[] IS NOT INITIAL.
        SORT: gt_adrc_forn BY addrnumber.
      ENDIF.
    ENDIF.

    REFRESH: lt_rbkp_fae[].
    lt_rbkp_fae[] = gt_rbkp[].
    SORT: lt_rbkp_fae BY bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_rbkp_fae COMPARING bukrs.

    SELECT bukrs
           butxt
      INTO TABLE gt_t001
      FROM t001
      FOR ALL ENTRIES IN lt_rbkp_fae
      WHERE bukrs EQ lt_rbkp_fae-bukrs.

    IF gt_t001[] IS NOT INITIAL.
      SORT: gt_t001 BY bukrs.
    ENDIF.

    REFRESH: lt_rbkp_fae[].
    lt_rbkp_fae[] = gt_rbkp[].
    SORT: lt_rbkp_fae BY xblnr lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_rbkp_fae COMPARING xblnr lifnr.

    LOOP AT lt_rbkp_fae INTO DATA(lwa_rbkp_fae).
      FIND FIRST OCCURRENCE OF abap_undefined IN lwa_rbkp_fae-xblnr MATCH OFFSET lv_qtdpos.

      IF lv_qtdpos IS NOT INITIAL.
        lwa_nfenum-sign   = c_i.
        lwa_nfenum-option = c_eq.
        lwa_nfenum-low    = lwa_rbkp_fae-xblnr(lv_qtdpos).
        PERFORM f_zeros_esquerda USING lwa_nfenum-low.
        APPEND: lwa_nfenum TO lr_nfenum.
        CLEAR: lwa_nfenum, lv_posicao.

        lv_posicao = lv_qtdpos + 1.

        lwa_series-sign   = c_i.
        lwa_series-option = c_eq.
        lwa_series-low    = lwa_rbkp_fae-xblnr+lv_posicao.
        APPEND: lwa_series TO lr_series.
        CLEAR: lwa_series.
      ENDIF.

      CLEAR: lv_posicao, lv_qtdpos.
    ENDLOOP.

    SELECT docnum
           doctyp
           direct
           parvw
           parid
           cancel
           nfenum
           series
      INTO TABLE gt_1bnfdoc
      FROM j_1bnfdoc
      FOR ALL ENTRIES IN lt_rbkp_fae
      WHERE nfenum IN lr_nfenum         AND
            series IN lr_series         AND
            doctyp EQ c_1               AND
            direct EQ c_1               AND
            parvw  EQ c_lf              AND
            parid  EQ lt_rbkp_fae-lifnr AND
            cancel EQ space.

    IF gt_1bnfdoc[] IS NOT INITIAL.
      SORT: gt_1bnfdoc BY nfenum parid.
      REFRESH: lt_rbkp_fae[].
      lt_rbkp_fae[] = gt_rbkp[].
      SORT: lt_rbkp_fae BY belnr gjahr.
      DELETE ADJACENT DUPLICATES FROM lt_rbkp_fae COMPARING belnr gjahr.

      SELECT belnr
             gjahr
             buzei
             matnr
             werks
             ebeln
             ebelp
             menge
             wrbtr
        INTO TABLE gt_rseg
        FROM rseg
        FOR ALL ENTRIES IN lt_rbkp_fae
        WHERE belnr EQ lt_rbkp_fae-belnr AND
              gjahr EQ lt_rbkp_fae-gjahr.

      IF gt_rseg[] IS NOT INITIAL.
        SORT: gt_rseg BY belnr gjahr buzei.
        DATA(lt_rseg_fae) = gt_rseg[].
        SORT: lt_rseg_fae BY matnr.
        DELETE ADJACENT DUPLICATES FROM lt_rseg_fae COMPARING matnr.

        SELECT matnr
               mfrpn
          INTO TABLE gt_mara
          FROM mara
          FOR ALL ENTRIES IN lt_rseg_fae
          WHERE matnr EQ lt_rseg_fae-matnr.

        IF gt_mara[] IS NOT INITIAL.
          SORT: gt_mara BY matnr.
        ENDIF.

        REFRESH: lt_rseg_fae[].
        lt_rseg_fae[] = gt_rseg[].
        SORT: lt_rseg_fae BY werks.
        DELETE ADJACENT DUPLICATES FROM lt_rseg_fae COMPARING werks.

        SELECT werks
               adrnr
               j_1bbranch
          INTO TABLE gt_t001w
          FROM t001w
          FOR ALL ENTRIES IN lt_rseg_fae
          WHERE werks EQ lt_rseg_fae-werks.

        IF gt_t001w[] IS NOT INITIAL.
          SORT: gt_t001w BY werks.
          DATA(lt_t001w_fae) = gt_t001w[].
          SORT: lt_t001w_fae BY adrnr.
          DELETE ADJACENT DUPLICATES FROM lt_t001w_fae COMPARING adrnr.

          SELECT addrnumber
                 name1
                 city1
                 city2
                 post_code1
                 street
                 house_num1
                 house_num2
                 region
            INTO TABLE gt_adrc_cent
            FROM adrc
            FOR ALL ENTRIES IN lt_t001w_fae
            WHERE addrnumber EQ lt_t001w_fae-adrnr.

          IF gt_adrc_cent[] IS NOT INITIAL.
            SORT: gt_adrc_cent BY addrnumber.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_saida
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_saida .

  DATA: lt_1bnflin TYPE STANDARD TABLE OF bapi_j_1bnflin_readdata,
        lt_1bnfstx TYPE STANDARD TABLE OF bapi_j_1bnfstx_readdata,
        lt_return  TYPE STANDARD TABLE OF bapiret2.

  DATA: lwa_saida      LIKE LINE OF gt_saida,
        lwa_obj_header TYPE bapi_j_1bnfdoc_readdata.

  DATA: lv_posicao TYPE i,
        lv_qtdpos  TYPE i,
        lv_refkey  TYPE j_1bnflin-refkey,
        lv_nfenum  TYPE j_1bnfdoc-nfenum.
************************************************************************

  LOOP AT gt_rbkp INTO DATA(lwa_rbkp).
    gv_nf = lwa_rbkp-xblnr.

    READ TABLE gt_rseg TRANSPORTING NO FIELDS
                       WITH KEY belnr = lwa_rbkp-belnr
                                gjahr = lwa_rbkp-gjahr
                                BINARY SEARCH.
    CHECK sy-subrc IS INITIAL.

    FIND FIRST OCCURRENCE OF abap_undefined IN lwa_rbkp-xblnr MATCH OFFSET lv_qtdpos.

    IF lv_qtdpos IS NOT INITIAL.
      lv_posicao = lv_qtdpos + 1.

      lv_nfenum = lwa_rbkp-xblnr(lv_qtdpos).
      PERFORM f_zeros_esquerda USING lv_nfenum.

      READ TABLE gt_1bnfdoc INTO DATA(lwa_1bnfdoc)
                            WITH KEY nfenum = lv_nfenum
                                     parid  = lwa_rbkp-lifnr
                                     BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_J_1B_NF_READDATA'
          EXPORTING
            docnum       = lwa_1bnfdoc-docnum
          IMPORTING
            obj_header   = lwa_obj_header
          TABLES
            obj_item     = lt_1bnflin[]
            obj_item_tax = lt_1bnfstx[]
            return       = lt_return[].

        IF lt_1bnflin[] IS NOT INITIAL.
          SORT: lt_1bnflin BY docnum xped nitemped.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT gt_rseg INTO DATA(lwa_rseg)
                    WHERE belnr EQ lwa_rbkp-belnr AND
                          gjahr EQ lwa_rbkp-gjahr.

      lwa_saida-belnr  = lwa_rbkp-belnr.
      lwa_saida-gjahr  = lwa_rbkp-gjahr.
      lwa_saida-bukrs  = lwa_rbkp-bukrs.
      lwa_saida-rmwwr  = lwa_rbkp-rmwwr.
      lwa_saida-wmwst1 = lwa_rbkp-wmwst1.
      lwa_saida-xblnr  = lwa_rbkp-xblnr.
      lwa_saida-ebeln  = lwa_rseg-ebeln.
      lwa_saida-ebelp  = lwa_rseg-ebelp.
      lwa_saida-werks  = lwa_rseg-werks.
      lwa_saida-menge  = lwa_rseg-menge.
      lwa_saida-vluni  = lwa_rseg-wrbtr. "/ lwa_rseg-menge.

      READ TABLE lt_1bnflin INTO DATA(lwa_1bnflin)
                            WITH KEY docnum   = lwa_1bnfdoc-docnum
                                     xped     = lwa_rseg-ebeln
                                     nitemped = lwa_rseg-ebelp
                                     BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lwa_saida-vltot = ( lwa_1bnflin-nfnett / lwa_1bnflin-menge ) * lwa_rseg-menge.
      ENDIF.

      READ TABLE gt_mara INTO DATA(lwa_mara)
                         WITH KEY matnr = lwa_rseg-matnr
                         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lwa_saida-matnr = lwa_mara-matnr.
        lwa_saida-mfrpn = lwa_mara-mfrpn.
      ENDIF.

      READ TABLE gt_lfa1 INTO DATA(lwa_lfa1)
                         WITH KEY lifnr = lwa_rbkp-lifnr
                         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lwa_saida-lifnr = lwa_lfa1-lifnr.
        lwa_saida-name1 = lwa_lfa1-name1.
      ENDIF.

      READ TABLE gt_t001 INTO DATA(lwa_t001)
                         WITH KEY bukrs = lwa_rbkp-bukrs
                         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lwa_saida-bukrs = lwa_t001-bukrs.
        lwa_saida-butxt = lwa_t001-butxt.
      ENDIF.

      APPEND: lwa_saida TO gt_saida.
      CLEAR: lwa_saida.
    ENDLOOP.

    CLEAR: lwa_obj_header.
    REFRESH: lt_1bnflin[], lt_1bnfstx[], lt_return[].
  ENDLOOP.

  IF gt_saida[] IS NOT INITIAL.
    SORT: gt_saida BY belnr ebeln ebelp.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_gerar_pdf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SAIDA_AUX
*&---------------------------------------------------------------------*
FORM f_gerar_pdf USING pt_saida TYPE zttmm_cockpit_devol_danfe.

  DATA: lt_1bnflin TYPE STANDARD TABLE OF bapi_j_1bnflin_readdata,
        lt_1bnfstx TYPE STANDARD TABLE OF bapi_j_1bnfstx_readdata,
        lt_return  TYPE STANDARD TABLE OF bapiret2.

  DATA: lwa_address     TYPE sadr,
        lwa_branch_data TYPE j_1bbranch,
        lwa_address1    TYPE addr1_val,
        lwa_obj_header  TYPE bapi_j_1bnfdoc_readdata,
        lwa_produtos    LIKE LINE OF gt_produtos.

  DATA: lv_branch     TYPE j_1bbranch-branch,
        lv_bukrs      TYPE j_1bbranch-bukrs,
        lv_cgc_number TYPE j_1bwfield-cgc_number,
        lv_posicao    TYPE i,
        lv_qtdpos     TYPE i,
        lv_refkey     TYPE j_1bnflin-refkey,
        lv_nfenum     TYPE j_1bnfdoc-nfenum.
************************************************************************

  REFRESH: gt_produtos[].

  CLEAR: gwa_emitente, gwa_destiatario, gwa_totais_nfe, gwa_info_comp.

  READ TABLE pt_saida INTO DATA(lwa_saida) INDEX 1.

  IF sy-subrc IS INITIAL.
*---> Emitente.
    READ TABLE gt_t001w INTO DATA(lwa_t001w)
                        WITH KEY werks = lwa_saida-werks
                        BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_adrc_cent INTO DATA(lwa_adrc_cent)
                              WITH KEY addrnumber = lwa_t001w-adrnr
                              BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lv_branch = lwa_t001w-j_1bbranch.
        lv_bukrs  = lwa_saida-bukrs.

        CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
          EXPORTING
            branch            = lv_branch
            bukrs             = lv_bukrs
          IMPORTING
            address           = lwa_address
            branch_data       = lwa_branch_data
            cgc_number        = lv_cgc_number
            address1          = lwa_address1
          EXCEPTIONS
            branch_not_found  = 1
            address_not_found = 2
            company_not_found = 3
            OTHERS            = 4.

        IF lv_cgc_number IS NOT INITIAL.
          gwa_emitente-cgc_number = lv_cgc_number.
        ENDIF.

        IF lwa_branch_data-state_insc IS NOT INITIAL.
          gwa_emitente-state_insc = lwa_branch_data-state_insc.
        ENDIF.
        gwa_emitente-name1      = lwa_adrc_cent-name1.
        gwa_emitente-street     = lwa_adrc_cent-street.
        gwa_emitente-house_num1 = lwa_adrc_cent-house_num1.
        gwa_emitente-house_num2 = lwa_adrc_cent-house_num2.
        gwa_emitente-city2      = lwa_adrc_cent-city2.
        gwa_emitente-city1      = lwa_adrc_cent-city1.
        gwa_emitente-post_code1 = lwa_adrc_cent-post_code1.
        gwa_emitente-region     = lwa_adrc_cent-region.
      ENDIF.
    ENDIF.

*---> Destinatário.
    READ TABLE gt_lfa1 INTO DATA(lwa_lfa1)
                       WITH KEY lifnr = lwa_saida-lifnr
                       BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_adrc_forn INTO DATA(lwa_adrc_forn)
                              WITH KEY addrnumber = lwa_lfa1-adrnr
                              BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        gwa_destiatario-name1      = lwa_lfa1-name1.
        gwa_destiatario-stcd1      = lwa_lfa1-stcd1.
        gwa_destiatario-stcd3      = lwa_lfa1-stcd3.
        gwa_destiatario-street     = lwa_adrc_forn-street.
        gwa_destiatario-house_num1 = lwa_adrc_forn-house_num1.
        gwa_destiatario-house_num2 = lwa_adrc_forn-house_num2.
        gwa_destiatario-city2      = lwa_adrc_forn-city2.
        gwa_destiatario-city1      = lwa_adrc_forn-city1.
        gwa_destiatario-region     = lwa_adrc_forn-region.
        gwa_destiatario-post_code1 = lwa_adrc_forn-post_code1.
      ENDIF.
    ENDIF.

    DATA(lt_rbkp) = gt_rbkp[].
    DATA(lt_rseg) = gt_rseg[].
    DELETE lt_rbkp WHERE belnr NE lwa_saida-belnr.
    DELETE lt_rseg WHERE belnr NE lwa_saida-belnr.

    SORT: lt_rbkp BY belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM lt_rbkp COMPARING belnr gjahr.
    SORT: lt_rseg BY belnr gjahr.

    LOOP AT lt_rbkp INTO DATA(lwa_rbkp).
      READ TABLE lt_rseg TRANSPORTING NO FIELDS
                         WITH KEY belnr = lwa_rbkp-belnr
                                  gjahr = lwa_rbkp-gjahr
                                  BINARY SEARCH.
      CHECK sy-subrc IS INITIAL.


* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 26.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
      gv_nf = lwa_rbkp-xblnr.
* <<<--- END <<< changed by FBRITO(Felype Brito) 26.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183


      FIND FIRST OCCURRENCE OF abap_undefined IN lwa_rbkp-xblnr MATCH OFFSET lv_qtdpos.

      IF lv_qtdpos IS NOT INITIAL.
        lv_posicao = lv_qtdpos + 1.

        lv_nfenum = lwa_rbkp-xblnr(lv_qtdpos).
        PERFORM f_zeros_esquerda USING lv_nfenum.

        READ TABLE gt_1bnfdoc INTO DATA(lwa_1bnfdoc)
                              WITH KEY nfenum = lv_nfenum
                                       parid  = lwa_rbkp-lifnr
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'BAPI_J_1B_NF_READDATA'
            EXPORTING
              docnum       = lwa_1bnfdoc-docnum
            IMPORTING
              obj_header   = lwa_obj_header
            TABLES
              obj_item     = lt_1bnflin[]
              obj_item_tax = lt_1bnfstx[]
              return       = lt_return[].

          IF lt_1bnflin[] IS NOT INITIAL AND lt_1bnfstx[] IS NOT INITIAL.
            SORT: lt_1bnflin BY docnum xped nitemped,
                  lt_1bnfstx BY docnum itmnum taxtyp.
          ENDIF.
        ENDIF.
      ENDIF.

      LOOP AT lt_rseg INTO DATA(lwa_rseg) FROM sy-tabix.
        IF lwa_rbkp-belnr NE lwa_rseg-belnr OR
           lwa_rbkp-gjahr NE lwa_rseg-gjahr.
          EXIT.
        ENDIF.

        "lwa_produtos-desco      =
        "lwa_produtos-BC_ICMS_ST =
        "lwa_produtos-MVA        =
        "lwa_produtos-ICMSST     =
        lwa_produtos-buzei = lwa_rseg-buzei.
        lwa_produtos-matnr = lwa_rseg-matnr.
        lwa_produtos-xblnr = lwa_rbkp-xblnr.
        lwa_produtos-menge = lwa_rseg-menge.
        lwa_produtos-vluni = lwa_rseg-wrbtr / lwa_rseg-menge.


        READ TABLE gt_mara INTO DATA(lwa_mara)
                           WITH KEY matnr = lwa_rseg-matnr
                           BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lwa_produtos-mfrpn = lwa_mara-mfrpn.
        ENDIF.

        READ TABLE gt_saida INTO DATA(lwa_saida_aux)
                            WITH KEY belnr = lwa_rseg-belnr
                                     ebeln = lwa_rseg-ebeln
                                     ebelp = lwa_rseg-ebelp
                                     BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lwa_produtos-vltot           = lwa_saida_aux-vltot.
          gwa_totais_nfe-tot_prod_serv = gwa_totais_nfe-tot_prod_serv + lwa_saida_aux-vltot.
        ENDIF.

        READ TABLE lt_1bnflin INTO DATA(lwa_1bnflin)
                              WITH KEY docnum   = lwa_1bnfdoc-docnum
                                       xped     = lwa_rseg-ebeln
                                       nitemped = lwa_rseg-ebelp
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_1bnfstx INTO DATA(lwa_1bnfstx)
                                WITH KEY docnum = lwa_1bnflin-docnum
                                         itmnum = lwa_1bnflin-itmnum
                                         taxtyp = c_icm1
                                         BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            gwa_totais_nfe-bc_icms = gwa_totais_nfe-bc_icms +
                                     ( lwa_1bnfstx-base / lwa_1bnflin-menge ) *
                                       lwa_rseg-menge.

            gwa_totais_nfe-tot_icms = gwa_totais_nfe-tot_icms +
                                      ( lwa_1bnfstx-taxval / lwa_1bnflin-menge ) *
                                        lwa_rseg-menge.

            lwa_produtos-bc_icms   = ( lwa_1bnfstx-base   / lwa_1bnflin-menge ) * lwa_rseg-menge.
            lwa_produtos-icms      = ( lwa_1bnfstx-taxval / lwa_1bnflin-menge ) * lwa_rseg-menge.
            lwa_produtos-aliq_icms = lwa_1bnfstx-rate.
          ENDIF.

          READ TABLE lt_1bnfstx INTO lwa_1bnfstx
                                WITH KEY docnum = lwa_1bnflin-docnum
                                         itmnum = lwa_1bnflin-itmnum
                                         taxtyp = c_ics1
                                         BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            gwa_info_comp-vl_icms_st = gwa_info_comp-vl_icms_st +
                                       ( ( lwa_1bnfstx-taxval / lwa_1bnflin-menge ) *
                                           lwa_rseg-menge ).

            gwa_totais_nfe-bc_icms_st = 0.
*            gwa_totais_nfe-bc_icms_st = gwa_totais_nfe-bc_icms_st +
*                                        ( lwa_1bnfstx-othbas / lwa_1bnflin-menge ) *
*                                          lwa_rseg-menge.

            gwa_totais_nfe-tot_icms_st = 0.
*            gwa_totais_nfe-tot_icms_st = gwa_totais_nfe-tot_icms_st +
*                                         ( lwa_1bnfstx-taxval / lwa_1bnflin-menge ) *
*                                           lwa_rseg-menge.

            gwa_info_comp-bc_st = gwa_info_comp-bc_st +
                                  ( ( lwa_1bnfstx-othbas / lwa_1bnflin-menge ) *
                                      lwa_rseg-menge ).

            lwa_produtos-perc_aliq = lwa_1bnfstx-rate.
          ENDIF.

          READ TABLE lt_1bnfstx INTO lwa_1bnfstx
                                WITH KEY docnum = lwa_1bnflin-docnum
                                         itmnum = lwa_1bnflin-itmnum
                                         taxtyp = c_ipi2
                                         BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            gwa_totais_nfe-tot_outras = gwa_totais_nfe-tot_outras +
                                        ( lwa_1bnfstx-taxval / lwa_1bnflin-menge ) *
                                          lwa_rseg-menge.
            "gwa_totais_nfe-tot_descontos

*            gwa_totais_nfe-tot_nfe = gwa_totais_nfe-tot_nfe +
*                                     ( ( lwa_1bnflin-nfnett / lwa_1bnflin-menge ) * lwa_rseg-menge ) +
*                                     ( ( lwa_1bnfstx-taxval / lwa_1bnflin-menge ) * lwa_rseg-menge ).

            gwa_info_comp-vl_ipi = gwa_info_comp-vl_ipi +
                                   ( ( lwa_1bnfstx-taxval / lwa_1bnflin-menge ) *
                                       lwa_rseg-menge ).
          ENDIF.
        ENDIF.

        APPEND: lwa_produtos TO gt_produtos.
        CLEAR: lwa_produtos.
      ENDLOOP.

      gwa_totais_nfe-tot_nfe = gwa_totais_nfe-tot_outras + gwa_totais_nfe-tot_prod_serv.

      CLEAR: lwa_obj_header.
      REFRESH: lt_1bnflin[], lt_1bnfstx[], lt_return[].
    ENDLOOP.
  ENDIF.

  PERFORM f_baixa_pdf.
ENDFORM.


* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
*&---------------------------------------------------------------------*
*& Form f_gerar_pdf_ressar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SAIDA_AUX
*&---------------------------------------------------------------------*
FORM f_gerar_pdf_ressar USING pt_saida TYPE zttmm_cockpit_devol_danfe.

  DATA: lt_1bnflin TYPE STANDARD TABLE OF bapi_j_1bnflin_readdata,
        lt_1bnfstx TYPE STANDARD TABLE OF bapi_j_1bnfstx_readdata,
        lt_return  TYPE STANDARD TABLE OF bapiret2.

  DATA: lwa_address     TYPE sadr,
        lwa_branch_data TYPE j_1bbranch,
        lwa_address1    TYPE addr1_val,
        lwa_obj_header  TYPE bapi_j_1bnfdoc_readdata,
        lwa_produtos    LIKE LINE OF gt_produtos.

  DATA: lv_branch        TYPE j_1bbranch-branch,
        lv_bukrs         TYPE j_1bbranch-bukrs,
        lv_cgc_number    TYPE j_1bwfield-cgc_number,
        lv_posicao       TYPE i,
        lv_qtdpos        TYPE i,
        lv_1bnflin_menge TYPE j_1bnflin-menge,
        lv_rseg_menge    TYPE j_1bnflin-menge,
        lv_refkey        TYPE j_1bnflin-refkey,
        lv_nfenum        TYPE j_1bnfdoc-nfenum,
        lv_matnr         TYPE mara-matnr.
************************************************************************

  REFRESH: gt_produtos[].

  CLEAR: gwa_emitente, gwa_destiatario, gwa_totais_nfe, gwa_info_comp.

  SELECT SINGLE low
  FROM tvarvc
  INTO  lv_matnr
  WHERE name EQ 'ZMM_GAP_902_RES_NFE_MATNR'.


  READ TABLE pt_saida INTO DATA(lwa_saida) INDEX 1.

  IF sy-subrc IS INITIAL.
*---> Emitente.
    READ TABLE gt_t001w INTO DATA(lwa_t001w)
                        WITH KEY werks = lwa_saida-werks
                        BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_adrc_cent INTO DATA(lwa_adrc_cent)
                              WITH KEY addrnumber = lwa_t001w-adrnr
                              BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lv_branch = lwa_t001w-j_1bbranch.
        lv_bukrs  = lwa_saida-bukrs.

        CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
          EXPORTING
            branch            = lv_branch
            bukrs             = lv_bukrs
          IMPORTING
            address           = lwa_address
            branch_data       = lwa_branch_data
            cgc_number        = lv_cgc_number
            address1          = lwa_address1
          EXCEPTIONS
            branch_not_found  = 1
            address_not_found = 2
            company_not_found = 3
            OTHERS            = 4.

        IF lv_cgc_number IS NOT INITIAL.
          gwa_emitente-cgc_number = lv_cgc_number.
        ENDIF.

        IF lwa_branch_data-state_insc IS NOT INITIAL.
          gwa_emitente-state_insc = lwa_branch_data-state_insc.
        ENDIF.
        gwa_emitente-name1      = lwa_adrc_cent-name1.
        gwa_emitente-street     = lwa_adrc_cent-street.
        gwa_emitente-house_num1 = lwa_adrc_cent-house_num1.
        gwa_emitente-house_num2 = lwa_adrc_cent-house_num2.
        gwa_emitente-city2      = lwa_adrc_cent-city2.
        gwa_emitente-city1      = lwa_adrc_cent-city1.
        gwa_emitente-post_code1 = lwa_adrc_cent-post_code1.
        gwa_emitente-region     = lwa_adrc_cent-region.
      ENDIF.
    ENDIF.

*---> Destinatário.
    READ TABLE gt_lfa1 INTO DATA(lwa_lfa1)
                       WITH KEY lifnr = lwa_saida-lifnr
                       BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_adrc_forn INTO DATA(lwa_adrc_forn)
                              WITH KEY addrnumber = lwa_lfa1-adrnr
                              BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        gwa_destiatario-name1      = lwa_lfa1-name1.
        gwa_destiatario-stcd1      = lwa_lfa1-stcd1.
        gwa_destiatario-stcd3      = lwa_lfa1-stcd3.
        gwa_destiatario-street     = lwa_adrc_forn-street.
        gwa_destiatario-house_num1 = lwa_adrc_forn-house_num1.
        gwa_destiatario-house_num2 = lwa_adrc_forn-house_num2.
        gwa_destiatario-city2      = lwa_adrc_forn-city2.
        gwa_destiatario-city1      = lwa_adrc_forn-city1.
        gwa_destiatario-region     = lwa_adrc_forn-region.
        gwa_destiatario-post_code1 = lwa_adrc_forn-post_code1.
      ENDIF.
    ENDIF.

    DATA(lt_rbkp) = gt_rbkp[].
    DATA(lt_rseg) = gt_rseg[].
    DELETE lt_rbkp WHERE belnr NE lwa_saida-belnr.
    DELETE lt_rseg WHERE belnr NE lwa_saida-belnr.

    SORT: lt_rbkp BY belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM lt_rbkp COMPARING belnr gjahr.
    SORT: lt_rseg BY belnr gjahr.



    LOOP AT lt_rbkp INTO DATA(lwa_rbkp).
      READ TABLE lt_rseg TRANSPORTING NO FIELDS
                         WITH KEY belnr = lwa_rbkp-belnr
                                  gjahr = lwa_rbkp-gjahr
                                  BINARY SEARCH.
      CHECK sy-subrc IS INITIAL.

      gv_nf = lwa_rbkp-xblnr.

      FIND FIRST OCCURRENCE OF abap_undefined IN lwa_rbkp-xblnr MATCH OFFSET lv_qtdpos.

      IF lv_qtdpos IS NOT INITIAL.
        lv_posicao = lv_qtdpos + 1.

        lv_nfenum = lwa_rbkp-xblnr(lv_qtdpos).
        PERFORM f_zeros_esquerda USING lv_nfenum.

        READ TABLE gt_1bnfdoc INTO DATA(lwa_1bnfdoc)
                              WITH KEY nfenum = lv_nfenum
                                       parid  = lwa_rbkp-lifnr
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'BAPI_J_1B_NF_READDATA'
            EXPORTING
              docnum       = lwa_1bnfdoc-docnum
            IMPORTING
              obj_header   = lwa_obj_header
            TABLES
              obj_item     = lt_1bnflin[]
              obj_item_tax = lt_1bnfstx[]
              return       = lt_return[].

          IF lt_1bnflin[] IS NOT INITIAL AND lt_1bnfstx[] IS NOT INITIAL.
            SORT: lt_1bnflin BY docnum xped nitemped,
                  lt_1bnfstx BY docnum itmnum taxtyp.
          ENDIF.
        ENDIF.
      ENDIF.

      gwa_totais_nfe-tot_outras = 0.
      gwa_totais_nfe-tot_descontos = 0.
      gwa_totais_nfe-bc_icms = 0.
      gwa_totais_nfe-tot_icms = 0.
      gwa_totais_nfe-bc_icms_st = 0.
      gwa_totais_nfe-tot_icms_st = 0.

      gwa_info_comp-vl_ipi = 0.

      lwa_produtos-buzei = 1.
      lwa_produtos-matnr = lv_matnr.
      lwa_produtos-mfrpn = lv_matnr.
      lwa_produtos-xblnr = lwa_rbkp-xblnr.
      lwa_produtos-menge = 1.
      lwa_produtos-bc_icms   = 0.
      lwa_produtos-icms      = 0.
      lwa_produtos-aliq_icms = 0.

      lwa_produtos-perc_aliq = 0.

      LOOP AT lt_rseg INTO DATA(lwa_rseg) FROM sy-tabix.
        IF lwa_rbkp-belnr NE lwa_rseg-belnr OR
           lwa_rbkp-gjahr NE lwa_rseg-gjahr.
          EXIT.
        ENDIF.

        lv_rseg_menge += lwa_rseg-menge.

        READ TABLE lt_1bnflin INTO DATA(lwa_1bnflin)
                              WITH KEY docnum   = lwa_1bnfdoc-docnum
                                       xped     = lwa_rseg-ebeln
                                       nitemped = lwa_rseg-ebelp
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_1bnflin_menge += lwa_1bnflin-menge.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_1bnfstx FROM sy-tabix INTO DATA(lwa_1bnfstx).
        IF lwa_1bnfstx-docnum EQ lwa_1bnfdoc-docnum.

          IF lwa_1bnfstx-taxtyp EQ c_ics2 OR lwa_1bnfstx-taxtyp EQ c_ics1 .

            gwa_totais_nfe-tot_prod_serv += lwa_1bnfstx-taxval.
            gwa_totais_nfe-tot_nfe += lwa_1bnfstx-taxval.
            lwa_produtos-vltot += lwa_1bnfstx-taxval.
            lwa_produtos-vluni += lwa_1bnfstx-taxval.
            gwa_info_comp-vl_icms_st += lwa_1bnfstx-taxval.
            gwa_info_comp-bc_st += lwa_1bnfstx-othbas.

          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

      gwa_totais_nfe-tot_prod_serv = ( gwa_totais_nfe-tot_prod_serv / lv_1bnflin_menge ) * lv_rseg_menge.
      gwa_totais_nfe-tot_nfe = ( gwa_totais_nfe-tot_nfe / lv_1bnflin_menge ) * lv_rseg_menge.
      lwa_produtos-vltot = ( lwa_produtos-vltot / lv_1bnflin_menge ) * lv_rseg_menge.
      lwa_produtos-vluni = ( lwa_produtos-vluni / lv_1bnflin_menge ) * lv_rseg_menge.
      gwa_info_comp-vl_icms_st = (  gwa_info_comp-vl_icms_st / lv_1bnflin_menge ) * lv_rseg_menge.
      gwa_info_comp-bc_st = ( gwa_info_comp-bc_st / lv_1bnflin_menge ) * lv_rseg_menge.


      APPEND: lwa_produtos TO gt_produtos.
      CLEAR: lwa_produtos.

      CLEAR: lwa_obj_header.
      REFRESH: lt_1bnflin[], lt_1bnfstx[], lt_return[].
    ENDLOOP.
  ENDIF.

  PERFORM f_baixa_pdf.
ENDFORM.
* <<<--- END <<< changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183

*&---------------------------------------------------------------------*
*& Form f_chama_tela
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_chama_tela .
  IF gt_saida[] IS NOT INITIAL.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE: TEXT-001 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_tela_inicial
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_tela_inicial .

  DATA: lv_answer TYPE c LENGTH 1.

  CONSTANTS: c_text1 TYPE string VALUE 'Os dados não salvo serão perdidos',
             c_text2 TYPE string VALUE 'Deseja realmente sair ?',
             c_text3 TYPE string VALUE 'Atenção !'.
************************************************************************

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = c_text1
      textline2      = c_text2
      titel          = c_text3
      cancel_display = abap_false
    IMPORTING
      answer         = lv_answer.

  IF lv_answer EQ c_j.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_pdf_espelho
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pdf_espelho.

  DATA: lt_sel_row   TYPE STANDARD TABLE OF lvc_s_roid,
        lt_saida_aux TYPE STANDARD TABLE OF zstmm_cockpit_devol_danfe.

  DATA: lv_belnr TYPE zstmm_cockpit_devol_danfe-belnr.
************************************************************************

  go_alv->check_changed_data( ).

  go_alv->get_selected_rows(
    IMPORTING
      et_row_no = lt_sel_row[] ).

  IF lt_sel_row[] IS NOT INITIAL.
    LOOP AT lt_sel_row INTO DATA(lwa_sel_row).
      READ TABLE gt_saida INTO DATA(lwa_saida) INDEX lwa_sel_row-row_id.

      IF sy-subrc IS INITIAL.
        IF lv_belnr IS INITIAL.
          lv_belnr = lwa_saida-belnr.
        ENDIF.

        IF lv_belnr NE lwa_saida-belnr.
          MESSAGE: TEXT-019 TYPE c_s DISPLAY LIKE c_e.
          EXIT.
        ENDIF.

        APPEND: lwa_saida TO lt_saida_aux.
        CLEAR: lwa_saida.
      ENDIF.
    ENDLOOP.

    IF lt_saida_aux[] IS NOT INITIAL.
      SORT: lt_saida_aux BY belnr.
      DELETE ADJACENT DUPLICATES FROM lt_saida_aux COMPARING belnr.
* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
*       PERFORM f_gerar_pdf USING lt_saida_aux.
      CASE sy-ucomm.
        WHEN c_bt_pdf.
          PERFORM f_gerar_pdf USING lt_saida_aux.
        WHEN c_bt_pdf_ressar.
          PERFORM f_gerar_pdf_ressar USING lt_saida_aux.
      ENDCASE.
* <<<--- END <<< changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183


    ENDIF.

  ELSE.
    MESSAGE: TEXT-018 TYPE c_s DISPLAY LIKE c_w.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_layout.

  REFRESH: gt_fieldcat[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = c_est_alv
    CHANGING
      ct_fieldcat            = gt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF gt_fieldcat[] IS NOT INITIAL.
    LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<lwa_fieldcat>).
      CASE <lwa_fieldcat>-fieldname.
        WHEN 'BELNR'.
          <lwa_fieldcat>-col_pos = 1.
          <lwa_fieldcat>-coltext = TEXT-002. "Nº Doc. Fatura
        WHEN 'GJAHR'.
          <lwa_fieldcat>-col_pos = 2.
          <lwa_fieldcat>-coltext = TEXT-003. "Ano da préfatura criada
        WHEN 'BUKRS'.
          <lwa_fieldcat>-col_pos = 3.
          <lwa_fieldcat>-coltext = TEXT-004. "Número da empresa
        WHEN 'BUTXT'.
          <lwa_fieldcat>-col_pos = 4.
          <lwa_fieldcat>-coltext = TEXT-020. "Nome empresa
        WHEN 'LIFNR'.
          <lwa_fieldcat>-col_pos = 5.
          <lwa_fieldcat>-coltext = TEXT-005. "Nº fornecedor emissor da fatura
        WHEN 'NAME1'.
          <lwa_fieldcat>-col_pos = 6.
          <lwa_fieldcat>-coltext = TEXT-006. "Nome Fornecedor emissor da fatura
        WHEN 'RMWWR'.
          <lwa_fieldcat>-col_pos = 7.
          <lwa_fieldcat>-coltext = TEXT-007. "Valor bruto da fatura.
        WHEN 'WMWST1'.
          <lwa_fieldcat>-col_pos = 8.
          <lwa_fieldcat>-coltext = TEXT-008. "Valor do montante de impostos
        WHEN 'XBLNR'.
          <lwa_fieldcat>-col_pos = 9.
          <lwa_fieldcat>-coltext = TEXT-009. "Nº NFe ref.
        WHEN 'EBELN'.
          <lwa_fieldcat>-col_pos = 10.
          <lwa_fieldcat>-coltext = TEXT-010. "Nº Pedido ref.
        WHEN 'EBELP'.
          <lwa_fieldcat>-col_pos = 11.
          <lwa_fieldcat>-coltext = TEXT-011. "Item Pedido ref.
        WHEN 'MATNR'.
          <lwa_fieldcat>-col_pos = 12.
          <lwa_fieldcat>-coltext = TEXT-012. "Nº Material SAP
        WHEN 'MFRPN'.
          <lwa_fieldcat>-col_pos = 13.
          <lwa_fieldcat>-coltext = TEXT-013. "Nº Material fornec.
        WHEN 'WERKS'.
          <lwa_fieldcat>-col_pos = 14.
          <lwa_fieldcat>-coltext = TEXT-014. "Centro
        WHEN 'MENGE'.
          <lwa_fieldcat>-col_pos = 15.
          <lwa_fieldcat>-coltext = TEXT-015. "Qtde devolução
        WHEN 'VLUNI'.
          <lwa_fieldcat>-col_pos = 16.
          <lwa_fieldcat>-coltext = TEXT-016. "Valor unitário item
        WHEN 'VLTOT'.
          <lwa_fieldcat>-col_pos = 17.
          <lwa_fieldcat>-coltext = TEXT-017. "Valor total item
      ENDCASE.
    ENDLOOP.

*---> Monta layout e variant.
    gwa_layout-cwidth_opt = abap_true.
    gwa_layout-no_toolbar = abap_true.
    gwa_layout-zebra      = abap_true.
    gwa_layout-sel_mode   = c_a.
    gwa_variant-report    = sy-repid.
    gwa_variant-username  = sy-uname.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_baixa_pdf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_baixa_pdf .

  DATA: lt_pdf_tab       TYPE STANDARD TABLE OF tline,
        lt_tab_otf_final TYPE STANDARD TABLE OF itcoo,
        lt_otf_data      TYPE ssfcrescl.

  DATA: lwa_cparam TYPE ssfctrlop,
        lwa_outop  TYPE ssfcompop.

  DATA: lv_file_size     TYPE i,
        lv_bin_filesize  TYPE i,
        lv_file_name     TYPE string,
        lv_file_path     TYPE string,
        lv_full_path     TYPE string,
        lv_fm_name       TYPE rs38l_fnam,
        lv_dir           TYPE string,
* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 25.01.2023 - v1[GAP 902/903-SM Item (4)] - DS4K908183
        lv_sub_titulo    TYPE c LENGTH 36,
        lv_inf_comp_cfop TYPE c LENGTH 10,
        lv_default_name  TYPE string.
* <<<--- END <<< changed by FBRITO(Felype Brito) 25.01.2023 - v1[GAP 902/903-SM Item (4)] - DS4K908183

  CONSTANTS: lc_lp01              TYPE c LENGTH 4 VALUE 'LP01',
             lc_form              TYPE tdsfname VALUE 'ZSFMM002_DEV_ESP_DANFE',
             lc_window_title      TYPE string VALUE 'Salvar como...',
             lc_default_extension TYPE string VALUE 'PDF',
             lc_initial_directory TYPE string VALUE 'C:'.
************************************************************************

  lwa_outop-tddest     = lc_lp01.
  lwa_cparam-no_dialog = abap_true.
  lwa_cparam-preview   = space.
  lwa_cparam-getotf    = abap_true.


* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 25.01.2023 - v1[GAP 902/903 Item (4)] - DS4K908183
  CASE sy-ucomm.
    WHEN c_bt_pdf.
      lv_sub_titulo = 'ESPELHO DE NOTA DEVOLUÇÃO MERCADORIA'.
      lv_default_name =  `Espelho_Devolução_Mercadoria_NF_`.
    WHEN c_bt_pdf_ressar.
      lv_sub_titulo = 'ESPELHO DE NOTA RESSARCIMENTO'.
      lv_inf_comp_cfop = 'CFOP: 6603'.
      lv_default_name =  `Espelho_Devolução_Ressarcimento_NF_`.
  ENDCASE.
* <<<--- END <<< changed by FBRITO(Felype Brito) 25.01.2023 - v1[GAP 902/903 Item (4)] - DS4K908183

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lc_form
    IMPORTING
      fm_name            = lv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF lv_fm_name IS NOT INITIAL.
    CALL FUNCTION lv_fm_name
      EXPORTING
        control_parameters = lwa_cparam
        output_options     = lwa_outop
        user_settings      = space
        is_totais_nfe      = gwa_totais_nfe
        is_info_comp       = gwa_info_comp
        is_emitente        = gwa_emitente
        is_destiatario     = gwa_destiatario
* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 25.01.2023 - v1[GAP 902/903 Item (4)] - DS4K908183
        lv_sub_titulo      = lv_sub_titulo
        lv_inf_comp_cfop   = lv_inf_comp_cfop
* <<<--- END <<< changed by FBRITO(Felype Brito) 25.01.2023 - v1[GAP 902/903 Item (4)] - DS4K908183
      IMPORTING
        job_output_info    = lt_otf_data
      TABLES
        it_produtos        = gt_produtos[]
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    lt_tab_otf_final[] = lt_otf_data-otfdata[].

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = lv_bin_filesize
      TABLES
        otf                   = lt_tab_otf_final
        lines                 = lt_pdf_tab
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    lv_default_name = lv_default_name && gv_nf.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title              = lc_window_title
        default_extension         = lc_default_extension
        initial_directory         = lc_initial_directory
        default_file_name         = lv_default_name
        file_filter               = 'Arquivo PDF'
      CHANGING
        filename                  = lv_dir
        path                      = lv_file_path
        fullpath                  = lv_full_path
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.

    IF lv_dir IS NOT INITIAL.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize            = lv_bin_filesize
          filename                = lv_dir
          filetype                = 'BIN'
        IMPORTING
          filelength              = lv_file_size
        CHANGING
          data_tab                = lt_pdf_tab[]
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
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.

      IF sy-subrc IS INITIAL.
        MESSAGE: TEXT-021 TYPE c_s.
      ELSE.
        MESSAGE: TEXT-023 TYPE c_s DISPLAY LIKE c_e.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_zeros_esquerda
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_SERIES_LOW
*&---------------------------------------------------------------------*
FORM f_zeros_esquerda USING pv_zeros_esquerda.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pv_zeros_esquerda
    IMPORTING
      output = pv_zeros_esquerda.

ENDFORM.