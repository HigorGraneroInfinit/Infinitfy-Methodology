08.02.2023                         Sair classe                                 1
--------------------------------------------------------------------------------
class-pool .




class ZSDCL_RET_CONSERTO definition
  public
  final
  create public .
public section.
  types:
    BEGIN OF tp_ov_zvef,
        vbelv  TYPE vbap-vbeln,
        posnv  TYPE vbap-posnr,
        kwmeng TYPE vbap-kwmeng,
      END OF tp_ov_zvef .
  types:
    BEGIN OF tp_fat_zvef,
        vbeln TYPE vbrk-vbeln,
        posnn TYPE vbrp-posnr,
      END OF tp_fat_zvef .
  types:
    BEGIN OF tp_qtd_fat,
        vbeln  TYPE vbap-vbeln,
        posnr  TYPE vbap-posnr,
        kwmeng TYPE vbap-kwmeng,
      END OF tp_qtd_fat .
  types:
    tt_fat_zvef TYPE TABLE OF tp_fat_zvef .
  types:
    tt_qtd_fat  TYPE TABLE OF tp_qtd_fat .
  data MS_OV_ZVEF type TP_OV_ZVEF .
  data MT_FAT_ZVEF type TT_FAT_ZVEF .
  methods GET_QUANT_FATURADA
    importing
      !IS_VBAK type VBAK
      value(IT_VBAPVB) type VA_VBAPVB_T
    exporting
      !ET_RETURN type BAPIRET2_T .
  methods GET_QUANT_DISP
    importing
      !IS_VBAK type VBAK
      !IS_XVBAP type VBAPVB
      !IS_VBEP type VBEP
    returning
      value(R_QTD_DISP) type VBAP-KWMENG .
  methods GET_DADOS
    importing
      !IT_VBAPVB type VA_VBAPVB_T
      !I_AT_SAVE type XFELD default SPACE
    exporting
      !ET_QTD_FAT type TT_QTD_FAT
      !ET_QTD_ZVEF type TT_QTD_FAT .

protected section.

private section.
endclass. "ZSDCL_RET_CONSERTO definition

class ZSDCL_RET_CONSERTO implementation.

  METHOD get_dados.
    DATA: ls_qtd_fat TYPE tp_qtd_fat.
08.02.2023                         Sair classe                                 2
--------------------------------------------------------------------------------
    DATA: ls_qtd_zcon TYPE tp_qtd_fat.
    FIELD-SYMBOLS: <fs_auart> TYPE auart.
    ASSIGN ('(SAPMV45A)VBAK-AUART') TO <fs_auart>.
    "Seleciona a ordem de venda ZRCC ref. a fatura ZCON
    SELECT a~vbelv, a~posnv, kwmeng, a~vbeln, posnn
      INTO TABLE @DATA(lt_ov_zcon)
      FROM vbfa AS a
           INNER JOIN vbap AS b
           ON b~vbeln EQ a~vbelv
          AND b~posnr EQ a~posnv
          AND b~abgru EQ @space
           INNER JOIN vbak AS c
           ON c~vbeln EQ b~vbeln
          AND c~auart EQ 'ZCON'
      FOR ALL ENTRIES IN @it_vbapvb
     WHERE a~vbeln EQ @it_vbapvb-vgbel
       AND posnn   EQ @it_vbapvb-vgpos
       AND vbtyp_n EQ 'M'
       AND vbtyp_v EQ 'C'.
    CHECK sy-subrc = 0.
    "Seleciona todas as faturas correspondentes a ordem ZCON
    SELECT a~vbeln, posnn, rfmng, vbelv, posnv
      INTO TABLE @DATA(lt_fat_zcon)
      FROM vbfa AS a
           INNER JOIN vbrk AS b
           ON b~vbeln EQ a~vbeln
          AND b~fkart EQ 'ZCON'
          AND b~fksto EQ @space "Não estornados
          FOR ALL ENTRIES IN @lt_ov_zcon
     WHERE vbelv   EQ @lt_ov_zcon-vbelv
       AND posnv   EQ @lt_ov_zcon-posnv
       AND vbtyp_n EQ 'M'
       AND vbtyp_v EQ 'C'.
    CHECK sy-subrc = 0.
    "Quantidade da ZCON faturada
    LOOP AT lt_fat_zcon INTO DATA(ls_fat_zcon).
      CLEAR ls_qtd_zcon.
      ls_qtd_zcon-vbeln = ls_fat_zcon-vbeln.
      ls_qtd_zcon-posnr = ls_fat_zcon-posnn.
      ls_qtd_zcon-kwmeng = ls_fat_zcon-rfmng.
      COLLECT ls_qtd_zcon INTO et_qtd_zvef.
    ENDLOOP.
    SELECT vgbel, vgpos, kwmeng AS rfmng, vbeln, posnr AS posnn
            INTO TABLE @DATA(lt_fat_zrcc)
            FROM vbap
             FOR ALL ENTRIES IN @lt_fat_zcon
           WHERE vgbel EQ @lt_fat_zcon-vbeln
             AND vgpos EQ @lt_fat_zcon-posnn
             AND abgru EQ @space.
    IF lt_fat_zrcc[] IS NOT INITIAL.
      IF i_at_save IS INITIAL.
        DATA(ls_vbapvb) = it_vbapvb[ 1 ].
        DELETE lt_fat_zrcc
        WHERE vbeln EQ ls_vbapvb-vbeln
        AND posnn EQ ls_vbapvb-posnr.
      ENDIF.
      IF lt_fat_zrcc[] IS NOT INITIAL.
        "Quantidade já faturada
        DATA l_qtd_dev TYPE vbap-kwmeng.
        LOOP AT lt_fat_zrcc INTO DATA(ls_fat_zrcc).
          CLEAR ls_qtd_fat.
          ls_qtd_fat-vbeln = ls_fat_zrcc-vgbel.
          ls_qtd_fat-posnr = ls_fat_zrcc-vgpos.
08.02.2023                         Sair classe                                 3
--------------------------------------------------------------------------------
          ls_qtd_fat-kwmeng = ls_fat_zrcc-rfmng - l_qtd_dev.
          COLLECT ls_qtd_fat INTO et_qtd_fat.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_quant_disp.
    DATA: lt_qtd_fat TYPE tt_qtd_fat.
    DATA: ls_qtd_fat TYPE tp_qtd_fat.
    DATA: lt_qtd_zcon TYPE tt_qtd_fat.
    DATA: ls_qtd_zcon TYPE tp_qtd_fat.
    DATA: lt_vbapvp TYPE va_vbapvb_t.
    CHECK is_xvbap-vgbel IS NOT INITIAL.
    APPEND is_xvbap TO lt_vbapvp.
    me->get_dados(
        EXPORTING it_vbapvb   = lt_vbapvp
        IMPORTING et_qtd_fat  = lt_qtd_fat
                  et_qtd_zvef = lt_qtd_zcon ).
    "Quantidade da ordem ZCON
    READ TABLE lt_qtd_zcon INTO ls_qtd_zcon
    WITH KEY vbeln = is_xvbap-vgbel
                      posnr = is_xvbap-vgpos.
    CHECK sy-subrc = 0.
    "Quantidade total faturada da ZCON
       READ TABLE lt_qtd_fat INTO ls_qtd_fat
                  WITH KEY vbeln = is_xvbap-vgbel
                                    posnr = is_xvbap-vgpos.
    IF sy-subrc <> 0.
      r_qtd_disp = is_vbep-wmeng.
    ELSE.
      IF is_vbep-wmeng < ls_qtd_fat-kwmeng.
        CLEAR r_qtd_disp.
      ELSE.
        r_qtd_disp = is_vbep-wmeng - ls_qtd_fat-kwmeng.
      ENDIF.
    ENDIF.
    "verifica se a qtd disponível atende a OV ZRCC
    "IF is_vbep-wmeng < r_qtd_disp.
    "ENDIF.
  ENDMETHOD.

  METHOD get_quant_faturada.
    DATA: lt_qtd_fat TYPE TABLE OF tp_qtd_fat.
    DATA: ls_qtd_fat TYPE tp_qtd_fat.
    DATA: lt_qtd_zcon TYPE TABLE OF tp_qtd_fat.
    DATA: ls_qtd_zcon TYPE tp_qtd_fat.
    CLEAR et_return.
    me->get_dados(
    EXPORTING it_vbapvb = it_vbapvb
                i_at_save = abap_true
    IMPORTING et_qtd_fat = lt_qtd_fat
                          et_qtd_zvef = lt_qtd_zcon ).
    LOOP AT it_vbapvb INTO DATA(ls_vbapvb).
      "Quantidade da ordem ZCON
      READ TABLE lt_qtd_zcon INTO ls_qtd_zcon
                  WITH KEY vbeln = ls_vbapvb-vgbel
                           posnr = ls_vbapvb-vgpos.
      CHECK sy-subrc = 0.
      "Quantidade total faturada da ZCON
      READ TABLE lt_qtd_fat INTO ls_qtd_fat
                         WITH KEY vbeln = ls_vbapvb-vgbel
                                  posnr = ls_vbapvb-vgpos.
08.02.2023                         Sair classe                                 4
--------------------------------------------------------------------------------
      IF sy-subrc <> 0.
        CLEAR ls_qtd_fat.
      ENDIF.
      "verifica se a qtd disponível da ZCON atende a criação da ZRCC
      DATA(l_qtd_disp) = ls_qtd_zcon-kwmeng - ( ls_qtd_fat-kwmeng + ls_vbapvb-kw
      CHECK l_qtd_disp < 0.
      APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      <ls_return>-id = 'ZSD'.
      <ls_return>-type = 'E'.
      <ls_return>-number = '293'.
      <ls_return>-message_v1 = ls_vbapvb-kwmeng.
      CONDENSE <ls_return>-message_v1 NO-GAPS.
      <ls_return>-message_v2 = ls_vbapvb-posnr.
      <ls_return>-message_v3 = l_qtd_disp.
      CONDENSE <ls_return>-message_v3 NO-GAPS.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZSDCL_RET_CONSERTO implementation
