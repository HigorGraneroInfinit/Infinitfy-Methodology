*&---------------------------------------------------------------------*
*& Include ZSDI_0121
*&---------------------------------------------------------------------*

IF ( vbak-auart = 'ZRCC' OR vbak-auart = 'ZRCP' ) AND "OV de retorno de rem. conserto.
   ( sy-tcode   = 'VA01' OR sy-tcode   = 'VA02' ).

  DATA: l_qtd_disp_aux TYPE vbap-kwmeng.
  DATA(lo_ret_conserto) = NEW zsdcl_ret_conserto( ).

  l_qtd_disp_aux = lo_ret_conserto->get_quant_disp(
               EXPORTING
                 is_vbak = vbak
                 is_xvbap = xvbap
                 is_vbep = vbep ).

  vbep-cmeng  = l_qtd_disp_aux.
  vbep-lmeng  = l_qtd_disp_aux.
  cvbep-wmeng = l_qtd_disp_aux.
  vbep-wmeng  = l_qtd_disp_aux.

  IF ( vbak-auart = 'ZRCC' OR vbak-auart = 'ZRCP') AND vbap-zmeng EQ 0 AND l_qtd_disp_aux >= 0.
    vbap-zmeng = l_qtd_disp_aux.
  ENDIF.

  IF l_qtd_disp_aux = 0.
    MESSAGE e208(00) WITH 'Já existem notas de crédito para o item 0010: 1 UN / SALDO TOTAL ATENDIDO'.
  ENDIF.
ENDIF.