*&---------------------------------------------------------------------*
*&  Include  ZINC_PCBURZBR0
*&---------------------------------------------------------------------*

FORM fuz_abn.
  TYPES ls_2006 TYPE STANDARD TABLE OF p2006 WITH EMPTY KEY.
  TYPES ls_2001 TYPE STANDARD TABLE OF p2001 WITH EMPTY KEY.
  TYPES ls_it TYPE STANDARD TABLE OF pc207 WITH EMPTY KEY.

  DATA lt_ausencia TYPE SORTED TABLE OF p2001 WITH NON-UNIQUE KEY awart.
  DATA lt_cod_ausencia TYPE SORTED TABLE OF ztb_perdaabono WITH NON-UNIQUE KEY cod.

  CONSTANTS lc_deducao_maxima TYPE pa2006-kverb VALUE '30.00000'.
  CONSTANTS lc_prov_total_abono TYPE lgart VALUE '4901'.
  CONSTANTS lc_prov_periodo_abono TYPE lgart VALUE '4902'.
  CONSTANTS lc_abono_ferias_resicao TYPE lgart VALUE '4411'.
  CONSTANTS lc_desligamento TYPE massn VALUE '10'.
  CONSTANTS lc_disp_sem_justa_causa TYPE massg VALUE '02'.
  CONSTANTS lc_ferias_indenizadas TYPE lgart VALUE 'MR10'.

  CASE sy-tcode.
    WHEN `PC00_M37_CALC`.
      " Monta a tabela contendo os períodos em aberto
      DATA(lt_periodo) = VALUE ls_2006( FOR <lfs_2006> IN p2006[]
                                      ( LINES OF COND #( WHEN <lfs_2006>-anzhl > <lfs_2006>-kverb AND 
                                                              <lfs_2006>-anzhl <= lc_deducao_maxima
                                                         THEN VALUE #( ( <lfs_2006> ) ) ) )
                         ).

      " Se existirem períodos em aberto
      IF lt_periodo IS NOT INITIAL.
        SELECT * FROM ztb_perdaabono INTO TABLE @lt_cod_ausencia.
        lt_ausencia = p2001[].

        TRY.
            " Pega o período mais antigo em aberto
            SORT: lt_periodo BY begda endda.
            DATA(lw_periodo) = lt_periodo[ 1 ].

            " Filtra a tabela de ausências de acordo com a tabela
            " ZTB_PERDAABONO
            lt_ausencia = FILTER #( lt_ausencia[] IN lt_cod_ausencia WHERE awart = cod ).

            " Verifica se o grupo de empregados (campo PERSG)
            " no infotipo 2001 é igual a 2 - Executivos
            " e retorna 0 ou realiza a somatória de todas as ausências
            DATA(lv_ausencias) = COND abwtg( WHEN line_exists( p0001[ persg = '2' ] ) THEN 0
                                                               ELSE REDUCE #( INIT ausencias = 0
                                                                               FOR <lfs_ausencia> IN lt_ausencia
                                                                                WHERE ( begda >= lw_periodo-begda AND 
                                                                                        endda <= lw_periodo-endda )
                                                                                NEXT ausencias = ausencias + <lfs_ausencia>-abwtg )
                                ).

          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.
    WHEN `PC00_M37_FERI`.
    WHEN `PC00_M37_TRM0`.
      TRY.
          " Retorna o ultimo registro do infotipo 0000
          DATA(lw_medida) = p0000[ lines( p0000 ) ].

          " Verifica se o Tipo de Medida (MASSN) = 10, o Motivo da
          " Medida (MASSG) = 02 e se existe na IT a rúbrica MR10 -
          " Férias Indenizadas e o valor no campo AMOUNT não é inicial
          IF lw_medida-massn = lc_desligamento AND
              lw_medida-massg = lc_disp_sem_justa_causa AND
               xsdbool( lines( VALUE ls_it( FOR <lfs_it> IN it
                               WHERE ( lgart = lc_ferias_indenizadas AND
                                       betrg IS NOT INITIAL )
                                     ( <lfs_it> ) ) ) > 0 ) = abap_true.

            " Busca na tabela OCRT o valor da rúbrica 4901, grava
            " na tabela IT com o valor da rúbrica 4411 e lança o mesmo
            " valor com sinal negativo na rúbrica 4902
            DATA(lv_prov_total_abono) = ocrt[ lgart = lc_prov_total_abono ]-betrg.

            INSERT LINES OF VALUE ls_it( ( lgart = lc_abono_ferias_resicao betrg = lv_prov_total_abono )
                                         ( lgart = lc_prov_periodo_abono betrg = lv_prov_total_abono * -1 ) )
                                    INTO TABLE it.

          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
  ENDCASE.
ENDFORM.
