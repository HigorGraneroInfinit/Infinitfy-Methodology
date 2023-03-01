*&---------------------------------------------------------------------*
*& Nome: ZINC_PCBURZBR0
*& Tipo: Include
*& Objetivo: função de folha de pagamento
*& Data/Hora: Segunda, Janeiro 30, 2023 (GMT-3) - 8:00
*& Desenvolvedor: Sérgio Melges (Infinitfy)
*&---------------------------------------------------------------------*
*& Versão 1: Sérgio Melges (Infinitfy) - Inicio Desenvolvimento -
*& PKDK900074
*&---------------------------------------------------------------------*

FORM fuz_abn.
  TYPES ls_2006 TYPE STANDARD TABLE OF p2006 WITH EMPTY KEY.
  TYPES ls_2001 TYPE STANDARD TABLE OF p2001 WITH EMPTY KEY.
  TYPES ls_0008 TYPE STANDARD TABLE OF p0008 WITH EMPTY KEY.
  TYPES ls_it TYPE STANDARD TABLE OF pc207 WITH EMPTY KEY.

  DATA lt_ausencia TYPE SORTED TABLE OF p2001 WITH NON-UNIQUE KEY awart.
  DATA lt_cod_ausencia TYPE SORTED TABLE OF ztb_perdaabono WITH
NON-UNIQUE KEY cod.

  CONSTANTS lc_deducao_maxima TYPE pa2006-kverb VALUE '30.00000'.
  CONSTANTS lc_desligamento TYPE massn VALUE '10'.
  CONSTANTS lc_disp_sem_justa_causa TYPE massg VALUE '02'.

  CONSTANTS lc_abono_saida_ferias TYPE lgart VALUE '4410'.
  CONSTANTS lc_abono_ferias_resicao TYPE lgart VALUE '4411'.
  CONSTANTS lc_base_prov_abono TYPE lgart VALUE '4900'.
  CONSTANTS lc_prov_total_abono TYPE lgart VALUE '4901'.
  CONSTANTS lc_prov_periodo_abono TYPE lgart VALUE '4902'.
  CONSTANTS lc_ferias_indenizadas TYPE lgart VALUE 'MR10'.
  CONSTANTS lc_salario_hora TYPE lgart VALUE '0002'.

  CONSTANTS: BEGIN OF lc_teto_abono,
               teto0 TYPE t511k-konst VALUE 'ZTET0',
               teto1 TYPE t511k-konst VALUE 'ZTET1',
               teto2 TYPE t511k-konst VALUE 'ZTET2',
             END OF lc_teto_abono.

  CASE sy-tcode.
    WHEN `PC00_M37_CALC`.
      " Monta a tabela contendo os períodos em aberto
      DATA(lt_periodo) = VALUE ls_2006( FOR <lfs_2006> IN p2006[]
                                      ( LINES OF COND #( WHEN
                                   <lfs_2006>-anzhl > <lfs_2006>-kverb
AND
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
            lt_ausencia = FILTER #( lt_ausencia[] IN lt_cod_ausencia
                                   WHERE awart = cod ).

            " Verifica se o grupo de empregados (campo PERSG)
            " no infotipo 2001 é igual a 2 - Executivos
            " e retorna 0 ou realiza a somatória de todas as ausências
            DATA(lv_ausencias) = COND abwtg( WHEN line_exists( p0001[
                                             persg = '2' ] ) THEN 0
                                     ELSE REDUCE #( INIT ausencias = 0
                                     FOR <lfs_ausencia> IN lt_ausencia
                                     WHERE ( begda >= lw_periodo-begda
AND
                                             endda <= lw_periodo-endda )
                                     NEXT ausencias = ausencias +
                                          <lfs_ausencia>-abwtg )
                                ).

            " Pega o mês/ano do processamento
            " (campo PABRP + PABRJ na tela seleção do programa HBRCALC0)
            ASSIGN ('(HBRCALC0)QPPNP-PABRP') TO
                      FIELD-SYMBOL(<lfs_mes>).
            ASSIGN ('(HBRCALC0)QPPNP-PABRJ') TO
                      FIELD-SYMBOL(<lfs_ano>).
            IF <lfs_mes> IS ASSIGNED AND <lfs_ano> IS ASSIGNED.
              " Busca o salário para o mês do processamento
              DATA(lt_0008_aux) = VALUE ls_0008( FOR <lfs_0008> IN p0008
                                           LET mes =
<lfs_0008>-begda+4(2)
                                               ano = <lfs_0008>-begda(4)
                                           IN ( LINES OF COND #(
                                           WHEN mes = <lfs_mes> AND
                                                ano = <lfs_ano>
                                           THEN VALUE #( ( <lfs_0008> )
)
                                  ) ) ).

              " Executa o cálculo do salário base
              DATA(lw_0008) = lt_0008_aux[ 1 ].
              DATA(lv_salario_base) = COND betrg( WHEN lw_0008-lga01 =
lc_salario_hora
                                                    THEN lw_0008-bet01 *
lw_0008-divgv
                                                     ELSE lw_0008-bet01
).

              " Seleciona os valores de teto do abono
              SELECT konst, kwert
                FROM t511k INTO TABLE @DATA(lt_teto_abono)
                WHERE konst LIKE 'ZTET%'.

              " Calcula o valor do abono
              DATA(lv_valor_abono) = VALUE betrg( ).
              IF lv_ausencias = 0.
                lv_valor_abono = lv_salario_base / 3.
                lv_valor_abono = COND #( WHEN lv_valor_abono >
lt_teto_abono[ konst = lc_teto_abono-teto0 ]-kwert THEN
                                                lt_teto_abono[ konst =
lc_teto_abono-teto0 ]-kwert
                                                ELSE lv_valor_abono ).
              ELSEIF lv_ausencias <= 4.
                lv_valor_abono = lv_salario_base / 4.
                lv_valor_abono = COND #( WHEN lv_valor_abono >
lt_teto_abono[ konst = lc_teto_abono-teto1 ]-kwert THEN
                                                lt_teto_abono[ konst =
lc_teto_abono-teto1 ]-kwert
                                                ELSE lv_valor_abono ).
              ELSEIF lv_ausencias <= 7.
                lv_valor_abono = lv_salario_base / 7.
                lv_valor_abono = COND #( WHEN lv_valor_abono >
lt_teto_abono[ konst = lc_teto_abono-teto2 ]-kwert THEN
                                                lt_teto_abono[ konst =
lc_teto_abono-teto2 ]-kwert
                                                ELSE lv_valor_abono ).
              ELSE.
                lv_valor_abono = 0.
              ENDIF.

              " Cálculo do Valor do Abono proporcional
              DATA(lv_valor_abono_prop) = ( lv_valor_abono /
lw_periodo-anzhl ) * lw_periodo-anzhl - lw_periodo-kverb.

              " Inserção das rúbricas na tabela IT
              INSERT LINES OF VALUE ls_it( ( lgart = lc_base_prov_abono
betrg = lv_valor_abono )
                                              ( lgart =
lc_prov_total_abono betrg = lv_valor_abono_prop ) )
                                            INTO TABLE it.

              " Provisionamento do valor do abono
              TRY.
                  DATA(lv_valor_abono_ant) = ocrt[ lgart =
  lc_prov_total_abono ]-betrg.
                  INSERT VALUE #( lgart = lc_prov_periodo_abono betrg =
  lv_valor_abono_prop - lv_valor_abono_ant )
                                  INTO TABLE it.
                CATCH cx_sy_itab_line_not_found.
                  INSERT VALUE #( lgart = lc_prov_periodo_abono betrg =
lv_valor_abono_prop ) INTO TABLE it.
              ENDTRY.

            ENDIF.

          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.
    WHEN `PC00_M37_FERI`.
      ASSIGN ('(HBRCFER0)QPPNP-BEGDA') TO
                FIELD-SYMBOL(<lfs_data_inicio>).
      ASSIGN ('(HBRCFER0)QPPNP-ENDDA') TO
                FIELD-SYMBOL(<lfs_data_fim>).

      IF <lfs_mes> IS ASSIGNED AND <lfs_ano> IS ASSIGNED.
        " Busca o salário para o mês do processamento
        TRY.
            DATA(lv_dias_gozo) = p2006[ begda = <lfs_data_inicio> endda
= <lfs_data_fim> ]-kverb.
            DATA(lv_4901) = ocrt[ lgart = lc_prov_total_abono ]-betrg.
            DATA(lv_valor_proporcional) = ( lv_4901 / 30 ) *
lv_dias_gozo.
            INSERT LINES OF VALUE ls_it( ( lgart = lc_abono_saida_ferias
betrg = lv_valor_proporcional )
                                            ( lgart =
lc_prov_periodo_abono betrg = lv_valor_proporcional * -1 )
                                    ) INTO TABLE it.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.

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
            DATA(lv_prov_total_abono) = ocrt[ lgart =
lc_prov_total_abono ]-betrg.

            INSERT LINES OF VALUE ls_it( ( lgart =
lc_abono_ferias_resicao betrg = lv_prov_total_abono )
                                         ( lgart = lc_prov_periodo_abono
betrg = lv_prov_total_abono * -1 ) )
                                    INTO TABLE it.

          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
  ENDCASE.
ENDFORM.