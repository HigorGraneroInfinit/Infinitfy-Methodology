*&---------------------------------------------------------------------*
*& Include          ZMM0011_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZMM0011_9000'.
  SET TITLEBAR 'ZMM0011_9000'.

* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
  IF rb_merc EQ 'X'.
    LOOP AT SCREEN.
      CASE screen-name .
        WHEN 'BT_PDF_RESSAR' .
          screen-active = '0'.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.
* <<<--- END <<< changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module M_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_alv OUTPUT.

  IF go_alv IS NOT BOUND AND go_container IS NOT BOUND.
    go_container = NEW cl_gui_custom_container( container_name = c_zcont_alv ).

    IF go_container IS BOUND.
      go_alv = NEW cl_gui_alv_grid( i_parent = go_container ).

      IF go_alv IS BOUND.
        PERFORM f_monta_layout.

        CALL METHOD go_alv->set_table_for_first_display
          EXPORTING
            is_layout                     = gwa_layout
            is_variant                    = gwa_variant
          CHANGING
            it_outtab                     = gt_saida[]
            it_fieldcatalog               = gt_fieldcat[]
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
      ENDIF.
    ENDIF.

  ELSE.
    go_alv->refresh_table_display( ).
  ENDIF.
ENDMODULE.