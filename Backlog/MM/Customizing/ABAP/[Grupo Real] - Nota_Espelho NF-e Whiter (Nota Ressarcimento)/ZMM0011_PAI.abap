*&---------------------------------------------------------------------*
*& Include          ZMM0011_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN c_back OR c_canc OR c_exit.
      PERFORM f_tela_inicial.
    WHEN c_bt_pdf.
      PERFORM f_pdf_espelho.
* --->>> BEGIN >>> changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183
    WHEN c_bt_pdf_ressar.
      PERFORM f_pdf_espelho.
* <<<--- END <<< changed by FBRITO(Felype Brito) 23.01.2023 - v1[GAP 902/903-SM45 Item (4)] - DS4K908183

  ENDCASE.

ENDMODULE.