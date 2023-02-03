*----------------------------------------------------------------------*
*                             T-SYSTEMS                                *
*----------------------------------------------------------------------*
* Client.....: Grupo Real                                              *
* Author.....: Antonio Carlos (ACARLOS)                                *
* Date.......: 25/07/2022                                              *
* Description: Cockpit devolução espelho DANFE Antes do envio          *
*              ao fornecedor.                                          *
* Project....: Projeto Transformar                                     *
* GAP........: MM908                                                   *
*----------------------------------------------------------------------*
* Change log                                                           *
*----------------------------------------------------------------------*
* Author: Felype Brito                                  Date: 27/01/23 *
* Request: DS4K908183                                                  *
* Description: Nota/Espelho NF-e Whiter (Nota Ressarcimento)           *
*----------------------------------------------------------------------*
REPORT zmm0011_cockpit_devol_danfe.

INCLUDE: zmm0011_top.
INCLUDE: zmm0011_lcl.
INCLUDE: zmm0011_pbo.
INCLUDE: zmm0011_pai.
INCLUDE: zmm0011_for.

START-OF-SELECTION.
  PERFORM f_busca_dados.
  PERFORM f_monta_saida.
  PERFORM f_chama_tela.