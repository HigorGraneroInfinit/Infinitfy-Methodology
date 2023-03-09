*&---------------------------------------------------------------------*
*& Report  ZPR_IMP_VALORES_ASS_MEDICA
*&---------------------------------------------------------------------*
*& Nome: GAP_01_ECP_02 - Importação de valores de Assistência Médica
*& Tipo: Report
*& Objetivo: carga das informações financeiras mensais dos planos de
*& assistência médica da Unimed
*& Data/Hora: Quarta, Março 08, 2023 (GMT-3) - 08:00
*& Desenvolvedor: Sérgio Melges (Infinitfy)
*& Programa base (cópia): ZPPYC001
*&---------------------------------------------------------------------*
*& Versão 1: Sérgio Melges (Infinitfy) - Inicio Desenvolvimento -
*& PKDK900135
*& Versão 2: ?
*& Versão 3: ?
*&---------------------------------------------------------------------*

REPORT zpr_imp_valores_ass_medica.

*&---------------------------------------------------------------------*
*& Types
*&---------------------------------------------------------------------*
TYPES :
*        Tipo para os registros do arquivo
         BEGIN OF y_data              ,
           linha(630) TYPE c         ,   " Linha do arquivo
         END   OF y_data              ,
*        Tipo para log dos registros processados
         BEGIN OF y_log               ,
           lgart      TYPE p0015-lgart,   " Rúbrica
           pernr      TYPE prelp-pernr,   " Número pessoal
           begda      TYPE prelp-begda,   " Data inicial
           endda      TYPE prelp-endda,   " Data final
           betrg      TYPE p0015-betrg,   " Motante
           anzhl      TYPE p0015-anzhl,   " Quantidade
           tipo       TYPE char1      ,   " E=Erro / S=Sucesso
           msg        TYPE char255    ,   " Mensagem de erro
           afast      TYPE char1      ,   " Afastado?
           linha      TYPE sy-tabix   ,   " Linha
         END   OF y_log               ,
*        Tipo para sumarização de linhas
         BEGIN OF y_sum               ,
           refer      TYPE char18     ,   " Número pessoal + Rubrica +
           " mês/ano referência
           betrg      TYPE p0015-betrg,   " Motante
           anzhl      TYPE p0015-anzhl,   " Quantidade
           proce      TYPE char10     ,   " Linha processada
         END   OF y_sum               ,
*        Tipo para controle de linhas já processadas
         BEGIN OF y_lin               ,
           linha      TYPE sy-tabix   ,   " Linha
         END   OF y_lin               .

TYPES: BEGIN OF y_file,
        dirname     TYPE dirname_al11, " name of directory
        name        TYPE filename_al11," name of entry
        type(10)    TYPE c,            " type of entry.
        len         TYPE p LENGTH 8 DECIMALS 0, " length in bytes.
owner(8)    TYPE c,            " owner of the entry.
mtime       TYPE p LENGTH 6 DECIMALS 0,  " last mod.date, sec since 1970
mode(9)     TYPE c,            " like "rwx-r-x--x": prot. mode
useable(1)  TYPE c,
subrc(4)    TYPE c,
errno(3)    TYPE c,
errmsg(40)  TYPE c,
mod_date    TYPE dats,
mod_time(8) TYPE c,            " hh:mm:ss
seen(1)     TYPE c,
changed(1)  TYPE c,
END OF y_file.
TYPES: st_despmed_td TYPE ztb_despmed_td.

*&---------------------------------------------------------------------*
*& Internal Tables
*&---------------------------------------------------------------------*
DATA: it_data TYPE STANDARD TABLE OF y_data,   " Tabela com os registros
 " do arquivo
      it_log  TYPE STANDARD TABLE OF y_log ,   " Tabela com o log do
 " processamento
      it_sum  TYPE STANDARD TABLE OF y_sum ,   " Tabela com registros
 " sumarizados
      it_lin  TYPE STANDARD TABLE OF y_lin ,   " Tabela com linhas
 " processadas
      it_0000 TYPE STANDARD TABLE OF p0000 ,   " Tabela para infotipo
 " 0000
      it_0014 TYPE STANDARD TABLE OF p0014 ,   " Tabela para infotipo
 " 0014
      it_0015 TYPE STANDARD TABLE OF p0015 ,   " Tabela para infotipo
 " 0015
      it_2001 TYPE STANDARD TABLE OF p2001 ,   " Tabela para infotipo
 " 2001
      it_0267 TYPE STANDARD TABLE OF p0267 ,   " Tabela para infotipo
 " 0267
      it_file TYPE STANDARD TABLE OF y_file,
      it_desp_medica_titu_dep TYPE TABLE OF ztb_despmed_td.

*&---------------------------------------------------------------------*
*& Work Areas
*&---------------------------------------------------------------------*
DATA: w_data TYPE y_data,   " WA para registro do arquivo
      w_file LIKE LINE OF it_file,
      w_log  TYPE y_log ,   " WA para log do processamento
      w_sum  TYPE y_sum ,   " WA para registros sumarizados
      w_lin  TYPE y_lin ,   " WA para linhas processadas
      w_0000 TYPE p0000 ,   " WA para infotipo 0000
      w_0014 TYPE p0014 ,   " WA para infotipo 0014
      w_0015 TYPE p0015 ,   " WA para infotipo 0015
      w_2001 TYPE p2001 ,   " WA para infotipo 2001
      w_0267 TYPE p0267 ,   " WA para infotipo 0267
      v_erro,
      v_message   TYPE   string. " Messagem para documentos inválidos na
" rede

CONSTANTS: c_nome TYPE rlgrap-filename VALUE
'/INTERFACES/HR/COOPERATIVA',
          c_sign                      VALUE 'I',
          c_ast   TYPE filename_al11  VALUE '*',
          c_file_f                    VALUE 'F',
          c_file_min                  VALUE 'f',
          c_extensao TYPE c LENGTH 9  VALUE '.txt', "Extensão do
          " arquivo
          c_flag                      VALUE 'X'   , "Flag
          c_pontov                    VALUE ';'   ,
          c_barra                     VALUE '/'   ,
          c_desconto_copart           VALUE '4754'.

*&---------------------------------------------------------------------*
*& Field-symbols
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <fs_it_infty> TYPE STANDARD TABLE,
               <fs_w_infty>  TYPE any           ,
               <fs_campo>    TYPE any           ,
               <fs_sum>      TYPE y_sum         .

*&---------------------------------------------------------------------*
*& Tela de seleção
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_serv RADIOBUTTON GROUP g2  DEFAULT 'X' USER-COMMAND xx1,
            " servidor
            p_locl RADIOBUTTON GROUP g2. " LOCAL
PARAMETERS:
 p_arq    TYPE rlgrap-filename OBLIGATORY.  " Nome do arquivo
SELECTION-SCREEN END   OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:
  p_i14    TYPE c RADIOBUTTON GROUP rg1,     " Infotipo 0014
  p_i15    TYPE c RADIOBUTTON GROUP rg1,     " Infotipo 0015
  p_i15_un TYPE c RADIOBUTTON GROUP rg1,     " Infotipo 0015 (Unimed)
  p_i267   TYPE c RADIOBUTTON GROUP rg1.     " Infotipo 0267
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(27) text-004.
SELECTION-SCREEN POSITION 32.
PARAMETERS:
  p_mot    TYPE p0267-ocrsn.                 " Motivo para cálc. folha
" pgto.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(27) text-005.
SELECTION-SCREEN POSITION 32.
PARAMETERS:
  p_cat    TYPE payty,                       " Característica do cálculo
  p_ide    TYPE payid,                       " Identificador do cálculo
  p_dat    TYPE begda.                       " Data de pagamento
SELECTION-SCREEN END   OF LINE.
PARAMETERS:
  p_exc    TYPE c AS CHECKBOX.               "Exclusão de lançamentos
" indevidos
SELECTION-SCREEN END   OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS:
  p_ati    TYPE c RADIOBUTTON GROUP rg2,               " Somente
  " ocupação ativo
  p_dem    TYPE c RADIOBUTTON GROUP rg2 DEFAULT 'X'.   " Somente
" ocupação demitido
SELECTION-SCREEN END   OF BLOCK b3.


*&---------------------------------------------------------------------*
*& Evento AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

* Realiza validação dos campos da tela de seleção
  IF NOT p_i267 IS INITIAL.
    IF p_mot IS INITIAL.
      MESSAGE e252(s#) WITH text-e11.  " Necessário informar motivo para
      " cálculo.
    ENDIF.
    IF p_cat IS INITIAL OR
       p_ide IS INITIAL OR
       p_dat IS INITIAL.
      MESSAGE e252(s#) WITH text-e12.  " Necessário informar Folha pgto.
      " off-cycle.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*& Evento AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF NOT p_locl IS INITIAL AND p_arq EQ c_nome.
    CLEAR p_arq.
  ELSE.
    IF p_arq IS INITIAL.
      p_arq = c_nome.
    ENDIF.
  ENDIF.



*&---------------------------------------------------------------------*
*& Evento AT SELECTION-SCREEN ON VALUE-REQUEST
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq.

* Auxiliar para busca de arquivo
  PERFORM zf_abrir_arquivo USING p_arq.


*&---------------------------------------------------------------------*
*& Evento START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*          Inicializa work areas e tabelas internas
  PERFORM: zf_inicializa        ,
*          Verifica se o arquivo informado na tela de seleção é válido
           zf_verifica_arquivo  ,
*          Carrega registros do arquivo para tabela interna
           zf_carrega_arquivo   ,
*          Sumariza registros do arquivo
           zf_sumariza_registros,
*          Realiza o processamento: inclusão ou deleção
           zf_processa          .


*&---------------------------------------------------------------------*
*& Evento END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

*          Mostra Log do processamento
  PERFORM: zf_mostra_log_proces,

*          Limpa tabelas internas utilizadas no processamento
           zf_inicializa       .

*&---------------------------------------------------------------------*
*&      Form  zf_abrir_arquivo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_abrir_arquivo  USING p_arquivo TYPE any.

  DATA: itl_filetable TYPE filetable        ,
        wl_filetable  TYPE LINE OF filetable,
        vl_title      TYPE string           ,
        vl_filter     TYPE string           ,
        vl_rc         TYPE sy-subrc         .

  vl_filter = '*.txt'.
  vl_title  = text-006.

  CHECK NOT p_locl IS INITIAL.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = vl_title
      file_filter             = vl_filter
      multiselection          = space
    CHANGING
      file_table              = itl_filetable
      rc                      = vl_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itl_filetable INTO wl_filetable INDEX 1.
    p_arquivo = wl_filetable-filename.
  ENDIF.

ENDFORM.                    "zf_abrir_arquivo

*&---------------------------------------------------------------------*
*&      Form  zf_verifica_arquivo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_verifica_arquivo .

  DATA: vl_filename TYPE string,
        vl_result   TYPE abap_bool,
        vl_msg       TYPE string,
        vl_diretorio TYPE  btch0000-text80.

  IF NOT p_locl IS INITIAL.

* Atribui o arquivo da tela de seleção
    vl_filename = p_arq.

    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = vl_filename
      RECEIVING
        result               = vl_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.

    IF sy-subrc IS INITIAL.

      IF vl_result EQ space.
        MESSAGE i252(s#) WITH text-e02. " Arquivo não encontrado.
        LEAVE LIST-PROCESSING.          " Paralisa Processamento
      ENDIF.

    ENDIF.

  ELSE.

    CLEAR vl_diretorio.
    vl_diretorio = p_arq.

    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory                   = vl_diretorio
*       WRITE_CHECK                 = ' '
*       FILNAME                     = ' '
*       DIRECTORY_LONG              =
      EXCEPTIONS
        pfl_dir_not_exist           = 1
        pfl_permission_denied       = 2
        pfl_cant_build_dataset_name = 3
        pfl_file_not_exist          = 4
        OTHERS                      = 5.

    CASE sy-subrc .
      WHEN 1.
        CONCATENATE text-007 p_arq INTO vl_msg.
        MESSAGE vl_msg TYPE c_sign.
        STOP.

      WHEN 2.
        CONCATENATE text-008 p_arq INTO vl_msg.
        MESSAGE vl_msg TYPE c_sign.
        STOP.
    ENDCASE.


  ENDIF.

ENDFORM.                    "f_verifica_arquivo


*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_carrega_arquivo .

  DATA: vl_fname TYPE string         ,
        vl_file  TYPE rlgrap-filename,
        vl_arq   TYPE string         .

* Limpa tabela interna
  FREE : it_data.

  IF NOT p_locl IS INITIAL.

* Limpa variavel
    CLEAR:  vl_fname.

* Atribui Caminho
    vl_fname = p_arq.

* Busca arquivo e carrega em tabela interna
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = vl_fname
        filetype                = 'ASC'
        has_field_separator     = space
      CHANGING
        data_tab                = it_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.

* Retorno de erro
    IF NOT sy-subrc IS INITIAL.
*   Mensagem de erro
      MESSAGE i252(s#) WITH text-e01. " Nenhum registro encontrado.
      LEAVE LIST-PROCESSING.          " Paralisa Processamento
    ENDIF.

  ELSE.

**  Buscar os arquivos no diretório.
    PERFORM zf_buscar_arq_direrio USING p_arq c_ast.

*   Verifica se foi encontrado no diretório
    IF NOT it_file[] IS INITIAL.

***   Processar os arquivos do diretório.
      LOOP AT it_file INTO w_file.

        CLEAR: vl_arq, vl_file.
        REFRESH it_data[].

*       Montar o nome do arquivo
        CONCATENATE p_arq c_barra w_file-dirname INTO vl_file.

*****   Buscar conteúdo do arquivo
        PERFORM zf_busca_arq USING      vl_file
                             CHANGING   w_data
                                        it_data .

******* Após copiar, apagar o arquivo da origem
        DELETE DATASET vl_file.

        CLOSE DATASET vl_file.

      ENDLOOP.
    ELSE.
      MESSAGE text-009 TYPE 'I'.
    ENDIF.

  ENDIF. " IF NOT p_locl IS INITIAL.

ENDFORM.                    " ZF_CARREGA_ARQUIVO


*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSA
*&---------------------------------------------------------------------*
*       PROCESSA INFOTIPO
*----------------------------------------------------------------------*
FORM zf_processa.

  DATA: vl_pernr TYPE prelp-pernr,   " Número pessoal
        vl_betrg TYPE p0015-betrg,   " Montante
        vl_lgart TYPE p0015-lgart,   " Rubrica
        vl_begda TYPE prelp-begda,   " Data inicio
        vl_endda TYPE prelp-endda,   " Data fim
        vl_anzhl TYPE p0015-anzhl,   " Quantidade
        vl_tabix TYPE char10     ,   " Indice atual
        vl_infty TYPE prelp-infty,   " Infotipo
        vl_conv  TYPE c          ,   " Controle das conversões
        vl_afast TYPE c          ,   " Afastado?
        vl_refer TYPE char18     ,   " Referencia
        vl_msg   TYPE char255    ,   " Mensagem
        vl_cname TYPE ztb_despmed_td-cname, " Nome
        vl_objps TYPE ztb_despmed_td-objps, " CPF
        vl_data_emis TYPE ztb_despmed_td-data, " Data de emissão
        vl_tipo_benf TYPE ztb_despmed_td-tipo. " Tipo de Beneficiário


*** Douglas Leite
  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

* Inivializa infotipos
  PERFORM: zf_inicia_infotipos CHANGING vl_infty.

  LOOP AT it_data INTO w_data.

    vl_tabix = sy-tabix.

    CONDENSE: vl_tabix.

*   Não considera o primeiro registro para o IT 0015 - Pão de Açucar
**    IF sy-tabix EQ 1 AND NOT  p_i15_pa IS INITIAL.
**      CONTINUE.
**    ENDIF.

*** Douglas Leite
    IF ( sy-tabix MOD 500 ) EQ 0.
      CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
    ENDIF.

*   Se o registro já foi processado, pula
    READ TABLE it_lin WITH KEY linha = vl_tabix BINARY SEARCH
TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

*   Inicializa objetos
    CLEAR: vl_pernr,
           vl_betrg,
           vl_lgart,
           vl_begda,
           vl_endda,
           vl_anzhl,
           vl_afast,
           vl_cname,
           vl_objps,
           vl_data_emis,
           vl_tipo_benf.

    PERFORM: zf_converte_valores USING    w_data
                                          vl_tabix
                                 CHANGING vl_pernr
                                          vl_betrg
                                          vl_anzhl
                                          vl_endda
                                          vl_begda
                                          vl_lgart
                                          vl_conv
                                          vl_cname
                                          vl_objps
                                          vl_data_emis
                                          vl_tipo_benf.

*   Se algum campo apresentou problemas, pula registro
    IF vl_conv EQ 'E'.
      CONTINUE.
    ENDIF.

*   Verifica se o registro já foi processado
    CLEAR: w_sum   ,
           vl_refer.
    CONCATENATE vl_pernr vl_lgart vl_begda(6) INTO vl_refer.
    READ TABLE it_sum ASSIGNING <fs_sum> WITH KEY refer = vl_refer
  BINARY SEARCH.
    IF sy-subrc IS INITIAL.
*     Verifica se já foi processada esta referencia
      IF NOT <fs_sum>-proce IS INITIAL.
        CLEAR   : vl_betrg,
                  vl_anzhl.
        CONDENSE: <fs_sum>-proce.
        CONCATENATE text-s03 <fs_sum>-proce INTO vl_msg SEPARATED BY
  space.
        PERFORM : zf_atribui_log USING vl_pernr vl_lgart vl_begda
  vl_endda vl_betrg vl_anzhl vl_afast 'S' vl_msg vl_tabix.
        CONTINUE.
      ELSE.
        vl_betrg = <fs_sum>-betrg.
        vl_anzhl = <fs_sum>-anzhl.
      ENDIF.
    ELSE.
      CLEAR: vl_refer.
    ENDIF.

*   Verifica se a rubrica já existe para o funcionário
    FREE: <fs_it_infty>.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = vl_pernr
        infty           = vl_infty
        begda           = vl_begda
        endda           = vl_endda
      TABLES
        infty_tab       = <fs_it_infty>
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
*   Verifica se é inclusão ou exclusão
    IF p_exc IS INITIAL.
*     Para o infotipo 0267, verifica o motivo e identificador
      IF p_i267 IS INITIAL.
        SORT <fs_it_infty> BY ('LGART')
                              ('BETRG').
        READ TABLE <fs_it_infty> WITH KEY ('LGART') = vl_lgart
                                          ('BETRG') = vl_betrg
                                           TRANSPORTING NO FIELDS BINARY
  SEARCH.
      ELSE.
        SORT <fs_it_infty> BY ('LGART')
                              ('BETRG')
                              ('OCRSN')
                              ('PAYID').
        READ TABLE <fs_it_infty> WITH KEY ('LGART') = vl_lgart
                                          ('BETRG') = vl_betrg
                                          ('OCRSN') = p_mot
                                          ('PAYID') = p_ide
                                          TRANSPORTING NO FIELDS BINARY
  SEARCH.
      ENDIF.
      IF sy-subrc IS INITIAL.
        PERFORM: zf_atribui_log USING vl_pernr vl_lgart vl_begda
  vl_endda vl_betrg vl_anzhl vl_afast 'E' text-e07 vl_tabix.
        CONTINUE.
      ELSE.
        CLEAR: <fs_w_infty>.
        IF NOT p_i267 IS INITIAL.
          w_0267-ocrsn = p_mot.  " Motivo para cálc. folha pgto.
          w_0267-payty = p_cat.  " Característica do cálculo
          w_0267-payid = p_ide.  " Identificador do cálculo
        ENDIF.
        w_0014-pernr = w_0015-pernr = w_0267-pernr = vl_pernr.
        w_0014-betrg = w_0015-betrg = w_0267-betrg = vl_betrg.
        w_0014-lgart = w_0015-lgart = w_0267-lgart = vl_lgart.
        w_0014-begda = w_0015-begda = w_0267-begda = vl_begda.
        w_0014-endda = w_0015-endda = w_0267-endda = vl_endda.
        w_0014-anzhl = w_0015-anzhl = w_0267-anzhl = vl_anzhl.
      ENDIF.
    ELSE.
      IF NOT <fs_it_infty>[] IS INITIAL.
        DELETE <fs_it_infty> WHERE ('PERNR NE VL_PERNR OR SUBTY NE
  VL_LGART OR BETRG NE VL_BETRG').
      ENDIF.
      IF <fs_it_infty>[] IS INITIAL.
        PERFORM: zf_atribui_log USING vl_pernr vl_lgart vl_begda
  vl_endda vl_betrg vl_anzhl vl_afast 'E' text-e08 vl_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Seleciona dados do funcionário
    FREE: it_0000.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = vl_pernr
        infty           = '0000'
        begda           = vl_begda
        endda           = vl_endda
      TABLES
        infty_tab       = it_0000
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    READ TABLE it_0000 INTO w_0000 INDEX 1.
    IF sy-subrc IS INITIAL.
*     Verifica se o funcionário esta desligado e se a opção escolhida é
*     só de ativos
      IF w_0000-stat2 EQ 0.
        IF NOT p_i15_un IS INITIAL OR
           NOT p_ati    IS INITIAL.
          PERFORM: zf_atribui_log USING vl_pernr vl_lgart vl_begda
  vl_endda vl_betrg vl_anzhl vl_afast 'E' text-e05 vl_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM: zf_atribui_log USING vl_pernr vl_lgart vl_begda vl_endda
  vl_betrg vl_anzhl vl_afast 'E' text-e06 vl_tabix.
      CONTINUE.
    ENDIF.

*   Verifica se o funcionário esta afastado
    vl_afast = 'N'.
    FREE: it_2001.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = vl_pernr
        infty           = '2001'
        begda           = vl_begda
        endda           = vl_endda
      TABLES
        infty_tab       = it_2001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF NOT it_2001[] IS INITIAL.
      PERFORM: zf_ver_afastamento CHANGING vl_afast.
    ENDIF.

*   Grava registro
    PERFORM zf_grava_registro USING vl_infty vl_pernr vl_betrg vl_lgart
vl_begda vl_endda vl_anzhl vl_afast <fs_w_infty> vl_tabix <fs_it_infty>.

*   Grava registro na tabela ztb_despmed_td
    PERFORM zf_gravar_valores_ind USING vl_tabix
                                        vl_pernr
                                        vl_betrg
                                        vl_anzhl
                                        vl_endda
                                        vl_begda
                                        vl_lgart
                                        vl_afast
                                        vl_cname
                                        vl_objps
                                        vl_data_emis
                                        vl_tipo_benf.

*   Registra como processado
    IF NOT vl_refer IS INITIAL.
      <fs_sum>-proce = vl_tabix.
    ENDIF.

  ENDLOOP.

  IF <fs_it_infty> IS ASSIGNED.
    UNASSIGN: <fs_it_infty>.
  ENDIF.
  IF <fs_w_infty> IS ASSIGNED.
    UNASSIGN: <fs_w_infty>.
  ENDIF.



ENDFORM.                    " F_PROCESSA

*&---------------------------------------------------------------------*
*&      Form  zf_inicializa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_inicializa.

  FREE : it_data,
         it_log ,
         it_sum ,
         it_lin ,
         it_0000,
         it_0014,
         it_0015,
         it_0267,
         it_2001.

  CLEAR: w_data,
         w_log ,
         w_sum ,
         w_lin ,
         w_0000,
         w_0014,
         w_0015,
         w_0267,
         w_2001.

ENDFORM.                    "zf_inicializa

*&---------------------------------------------------------------------*
*&      Form  zf_atribui_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_atribui_log USING p_pernr TYPE prelp-pernr  " Número pessoal
                          p_lgart TYPE p0015-lgart  " Rúbrica
                          p_begda TYPE prelp-begda  " Data inicio
                          p_endda TYPE prelp-endda  " Data fim
                          p_betrg TYPE p0015-betrg  " Motante
                          p_anzhl TYPE p0015-anzhl  " Número
                          p_afast TYPE char1        "Afastado?
                          p_tipo  TYPE char1        " E=Erro / S=Sucesso
                          p_msg   TYPE char255      " Mensagem de erro
                          p_linha TYPE char10.    " Linha

  CLEAR : w_log.
  w_log-pernr = p_pernr.
  w_log-lgart = p_lgart.
  w_log-begda = p_begda.
  w_log-endda = p_endda.
  w_log-betrg = p_betrg.
  w_log-anzhl = p_anzhl.
  w_log-afast = p_afast.
  w_log-tipo  = p_tipo .
  w_log-msg   = p_msg  .
  w_log-linha = p_linha.
  APPEND: w_log TO it_log.
  CLEAR : w_log.

ENDFORM.                    "zf_atribui_log

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_REGISTRO
*&---------------------------------------------------------------------*
*       GRAVA REGISTRO INFOTIPO
*----------------------------------------------------------------------*
FORM zf_grava_registro USING p_infty TYPE prelp-infty
                             p_pernr TYPE prelp-pernr
                             p_betrg TYPE p0015-betrg
                             p_lgart TYPE p0015-lgart
                             p_begda TYPE prelp-begda
                             p_endda TYPE prelp-endda
                             p_anzhl TYPE p0015-anzhl
                             p_afast TYPE char1
                             p_recor TYPE any
                             p_linha TYPE char10
                             p_table TYPE STANDARD TABLE.

  DATA: vl_seqnr TYPE prelp-seqnr.

  IF p_exc IS INITIAL.

    PERFORM: zf_salva_registro USING p_infty p_pernr p_betrg p_lgart
p_begda p_endda p_anzhl vl_seqnr p_afast p_recor p_linha.

  ELSE.

    LOOP AT p_table INTO p_recor.
*     Exclui Registro Infotipo
      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE p_recor TO <fs_campo>.
      IF sy-subrc IS INITIAL.
        p_begda = <fs_campo>.
      ENDIF.
      ASSIGN COMPONENT 'ENDDA' OF STRUCTURE p_recor TO <fs_campo>.
      IF sy-subrc IS INITIAL.
        p_endda = <fs_campo>.
      ENDIF.
      ASSIGN COMPONENT 'SEQNR' OF STRUCTURE p_recor TO <fs_campo>.
      IF sy-subrc IS INITIAL.
        vl_seqnr = <fs_campo>.
      ENDIF.
      PERFORM: zf_salva_registro USING p_infty p_pernr p_betrg p_lgart
p_begda p_endda p_anzhl vl_seqnr p_afast p_recor p_linha.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "zf_grava_registro


*&---------------------------------------------------------------------*
*&      Form  ZF_SALVA_REGISTRO
*&---------------------------------------------------------------------*
*       GRAVA REGISTRO INFOTIPO
*----------------------------------------------------------------------*
FORM zf_salva_registro USING p_infty TYPE prelp-infty
                             p_pernr TYPE prelp-pernr
                             p_betrg TYPE p0015-betrg
                             p_lgart TYPE p0015-lgart
                             p_begda TYPE prelp-begda
                             p_endda TYPE prelp-endda
                             p_anzhl TYPE p0015-anzhl
                             p_seqnr TYPE prelp-seqnr
                             p_afast TYPE char1
                             p_recor TYPE any
                             p_linha TYPE char10.

  DATA: vl_return TYPE bapireturn1,
        vl_msg    TYPE char255    ,
        vl_opera  TYPE pspar-actio.

* Atribui a ação
  IF p_exc IS INITIAL.
    vl_opera = 'INS'.
    vl_msg   = text-s01.
  ELSE.
    vl_opera = 'DEL'.
    vl_msg   = text-s02.
  ENDIF.

* Bloqueia Matrícula
  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = p_pernr
    IMPORTING
      return = vl_return.

  IF vl_return IS INITIAL.

*   Inclui Registro Infotipo
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = p_infty
        number        = p_pernr
        subtype       = p_lgart
        validityend   = p_endda
        validitybegin = p_begda
        recordnumber  = p_seqnr
        record        = p_recor
        operation     = vl_opera
      IMPORTING
        return        = vl_return.

    IF NOT vl_return IS INITIAL.
      vl_msg = vl_return-message.
      PERFORM: zf_atribui_log USING p_pernr p_lgart p_begda p_endda
p_betrg p_anzhl p_afast 'E' vl_msg p_linha.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
      PERFORM: zf_atribui_log USING p_pernr p_lgart p_begda p_endda
p_betrg p_anzhl p_afast 'S' vl_msg p_linha.
    ENDIF.

*   Desbloqueia Matrícula
    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = p_pernr.

  ELSE.

    vl_msg = vl_return-message.
    PERFORM: zf_atribui_log USING p_pernr p_lgart p_begda p_endda
p_betrg p_anzhl p_afast 'E' vl_msg p_linha.

  ENDIF.

ENDFORM.                    "zf_salva_registro

*&---------------------------------------------------------------------*
*&      Form  ZF_MOSTRA_LOG_PROCES
*&---------------------------------------------------------------------*
FORM zf_mostra_log_proces.

  DATA: vl_aux   TYPE          char100    ,
        vl_tot   TYPE          sy-tabix   ,
        vl_err   TYPE          sy-tabix   ,
        vl_suc   TYPE          sy-tabix   ,
        vl_betrg TYPE          p0015-betrg,
        vl_anzhl TYPE          p0015-anzhl,
        itl_logs TYPE TABLE OF y_log      ,
        itl_loge TYPE TABLE OF y_log      .

  CHECK NOT it_log[] IS INITIAL.

* Ordena tabela para impressão
  SORT it_log BY linha tipo lgart.

* Atribui tabelas para impressão
  itl_loge[] = itl_logs[] = it_log[].

* Matem nas tabelas os registros de acordo com o tipo da mensagem
  DELETE        : itl_loge WHERE ('TIPO = ''S'''),
                  itl_logs WHERE ('TIPO = ''E''').

* Gera totais
  CLEAR         : vl_tot,
                  vl_err,
                  vl_suc.
  DESCRIBE TABLE: it_log   LINES vl_tot,
                  itl_loge LINES vl_err,
                  itl_logs LINES vl_suc.

* Pula a primeira linha
  SKIP.

* Cor na linha inicial
  FORMAT COLOR 1 INTENSIFIED ON.

* Titulo para o report
  IF     NOT p_i14    IS INITIAL.
    CONCATENATE text-r01 text-r02 INTO vl_aux SEPARATED BY space.
  ELSEIF NOT p_i15    IS INITIAL.
    CONCATENATE text-r01 text-r03 INTO vl_aux SEPARATED BY space.
  ELSEIF NOT p_i15_un IS INITIAL.
    CONCATENATE text-r01 text-r04 INTO vl_aux SEPARATED BY space.
  ELSE.
    CONCATENATE text-r01 text-r05 INTO vl_aux SEPARATED BY space.
  ENDIF.

  WRITE: /01  sy-uline,
         /01  sy-vline,
          25  vl_aux CENTERED,  " Log de processamento
          180 sy-vline,
         /01  sy-uline.
  FORMAT COLOR OFF.

*   Linhas verticais em campos vazio da tela de log
  WRITE: /01  sy-vline,
          180 sy-vline.

* Imprime o total de funcionários processados
  WRITE: /01  sy-vline,
          05  text-r06,     " Total Funcionários Processados:
          60  vl_tot  ,
          180 sy-vline.

* Imprime o total de registros processados com sucesso
  WRITE: /01  sy-vline,
          05  text-r07,     " Total de registros processados com sucesso
:
          60  vl_suc  ,
          180 sy-vline.

* Imprime o total de erros apresentados no processamento
  WRITE: /01  sy-vline,
          05  text-r08,     " Total de erros apresentados no
 " processamento:
          60  vl_err  ,
          180 sy-vline.

* Insere linhas verificais nos em branco
  WRITE: /01  sy-vline,
          180 sy-vline.

* Linha
  ULINE.

  IF NOT vl_suc IS INITIAL.

*   Insere linhas verificais nos em branco
    WRITE: /01  sy-vline,
            180 sy-vline.

*   Imprime Funcionários processados:
    WRITE: /01  sy-vline      ,
            05  '@08@' AS ICON,
            10  text-r09      ,     " Funcionários processados com
" sucesso:
            180 sy-vline      .

*   Insere linhas verificais nos em branco
    WRITE: /01  sy-vline,
            180 sy-vline.

    LOOP AT itl_logs INTO w_log.

      AT NEW lgart.
        CLEAR: vl_betrg,
               vl_anzhl.
        WRITE: /01 sy-vline,
               05  text-r11,  " Linha
               20  text-r12,  " Funcionário
               35  text-r13,  " Afast
               45  text-r14,  " Rubrica
               55  text-r15,  " Dt.Início
               70  text-r16,  " Dt.Final
               85  text-r17,  " Quantidade
               109 text-r18,  " Montante
               125 text-r19,  " Mensagem
               180 sy-vline     .
        ULINE.
      ENDAT.

      WRITE: /01 sy-vline   ,
             05  w_log-linha,
             20  w_log-pernr,
             35  w_log-afast,
             45  w_log-lgart,
             55  w_log-begda,
             70  w_log-endda,
             85  w_log-anzhl,
             100 w_log-betrg,
             125 w_log-msg  ,
             180 sy-vline   .

      vl_betrg = vl_betrg + w_log-betrg.
      vl_anzhl = vl_anzhl + w_log-anzhl.

      AT END OF lgart.
        ULINE.
        WRITE: /01 sy-vline,
               70  text-r20, " TOTAL
               85  vl_anzhl,
               100 vl_betrg,
               180 sy-vline.
        ULINE.
      ENDAT.

    ENDLOOP.

  ENDIF.

  IF NOT vl_err IS INITIAL.

*   Insere linhas verificais nos em branco
    WRITE: /01  sy-vline,
            180 sy-vline.

*   Imprime Funcionários processados:
    WRITE: /01 sy-vline       ,
            05  '@0A@' AS ICON,
            10  text-r10      ,     " Funcionários processados com erro:
            180 sy-vline      .

*   Insere linhas verificais nos em branco
    WRITE: /01  sy-vline,
            180 sy-vline.

    LOOP AT itl_loge INTO w_log.

      AT NEW lgart.
        CLEAR: vl_betrg,
               vl_anzhl.
        WRITE: /01 sy-vline    ,
               05  text-r11,  " Linha
               20  text-r12,  " Funcionário
               35  text-r13,  " Afast
               45  text-r14,  " Rubrica
               55  text-r15,  " Dt.Início
               70  text-r16,  " Dt.Final
               85  text-r17,  " Quantidade
               109 text-r18,  " Montante
               125 text-r19,  " Mensagem
               180 sy-vline     .
        ULINE.
      ENDAT.

      WRITE: /01 sy-vline   ,
             05  w_log-linha,
             20  w_log-pernr,
             35  w_log-afast,
             45  w_log-lgart,
             55  w_log-begda,
             70  w_log-endda,
             85  w_log-anzhl,
             100 w_log-betrg,
             125 w_log-msg  ,
             180 sy-vline   .

      vl_betrg = vl_betrg + w_log-betrg.
      vl_anzhl = vl_anzhl + w_log-anzhl.

      AT END OF lgart.
        ULINE.
        WRITE: /01 sy-vline,
               70  text-r20, " TOTAL
               85  vl_anzhl,
               100 vl_betrg,
               180 sy-vline.
        ULINE.
      ENDAT.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " ZF_MOSTRA_LOG_PROCES

*&---------------------------------------------------------------------*
*&      Form  ZF_INICIA_INFOTIPOS
*&---------------------------------------------------------------------*
FORM zf_inicia_infotipos CHANGING ch_infty TYPE prelp-infty.

* Inicializa varáveis de controle geral
  IF <fs_it_infty> IS ASSIGNED.
    UNASSIGN: <fs_it_infty>.
  ENDIF.
  IF <fs_w_infty> IS ASSIGNED.
    UNASSIGN: <fs_w_infty>.
  ENDIF.
* Opção Infotipo 0014
  IF     NOT p_i14  IS INITIAL.
    ch_infty = '0014'.
    ASSIGN: ('IT_0014') TO <fs_it_infty>,
            ('W_0014')  TO <fs_w_infty> .
* Opção Infotipo 0267
  ELSEIF NOT p_i267 IS INITIAL.
    ch_infty = '0267'.
    ASSIGN: ('IT_0267') TO <fs_it_infty>,
            ('W_0267')  TO <fs_w_infty> .
* Opção Infotipo 0015
  ELSE.
    ch_infty = '0015'.
    ASSIGN: ('IT_0015') TO <fs_it_infty>,
            ('W_0015')  TO <fs_w_infty> .
  ENDIF.

ENDFORM.                    " ZF_INICIA_INFOTIPOS

*&---------------------------------------------------------------------*
*&      Form  ZF_CONVERTE_VALORES
*&---------------------------------------------------------------------*
FORM zf_converte_valores  USING    p_data   TYPE y_data
                                   p_tabix  TYPE char10
                          CHANGING ch_pernr TYPE prelp-pernr
                                   ch_betrg TYPE p0015-betrg
                                   ch_anzhl TYPE p0015-anzhl
                                   ch_endda TYPE prelp-endda
                                   ch_begda TYPE prelp-begda
                                   ch_lgart TYPE p0015-lgart
                                   ch_conv  TYPE c
                                   ch_cname TYPE ztb_despmed_td-cname
                                   ch_objps TYPE ztb_despmed_td-objps
                                   ch_data_emis TYPE ztb_despmed_td-data
                                   ch_tipo_benf TYPE ztb_despmed_td-tipo
.

  DATA: vl_msg TYPE char255,   " Mensagem
        vl_aux TYPE char20 ,   " Auxiliar
        vl_mes TYPE i      .

* Inicia variável de controle das conversões
  CLEAR: ch_conv.

* Lê os campos do I0015 (Unimed) que serão gravados na tabela
* ZTB_DESPMED_TD
  CATCH SYSTEM-EXCEPTIONS convt_no_number = 1
                            convt_overflow  = 2.
    IF NOT p_i15_un IS INITIAL.
      ch_cname = p_data-linha+46(100).
      ch_objps = p_data-linha+19(11).
     ch_data_emis = replace( val = p_data-linha+501(10) sub = '/' with =
 '').
      ch_tipo_benf = p_data-linha+514(10).
    ENDIF.
  ENDCATCH.


* Atribui número pessoal convertendo
  CATCH SYSTEM-EXCEPTIONS convt_no_number = 1
                          convt_overflow  = 2.
*     Trata o layout conforme o tipo escolhido na tela de seleção
    IF NOT p_i15_un IS INITIAL.
      ch_pernr = p_data-linha+490(8).
    ELSE.
      ch_pernr = p_data-linha+0(8).
    ENDIF.
  ENDCATCH.
  IF NOT sy-subrc IS INITIAL.
    CLEAR: ch_pernr.
    CONCATENATE text-e03 p_tabix INTO vl_msg SEPARATED BY space.
    PERFORM: zf_atribui_log USING ch_pernr ch_lgart ch_begda ch_endda
ch_betrg ch_anzhl space 'E' vl_msg p_tabix.
    ch_conv = 'E'.
    RETURN.
  ENDIF.

* Atribui rubrica
  IF NOT p_i14  IS INITIAL OR
     NOT p_i15  IS INITIAL OR
     NOT p_i267 IS INITIAL.
    ch_lgart = p_data-linha+8(4).
  ELSE.
    ch_lgart = c_desconto_copart.
  ENDIF.

* Atribui montante convertendo
  CATCH SYSTEM-EXCEPTIONS convt_no_number = 1
                          convt_overflow  = 2.
    IF     NOT p_i14 IS INITIAL.
      CONCATENATE p_data-linha+37(13) '.' p_data-linha+50(2) INTO vl_aux
.
      ch_betrg = vl_aux.
    ELSEIF NOT p_i15_un IS INITIAL.
      CONCATENATE p_data-linha+369(13) '.' p_data-linha+382(2) INTO
vl_aux.
      ch_betrg = vl_aux.
    ELSE.
      CONCATENATE p_data-linha+29(13) '.' p_data-linha+42(2) INTO vl_aux
.
      ch_betrg = vl_aux.
    ENDIF.
  ENDCATCH.
  IF NOT sy-subrc IS INITIAL.
    CLEAR: ch_betrg.
    CONCATENATE text-e04 p_tabix INTO vl_msg SEPARATED BY space.
    PERFORM: zf_atribui_log USING ch_pernr ch_lgart ch_begda ch_endda
ch_betrg ch_anzhl space 'E' vl_msg p_tabix.
    ch_conv = 'E'.
    RETURN.
  ENDIF.

* Atribui quantidade convertendo
  CATCH SYSTEM-EXCEPTIONS convt_no_number = 1
                          convt_overflow  = 2.
    IF     NOT p_i14 IS INITIAL.
      CONCATENATE p_data-linha+28(7) '.' p_data-linha+35(2) INTO vl_aux.
      ch_anzhl = vl_aux.
    ELSEIF NOT p_i15 IS INITIAL OR
           NOT p_i267 IS INITIAL.
      CONCATENATE p_data-linha+20(7) '.' p_data-linha+27(2) INTO vl_aux.
      ch_anzhl = vl_aux.
    ELSEIF NOT p_i15_un IS INITIAL.
      CONCATENATE p_data-linha+355(11) '' INTO vl_aux.
      ch_anzhl = vl_aux.
    ENDIF.
  ENDCATCH.
  IF NOT sy-subrc IS INITIAL.
    CLEAR: ch_anzhl.
    CONCATENATE text-e09 p_tabix INTO vl_msg SEPARATED BY space.
    PERFORM: zf_atribui_log USING ch_pernr ch_lgart ch_begda ch_endda
ch_betrg ch_anzhl space 'E' vl_msg p_tabix.
    ch_conv = 'E'.
    RETURN.
  ENDIF.

* Atribui datas convertendo
  CATCH SYSTEM-EXCEPTIONS convt_no_number = 1
                          convt_overflow  = 2.
    IF NOT p_i14  IS INITIAL OR
       NOT p_i15  IS INITIAL.
      CONCATENATE p_data-linha+16(4) p_data-linha+14(2)
p_data-linha+12(2) INTO ch_begda.
      IF NOT p_i14 IS INITIAL.
*       Regra: Se o Infotipo for IT0014 e a rubrica 4036, o ENDDA será
*       formado pelo begda + anzhl
        IF ch_lgart = '4036'.
          vl_mes = ch_anzhl.
          CALL FUNCTION 'ISH_DATE_ADD_MONTH'
            EXPORTING
              olddate    = ch_begda
              diffmonths = vl_mes
            IMPORTING
              newdate    = ch_endda
            EXCEPTIONS
              wrong_date = 1
              OTHERS     = 2.
        ELSE.
          CONCATENATE p_data-linha+24(4) p_data-linha+22(2)
  p_data-linha+20(2) INTO ch_endda.
        ENDIF.
      ELSE.
        ch_endda = '99991231'.
      ENDIF.
    ELSE.
*     Para o IT 0267, a data inicio será a informada na tela de seleção
      IF NOT p_i267 IS INITIAL.
        ch_begda = p_dat.
      ELSE.
        ch_begda = sy-datum.
      ENDIF.
      ch_endda = '99991231'.
    ENDIF.
  ENDCATCH.
  IF NOT sy-subrc IS INITIAL.
    CLEAR: ch_begda,
           ch_endda.
    CONCATENATE text-e13 p_tabix INTO vl_msg SEPARATED BY space.
    PERFORM: zf_atribui_log USING ch_pernr ch_lgart ch_begda ch_endda
  ch_betrg ch_anzhl space 'E' vl_msg p_tabix.
    ch_conv = 'E'.
    RETURN.
  ENDIF.

ENDFORM.                    " ZF_CONVERTE_VALORES

*&---------------------------------------------------------------------*
*&      Form  ZF_VER_AFASTAMENTO
*&---------------------------------------------------------------------*
FORM zf_ver_afastamento CHANGING ch_afast TYPE char1.

  LOOP AT it_2001 INTO w_2001.
    IF w_2001-awart EQ '0210' OR
       w_2001-awart EQ '0211' OR
       w_2001-awart EQ '0230' OR
       w_2001-awart EQ '0240' OR
       w_2001-awart EQ '0241' OR
       w_2001-awart EQ '0251' OR
       w_2001-awart EQ '0252'.
      ch_afast = 'S'.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ZF_VER_AFASTAMENTO

*&---------------------------------------------------------------------*
*&      Form  ZF_SUMARIZA_REGISTROS
*&---------------------------------------------------------------------*
FORM zf_sumariza_registros.

  DATA: vl_pernr TYPE prelp-pernr,   " Número pessoal
        vl_betrg TYPE p0015-betrg,   " Montante
        vl_lgart TYPE p0015-lgart,   " Rubrica
        vl_begda TYPE prelp-begda,   " Data inicio
        vl_endda TYPE prelp-endda,   " Data fim
        vl_anzhl TYPE p0015-anzhl,   " Quantidade
        vl_tabix TYPE char10     ,   " Indice atual
        vl_conv  TYPE c          ,   " Controle das conversões
        vl_cname TYPE ztb_despmed_td-cname, " Nome
        vl_objps TYPE ztb_despmed_td-objps, " CPF
        vl_data_emis TYPE ztb_despmed_td-data, " Data de emissão
        vl_tipo_benf TYPE ztb_despmed_td-tipo. " Tipo de Beneficiário

  LOOP AT it_data INTO w_data.

    vl_tabix = sy-tabix.

    CONDENSE: vl_tabix.

*   Não considera o primeiro registro para o IT 0015 - Pão de Açucar
**    IF sy-tabix EQ 1 AND NOT p_i15_pa IS INITIAL.
**      CONTINUE.
**    ENDIF.

*   Inicializa variáveis
    CLEAR: vl_pernr,
           vl_betrg,
           vl_lgart,
           vl_begda,
           vl_endda,
           vl_anzhl,
           vl_cname,
           vl_objps,
           vl_data_emis,
           vl_tipo_benf.

    PERFORM: zf_converte_valores USING    w_data
                                          vl_tabix
                                 CHANGING vl_pernr
                                          vl_betrg
                                          vl_anzhl
                                          vl_endda
                                          vl_begda
                                          vl_lgart
                                          vl_conv
                                          vl_cname
                                          vl_objps
                                          vl_data_emis
                                          vl_tipo_benf.

*   Se algum campo apresentou problemas, grava como processado
    IF vl_conv EQ 'E'.

      w_lin-linha = vl_tabix.
      APPEND: w_lin TO it_lin.
      CLEAR : w_lin.

*   Se não, sumariza valores
    ELSE.

      CONCATENATE vl_pernr vl_lgart vl_begda(6) INTO w_sum-refer.
      w_sum-betrg = vl_betrg.
      w_sum-anzhl = vl_anzhl.
      COLLECT: w_sum INTO it_sum.
      CLEAR  : w_sum.

    ENDIF.

  ENDLOOP.

* Ordena
  SORT: it_sum BY refer,
        it_lin BY linha.

ENDFORM.                    " ZF_SUMARIZA_REGISTROS

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_ARQ_DIRERIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_buscar_arq_direrio USING p_dirname      TYPE rlgrap-filename
                                 p_sp_name      TYPE filename_al11.
  DATA: vl_errcnt     TYPE i,
        vl_sap_no     TYPE c,
        vl_sap_yes    TYPE c VALUE 'X',
        vl_dirname    TYPE dirname_al11.

  vl_dirname = p_dirname.


  REFRESH it_file.
  CALL 'C_DIR_READ_START' ID 'DIR'    FIELD vl_dirname   "/usr/sap/trans
                          ID 'FILE'   FIELD p_sp_name " *
                          ID 'ERRNO'  FIELD w_file-errno
                          ID 'ERRMSG' FIELD w_file-errmsg.
  IF sy-subrc IS INITIAL.
    DO.
      CLEAR: w_file, v_erro.
      CALL 'C_DIR_READ_NEXT'
        ID 'TYPE'   FIELD w_file-type
        ID 'NAME'   FIELD w_file-name
        ID 'LEN'    FIELD w_file-len
        ID 'OWNER'  FIELD w_file-owner
        ID 'MTIME'  FIELD w_file-mtime
        ID 'MODE'   FIELD w_file-mode
        ID 'ERRNO'  FIELD w_file-errno
        ID 'ERRMSG' FIELD w_file-errmsg.
      w_file-dirname = vl_dirname  .
      MOVE sy-subrc TO w_file-subrc.
      IF sy-subrc = 5.
        sy-subrc = 0.
      ENDIF.
      CASE sy-subrc.
        WHEN 0.
          CLEAR: w_file-errno, w_file-errmsg.
          CASE w_file-type(1).
            WHEN c_file_f .                    " normal file.
              MOVE vl_sap_yes  TO w_file-useable.
            WHEN c_file_min .                    " normal file.
              MOVE vl_sap_yes  TO w_file-useable.
            WHEN OTHERS. " directory, device, fifo, socket,...
              MOVE vl_sap_no  TO w_file-useable.
          ENDCASE.
          IF w_file-len = 0.
            MOVE vl_sap_no TO w_file-useable.
          ENDIF.
        WHEN 1.
          EXIT.
        WHEN OTHERS.                     " SY-SUBRC >= 2
          ADD 1 TO vl_errcnt.
          IF vl_errcnt > 10.
            EXIT.
          ENDIF.
          MOVE vl_sap_no TO w_file-useable.
      ENDCASE.
      IF w_file-useable NE vl_sap_no.

***     Validar extensão e nomenclatura do arquivo.
        PERFORM zf_valida_nome_arquivo USING w_file-name
                                       CHANGING v_erro.

        IF v_erro NE c_flag.
          APPEND w_file-name TO it_file.  "global table
        ELSE.
          CONCATENATE v_message w_file-name c_pontov INTO v_message
SEPARATED BY space.
        ENDIF.

      ENDIF."IF w_file-useable NE vl_sap_no.

    ENDDO.

    CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD w_file-errno
        ID 'ERRMSG' FIELD w_file-errmsg.
  ENDIF."CALL 'C_DIR_READ_START'
ENDFORM.                    " ZF_BUSCAR_ARQ_DIRERIO


*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_NOME_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_FILE_NAME  text
*      <--P_V_ERRO  text
*----------------------------------------------------------------------*
FORM zf_valida_nome_arquivo  USING    p_file_name TYPE filename_al11
                             CHANGING ch_erro .

  DATA: vl_name       TYPE filename_al11,
        wl_result     TYPE match_result.

  vl_name = p_file_name.
  TRANSLATE vl_name TO LOWER CASE.
** Verificar se a extensão do arquivo é TXT
  FIND c_extensao IN vl_name RESULTS wl_result .
  IF sy-subrc IS NOT INITIAL.
    ch_erro = c_flag.
    RETURN.
  ENDIF.

  CLEAR vl_name.
ENDFORM.                    " ZF_VALIDA_NOME_ARQUIVO


*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_ARQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_busca_arq USING      p_file  TYPE rlgrap-filename
                  CHANGING   ch_arq  LIKE LINE OF it_data
                             ch_tl_arq TYPE STANDARD TABLE.

  OPEN DATASET p_file FOR INPUT IN TEXT MODE
                              ENCODING DEFAULT.

  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF.

  DO.
    CLEAR ch_arq.
    READ DATASET p_file INTO ch_arq.

    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    IF NOT ch_arq IS INITIAL.
      APPEND ch_arq TO ch_tl_arq.
    ENDIF.
  ENDDO.

  CLOSE DATASET p_file.

ENDFORM.                    " ZF_BUSCA_ARQ
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVAR_VALORES_IND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_gravar_valores_ind USING p_tabix TYPE char10
                                 p_pernr TYPE prelp-pernr
                                 p_betrg TYPE p0015-betrg
                                 p_anzhl TYPE p0015-anzhl
                                 p_endda TYPE prelp-endda
                                 p_begda TYPE prelp-begda
                                 p_lgart TYPE p0015-lgart
                                 p_afast TYPE c
                                 p_cname TYPE ztb_despmed_td-cname
                                 p_objps TYPE ztb_despmed_td-objps
                                 p_data_emis TYPE ztb_despmed_td-data
                                 p_tipo_benf TYPE ztb_despmed_td-tipo.


  DATA(lw_despmed_td) = VALUE st_despmed_td( pernr = p_pernr cname =
p_cname objps = p_objps copar = p_betrg data = p_data_emis tipo =
p_tipo_benf ).

  INSERT INTO ztb_despmed_td VALUES lw_despmed_td.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    PERFORM: zf_atribui_log USING p_pernr p_lgart p_begda
      p_endda p_betrg p_anzhl p_afast 'E' text-e14 p_tabix.
  ENDIF.

ENDFORM.