TYPES: BEGIN OF ly_header,
      column TYPE char40,
       END OF ly_header.
  TYPES: ls_header TYPE TABLE OF ly_header WITH EMPTY KEY.

  DATA(lt_header) = VALUE ls_header(
  ( CONV ly_header-column( text-c01 ) ) "Coluna 1
  ( CONV ly_header-column( text-c02 ) ) "Coluna 2
 ).

  gv_path = COND #( WHEN gv_path IS INITIAL THEN p_file ELSE gv_path ).
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = gv_path
      filetype                = 'ASC'
      write_field_separator   = abap_true
    TABLES
      data_tab                = it_saida
      fieldnames              = lt_header
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
      OTHERS                  = 22.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.