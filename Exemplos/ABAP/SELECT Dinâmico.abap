FIELD-SYMBOLS: <lfs_table> TYPE ANY TABLE.

DATA lo_data       TYPE REF TO data.
DATA lo_strucdescr TYPE REF TO cl_abap_structdescr.
DATA lt_components TYPE cl_abap_structdescr=>component_table.

lo_strucdescr ?= cl_abap_typedescr=>describe_by_name( p_tabela ).
lt_components = lo_strucdescr->get_components( ).

* Monta tabela de fieldcat
DATA(lt_fieldcat) = VALUE lvc_t_fcat( FOR <lfs_component> IN lt_components
                     (
                     fieldname = <lfs_component>-name
                     ref_field = <lfs_component>-name
                     ref_table = p_tabela
                     )
                    ).

* Monta tabela com os campos do select
DATA(lt_campos) = VALUE string_table( FOR <lfs_component> IN lt_components
                               (
                               <lfs_component>-name
                               )
                  ).

* Monta tabela do tipo do fieldcat
CALL METHOD cl_alv_table_create=>create_dynamic_table
  EXPORTING
    it_fieldcatalog = lt_fieldcat
  IMPORTING
    ep_table        = lo_data.

* Associa a referência da tabela
ASSIGN lo_data->* TO <lfs_table>.

SELECT (lt_campos) FROM (p_tabela) INTO TABLE <lfs_table>.