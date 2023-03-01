DATA lo_data       TYPE REF TO data.
DATA lo_tabdescr   TYPE REF TO cl_abap_tabledescr.
DATA lo_strucdescr TYPE REF TO cl_abap_structdescr.
DATA lo_elemdescr  TYPE REF TO cl_abap_elemdescr.
DATA lt_components TYPE cl_abap_structdescr=>component_table.

FIELD-SYMBOLS <lfs_value> TYPE any.

lo_tabdescr   ?= cl_abap_tabledescr=>describe_by_data( lt_table ). "Internal table
lo_strucdescr ?= lo_tabdescr->get_table_line_type( ).
lt_components = lo_strucdescr->get_components( ).

LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<lfs_structure>).
  LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<lfs_component>).
    "IF YOU NEED FIELDCATALOG-data ...
    "lo_elemdescr ?= cl_abap_elemdescr=>describe_by_name( <lfs_component>-type->get_relative_name( ) ).

    "Check NAME / ABSOLUTE / RELATIVE Name as Mapping-Source
    ASSIGN COMPONENT <lfs_component>-name OF STRUCTURE <lfs_structure> TO <lfs_value>.
    ASSIGN COMPONENT <lfs_component>-type->absolute_name+6 OF STRUCTURE <lfs_structure> TO <lfs_value>.
    ASSIGN COMPONENT <lfs_component>-type->get_relative_name( ) OF STRUCTURE <lfs_structure> TO <lfs_value>.

    IF <lfs_value> IS ASSIGNED.
      "DO YOUR THING ...
    ENDIF.

  ENDLOOP.
  UNASSIGN <component>.
ENDLOOP.
UNASSIGN <structure>.