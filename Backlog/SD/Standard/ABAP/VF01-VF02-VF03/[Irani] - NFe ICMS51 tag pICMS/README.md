## Preenchimento da tag vICMS no XML da NFe quando for ICMS51

#### Transações

- VF01 - Criar documento de fatura
- VF03 - Visualizar documento de fatura

#### BADI

- J_1BNF_ADD_DATA

#### Métodos

- ADD_DATA

#### Exemplo de relações das tabelas

IT_VBRP:

- AUBEL = 0000431680
- AUPOS = 000010

VBAP:
- VBELN = 0000431680
- POSNR = 000010
- KNUMV_ANA = 0000135913

PRCD_ELEMENTS:
- KNUMV = 0000135913
- KPOSN = 000010
- KSCHL = 'BX13'

ET_ITEM:
- PICMSDEF = PRCD_ELEMENTS-KBETR