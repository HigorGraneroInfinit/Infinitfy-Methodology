# Endereço para entrega de cesta básica

## Tabelas utilizadas

##### PA0000 - Registro mestre HR infotipo 0000 (Medidas)

- PERNR: Nº pessoal

##### PA0001 - Registro mestre HR: infotipo 0001 (atrib.org.)

- PERNR: Nº pessoal
- BUKRS: Empresa
- WERKS: Área
- BTRTL: Subárea

##### PA0002 - Registro mestre HR infotipo 0002 (Dados pessoais)

- PERNR: Nº pessoal
- CNAME: Nome Pessoa

##### PA0006 - Registro mestre HR infotipo 0006 (Endereços) ZHR_BENEF_COD_END_CESTA = SUBTY

- PERNR: Nº pessoal
- STRAS: Rua
- HSNMR: Número
- POSTA: Complemento
- ORT02: Bairro
- ORT01: Cidade
- STATE: UF
- LAND1: PAÍS
- PSTLZ: CEP

##### T500P - Áreas de recursos humanos

- PERSA: Área de recursos humanos (PA0001-WERKS)
- NAME1: Texto da área recursos humanos

##### T001P - Áreas/subáreas HR

- WERKS: Área de recursos humanos (PA0001-WERKS)
- BTRTL: Subárea de recursos humanos (PA0001-BTRTL)
- BTEXT: Texto p/subárea de recursos humanos

##### T7BR0P - Assinalamento de áreas de pessoal a grupos de filiais/obras

- WERKS: Área de recursos humanos (PA0001-WERKS)
- BTRTL: Subárea de recursos humanos (PA0001-BTRTL)
- GRPBR: Grupo de filiais

##### T7BRAP - Agrupamento de filiais e/ou obras

- GRPBR: Grupo de filiais (T7BR0P-GRPBR)
- ENDDA: Fim da validade
- BUKRS: Empresa (PA0001-BUKRS)
- FILIA: Local de negócios