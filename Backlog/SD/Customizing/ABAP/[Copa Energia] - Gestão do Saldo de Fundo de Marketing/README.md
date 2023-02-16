# Gestão do Saldo de Fundo de Marketing

### Relações das tabelas:

VBAK - Documento de vendas: dados de cabeçalho

- VBELN (VBELN_VA): Documento de vendas
- VGBEL: Nº documento do documento de referência
- OBJNR: Nº objeto a nível de cabeçalho
- BUKRS_VF: Empresa que efetua o faturamento
- ERDAT: Data de criação do registro

VBAP - Documento de vendas: dados de item
- VBELN: Documento de vendas (VBAK-VBELN)
- POSNR: Item do documento de vendas

VBRK - Documento de faturamento: dados de cabeçalho
- VBELN (VBELN_VF): Documento de faturamento
- BUKRS: Empresa (BSEG-BUKRS)
- ERDAT: Data de criação do registro

VBRP - Documento de faturamento: dados de item
- VBELN: Documento de faturamento
- POSNR: Item do documento de faturamento
- VGBEL: Nº documento do documento de referência
- AUBEL: Documento de vendas (VBAK-VBELN)

BKPF - Cabeçalho do documento contábil
- BUKRS: Empresa
- BELNR: Nº documento de um documento contábil
- GJAHR: Exercício
- AWKEY: Chave referência

BSEG - Segmento do documento contabilidade financeira
- BUKRS: Empresa
- BELNR: Nº documento de um documento contábil (BKPF-BELNR)
- GJAHR: Exercício
- BUZEI: Nº linha de lançamento no documento contábil
- AUGDT: Data de compensação
- AUGCP: Data de entrada da compensação
- AUGBL: Nº documento de compensação