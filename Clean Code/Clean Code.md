# Clean Code

Para que os desenvolvimentos feitos pela Infinitfy possuam um código limpo (Clean Code), é imprescindível a utilização das seguintes práticas.

## Conteúdo

- [Como refatorar código legado](#Como-refatorar-código-legado)
- [Nomeando Objetos](#Nomeando-Objetos)
  - [Use nomes descritivos](#Use-nomes-descritivos)
  - [Use nomes pronunciáveis](#Use-nomes-pronunciáveis)
  - [Evite abreviaturas](#Evite-abreviaturas)
  - [Use as mesmas abreviações em todos os lugares](#Use-as-mesmas-abreviações-em-todos-os-lugares)
  - [Use substantivos para classes e verbos para métodos](#Use-substantivos-para-classes-e-verbos-para-métodos)
  - [Evite palavras de ruído ](#Evite-palavras-de-ruído )
  - [Escolha uma palavra por conceito](#Escolha-uma-palavra-por-conceito )
  - [Uso de nomes de design patterns](#Uso-de-nomes-de-design-patterns)
  - [Evite utilizar nomes de funções built-in](#Evite-utilizar-nomes-de-funções-built-in)
- [Linguagem](#Linguagem)
  - [Prefira a orientação a objetos à programação processual](#Prefira-a-orientação-a-objetos-à-programação-processual)
  - [Prefira construções de linguagem funcional a procedimental](#Prefira-construções-de-linguagem-funcional-a-procedimental)
  - [Evite elementos de linguagem obsoletos](#Evite-elementos-de-linguagem-obsoletos)
  - [Use padrões de design com sabedoria](#Use-padrões-de-design-com-sabedoria)
- [Constantes](#Constantes)
  - [Use constantes ao invés de hardcode](#Use-constantes-ao-invés-de-hardcode)
  - [Agrupamento](#[Agrupamento]())
    - [Classes de Enumeração](#Classes-de-Enumeração)
    - [Estruturas Constantes](#Estruturas-Constantes)
- [Variáveis](#Variáveis)
  - [Declarações em Métodos](#Declarações-em-Métodos)
  - [Declarações dentro de IF-ELSE](#Declarações-dentro-de-IF-ELSE)
  - [Não encadeie declarações iniciais](#Não-encadeie-declarações-iniciais)
  - [Conversão de dados](#Conversão-de-dados)
- [Selects](#Selects)
  - [Minimizando a criação de TYPES](#Minimizando-a-criação-de-TYPES)
  - [Retornando valores dinâmicos](#Retornando-valores-dinâmicos)
  - [Realizando cálculos](#Realizando-cálculos)
- [Tabelas](#Tabelas)
  - [Utilize o tipo de tabela correto](#Utilize-o-tipo-de-tabela-correto)
    - [Hashed Table](#Hashed-Table)
    - [Sorted Table](#Sorted-Table)
    - [Standard Table](#Standard-Table)
  - [Chaves](#Chaves)
  - [Inserção de dados](#Inserção-de-dados)
    - [Preenchendo uma tabela durante a sua criação](#Preenchendo-uma-tabela-durante-a-sua-criação)
  - [Buscando linhas](#Buscando-linhas)
    - [Por Índice](#Por-Índice)
    - [Por valor de um ou mais campos específicos](#Por-valor-de-um-ou-mais-campos-específicos)
  - [Retornando o índice de uma linha](#Retornando-o-índice-de-uma-linha)
  - [Acessando campos de uma linha diretamente](#Acessando-campos-de-uma-linha-diretamente)
  - [Passando dados de uma tabela para outra](#Passando-dados-de-uma-tabela-para-outra)
    - [Copiando uma tabela](#Copiando-uma-tabela)
    - [Adicionando linhas a uma tabela existente](#Adicionando-linhas-a-uma-tabela-existente)
    - [Tabelas de tipos diferentes](#Tabelas-de-tipos-diferentes)
    - [Filtrando dados](#Filtrando-dados)
  - [Utilizando LOOP AT](#Utilizando-LOOP-AT)
    - [Quando utilizar](#Quando-utilizar)
    - [Utilize FIELD SYMBOL ao invés de MODIFY](#Utilize-FIELD-SYMBOL-ao-invés-de-MODIFY)
  - [Verificando a existência de uma linha](#Verificando-a-existência-de-uma-linha)
  - [Evite leituras desnecessárias de tabelas](#Evite-leituras-desnecessárias-de-tabelas)
- [Strings](#Strings)
  - [Definindo textos](#Definindo-textos)
  - [Concatenação](#Concatenação)
  - [Adicionando zeros à esquerda](#Adicionando-zeros-à-esquerda)
  - [Removendo zeros à esquerda](#Removendo-zeros-à-esquerda)
- [Booleanos](#Booleanos)
  - [Use booleanos com sabedoria](#Use-booleanos-com-sabedoria)
  - [Use ABAP_BOOL para booleanos](#Use-ABAP_BOOL-para-booleanos)
  - [Use ABAP_TRUE e ABAP_FALSE para comparações](#Use-ABAP_TRUE-e-ABAP_FALSE-para-comparações)
  - [Use XSDBOOL para definir variáveis booleanas](#Use-XSDBOOL-para-definir-variáveis-booleanas)
- [Condições](#Condições) 
  - [Tente tornar as condições positivas](#Tente-tornar-as-condições-positivas)
  - [Prefira IS NOT do que NOT IS](#Prefira-IS-NOT-do-que-NOT-IS)
  - [Chamadas de método predicativo para métodos booleanos](#Chamadas-de-método-predicativo-para-métodos-booleanos)
  - [Decomponha condições complexas](#Decomponha-condições-complexas)
  - [Extraia condições complexas](#Extraia-condições-complexas)
- [Estruturas Condicionais](#Estruturas-Condicionais)
  - [IF ELSE](#IF ELSE)
  - [CASE](#CASE)
  - [SWITCH](#SWITCH)
  - [COND](#COND)
  - [Mantenha Alinhamento Simples](#Mantenha-Alinhamento-Simples)
  - [Retorno de funções e métodos em expressões lógicas](#Retorno-de-funções-e-métodos-em-expressões-lógicas)
    - [Omitindo ABAP_TRUE](#Omitindo-ABAP_TRUE)
- [Classes](#Classes)
  - [Instanciando objetos](#Instanciando-objetos)
    - [Tipo dinâmico](#Tipo-dinâmico)

- [Métodos](#Métodos)
  - [Chamadas](#Chamadas)
    - [Chamada de métodos estáticos](#Chamada-de-métodos-estáticos)
    - [Prefira chamadas funcionais a processuais](#Prefira-chamadas-funcionais-a-processuais)
    - [Omita RECEIVING](#Omita-RECEIVING)
    - [Omita EXPORTING](#Omita-EXPORTING)
    - [Omita o nome do parâmetro em chamadas de parâmetro único](#Omitir-o-nome-do-parâmetro-em-chamadas-de-parâmetro-único)
    - [Omita a auto-referência ME](#Omita-a-auto-referência-ME)

  - [Orientação a objetos](#Orientação-a-objetos)
    - [Prefira instância a métodos estáticos](#Prefira-instância-a-métodos-estáticos)
    - [Métodos de instância pública](#Métodos-de-instância-pública)

  - [Número do parâmetro](#Número-do-parâmetro)
    - [Parâmetros de IMPORTING](#Parâmetros-de-IMPORTING)
    - [Divida métodos](#Divida-métodos)
    - [Use o PREFERRED PARAMETER com moderação](#Use-o-PREFERRED-PARAMETER-com-moderação)
    - [RETURN, EXPORT ou CHANGE](#RETURN,-EXPORT-ou-CHANGE)

  - [Tipos de Parâmetros](#Tipos-de-Parâmetros)
    - [Prefira RETURNING a EXPORTING](#Prefira-RETURNING-a-EXPORTING)
    - [RETURNING em tabelas grandes](#RETURNING-em-tabelas-grandes)
    - [Use RETURNING ou EXPORTING ou CHANGING](#Use-RETURNING-ou-EXPORTING-ou-CHANGING)
    - [Use CHANGING quando adequado](#Use-CHANGING-quando-adequado)
    - [Divisão de métodos ao em vez de parâmetro de entrada booleano](#Divisão-de-métodos-ao-em-vez-de-parâmetro-de-entrada-booleano)
    - [Parâmetros do tipo TYPE REF TO DATA](#Parâmetros-do-tipo-TYPE-REF-TO-DATA)

  - [Nomes de Parâmetros](#Nomes-de-Parâmetros)
    - [Parâmetro RETURNING RESULT](#Parâmetro-RETURNING-RESULT)
  - [Inicialização de Parâmetros](#Inicialização-de-Parâmetros)
    - [Parâmetros de referência EXPORTING](#Parâmetro-de-referência-EXPORTING)
      - [Tome cuidado!](#Tome-cuidado!)
    - [Não limpe parâmetros VALUE](#Não-limpe-parâmetros-VALUE)
  - [Corpo do método](#Corpo-do-método)
    - [Regras](#Regras)
      - [Características](#Características)
      - [Método deve seguir apenas um propósito](#Método-deve-seguir-apenas-um-propósito)
      - [Desça um nível de Abstração](#Desça-um-nível-de-abstração)
      - [Mantenha-os-métodos-pequenos](#Mantenha-os-métodos-pequenos)
  - [Controle de fluxo](#Controle-de-fluxo)
    - [Valide mais cedo](#Valide-mais-cedo)
    - [CHECK vs. RETURN](#CHECK-vs.-RETURN)
    - [Evite o CHECK em outras posições](#Evite-o-CHECK-em-outras-posições)
  - [Retorno](#Retorno)
    - [Passando retorno de um método para outro](#Passando-retorno-de-um-método-para-outro)
    - [Utilize declarações in-line nos parâmetros de retorno](#Utilize-declarações-in-line-nos-parâmetros-de-retorno)



## Diretrizes

Para um código limpo teve seguir as seguintes especificações sendo as: 

1.  Não deve haver mistura de diferentes formas de programar, deve seguir apenas as citadas nesse documento.
2.  Deve-se usar nomes descritivos e de fácil entendimento aos objetos declarados.
3.  Os nomes devem ser pronunciáveis.
4.  Todos os objetos ao ser declarado deve seguir a Nomenclatura da Infinitfy, salvo quando o cliente possui uma.
5.  Não utilizar nomes já definidos pela SAP.
6.  Todo WHERE do SELECT deve possui @ na variável para diferenciar do campo do Banco de Dados.
7.  As declarações devem ser declaradas IN-LINE, salvo quando necessário.
8.  Deve se evitar ao máximo usar HARD CODE.
9.  Deve ser utilizado condições 



## Constantes

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Constantes)

#### Estruturas Constantes

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Constantes](#Constantes) > [Agrupamento](#Agrupamento) >[Seção atual](#Estruturas-Constantes)

Utilize estruturas constantes ao invés de constantes soltas.

```ABAP
CONSTANTS:
  BEGIN OF message_severity,
    lc_warning TYPE symsgty VALUE 'W',
    lc_error   TYPE symsgty VALUE 'E',
  END OF message_severity.
```

## Variáveis

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Variáveis)

## Declarações IN-LINE

### Variáveis 

```ABAP
DATA(lv_text) = 'Marcos'
```

### Loop At

```ABAP
DATA: lt_flight TYPE TABLE OF sflight.

LOOP AT lt_flight INTO DATA(ls_flight).
ENDLOOP.
```

### Read Table

```ABAP
READ TABLE lt_flight INTO DATA(ls_flight) index 1.
```

### Métodos

```ABAP
DATA(lv_node) = wd_context->get_child_node( name = wd_this->wdctx_node_detail ).
DATA(lv_element) = lv_node->get_element( ).
lv_element->get_attritute( EXPORTING name = 'DETAIL_TEXT'
						   IMPORTING value = DATA(lv_detail_text) ).
```

### Field-Symbol

```ABAP
READ TABLE lt_flight ASSINGNING FIELD-SYMBOL(<fs_string>) INDEX 1.
```

```ABAP
LOOP AT lt_flight ASSINGNING FIELD-SYMBOL(<fs_flight>).
ENDLOOP.
```

### Inserir Dados

```ABAP
TYPES ty_range TYPE RANGE OF string.

DATA(lt_dados) = VALUE ty_range ( ( sing = 'I' opition = 'EQ' low = 'Gabriel' ) 
								  ( sing = 'I' opition = 'EQ' low = 'Vitor' ) )
```

### Ler Índice 

```ABAP
DATA(lv_connid) = lt_sflight[ 1 ]-connid.
```

### Concatenar

````ABAP
DATA(lv_string) = 'Marcio' && ` é ` && 'Muito Legal'
````

### Conversão de dados

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) > [Seção atual](#Conversão-de-dados)

Para conversão de um tipo de dado para outro utilize:

```ABAP
DATA(lv_number) = CONV i( '1241' ).
```

## Selects

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Selects)

Caso o cliente utilize o HANA, fazer a parte de seleção e processamento de dados sempre na camada do banco de dados, utilizando sempre que possível CDS Views.

### Minimizando a criação de TYPES

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Selects](#Selects) > [Seção atual](#Minimizando-a-criação-de-TYPES)

Minimizar a criação de `TYPES` e tabelas internas utilizando declaração in-line dentro de `SELECT’s`:

```ABAP
SELECT matnr, mtype, maktx FROM zcdsv_material INTO TABLE @DATA(gt_material).
```

Ao invés de:

```ABAP
TYPES: BEGIN OF ty_material,
         matnr TYPE zcdsv_material-matnr,
         mtype TYPE zcdsv_material-mtype,
         maktx TYPE zcdsv_material-maktx,
       END OF ty_material.

DATA gt_material type table of ty_material.
SELECT matnr mtype maktx FROM zcdsv_material INTO TABLE gt_material.
```

Dessa forma, não é necessário definir um TYPE para receber os dados e evita a necessidade realizar modificações na estrutura caso haja alteração nos campos de seleção.

### Retornando valores dinâmicos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Selects](#Selects) > [Seção atual](#Retornando-valores-dinâmicos)

Para mudar dinamicamente o valor de um determinado campo retornado por um `SELECT` utilize:

```ABAP
SELECT cust_nbr, first_name, last_name,
      CASE
          WHEN cnpj IS NOT NULL THEN cnpj
          WHEN cpf IS NOT NULL THEN cpf
          ELSE '0'
      END AS registration_num
      FROM ycustomer
      WHERE cust_nbr IN @so_kunnr
      INTO TABLE @DATA(gt_customer).
```

### Realizando cálculos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Selects](#Selects) > [Seção atual](#Realizando-cálculos)

Para realizar operações aritméticas dentro do `SELECT` utilize: 

```ABAP
SELECT matnr AS material_number,
      maktx AS material_description,
      CAST( price AS CURR( 15,2 ) ) * @lv_percentual AS price
      FROM zcdsv_material
      WHERE matnr IN @so_matnr
      INTO TABLE @DATA(gt_saida).
```

Ao invés de:

```ABAP
SELECT matnr AS material_number,
      maktx AS material_description,
      price
      FROM zcdsv_material
      WHERE matnr IN @so_matnr
      INTO TABLE @DATA(gt_saida).

LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<lfs_saida>).
  <lfs_saida>-price = <lfs_saida>-price * lv_percentual.
ENDLOOP.
```

### Agregações

Para o uso das agregações `SUM` `MAX` `MIN` `COUNT` `AVG` sendo:

- SUM = Realiza a soma do conteúdo da coluna, podendo ser usado apenas para campos númerico.
- MAX = Determina o valor máximo do conteúdo da coluna.
- MIN = Determina o valor mínimo do conteúdo da coluna.
- COUNT = Determina o número de valores diferentes na coluna.
- AVG = Determina o valor médio da coluna especificada, podendo ser aplicado apenas em campos numérico.

```ABAP
SELECT AVG( netwr ) FROM vbap INTO DATA(lv_avg_valoritem) WHERE vbeln EQ p_vbeln.

SELECT MAX( kwmeng ) FROM vbap INTO DATA(lv_maxvendida) WHERE vbeln EQ p_vbeln.

SELECT MIN( kwmeng ) FROM vbap INTO DATA(lv_minvendida) WHERE vbeln EQ p_vbeln.

SELECT SUM( kwmeng ) FROM vbap INTO DATA(lv_totalvendida) WHERE vbeln EQ p_vbeln.

SELECT COUNT( matnr ) FROM vbap INTO DATA(lv_qntmaterial) WHERE vbeln EQ p_vbeln.

SELECT COUNT( * ) FROM vbap INTO DATA(lv_qnttotal) WHERE vbeln EQ p_vbeln.
```

## Tabelas

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Tabelas)

### Utilize o tipo de tabela correto

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Utilize-o-tipo-de-tabela-correto)

#### Hashed Table

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Utilize o tipo de tabela correto](#Utilize-o-tipo-de-tabela-correto) > [Seção atual](#Hashed-Table)

**Quando utilizar:**

- Tabelas com grande volume de dados
- Preenchidas em uma única etapa
- Nunca modificadas
- Lidas frequentemente por sua chave

Cada mudança no conteúdo da tabela requer recálculo do hash, portanto, o tipo `HASHED` não é indicado  para tabelas que são modificadas com muita frequência.

#### Sorted Table

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Utilize o tipo de tabela correto](#Utilize-o-tipo-de-tabela-correto) > [Seção atual](#Sorted-Table)

**Quando utilizar:**

- Tabelas com grande volume de dados
- Precisam ser classificadas o tempo todo
- Preenchidas pouco a pouco
- Modificadas frequentemente
- Lidas frequentemente por uma ou mais chaves completas ou parciais
- Processadas em uma determinada ordem

Adicionar, alterar ou remover conteúdo requer encontrar o ponto de inserção correto, mas não requer ajustar o restante do índice da tabela. O tipo `SORTED` demonstra seu valor apenas tabelas com grandes números de acessos de leitura.

#### Standard Table

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Utilize o tipo de tabela correto](#Utilize-o-tipo-de-tabela-correto) > [Seção atual](#Standard-Table)

**Quando utilizar:**

- Tabelas com pequeno volume de dados
- Ordem dos dados não é importante ou deseja processá-las na ordem em que foram anexadas

Além disso, se for necessário um acesso diferente à tabela, por exemplo, acesso indexado e acesso classificado via `SORT`e `BINARY SEARCH`.

### Chaves

>[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Chaves)

Especifique as chaves da sua tabela interna com `WITH NON-UNIQUE KEY`:

```ABAP
DATA lt_itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

Torna-se muito mais visível identificar quais as chaves existentes em uma tabela, do que se fosse utilizado `WITH DEFAULT KEY`:

```ABAP
" Fora do padrão
DATA lt_itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

Caso a tabela não precise de chaves, utilize `EMPTY KEY`:

```ABAP
DATA lt_itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

### Inserção de dados

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Inserção-de-dados)

Para inserir novas entradas em uma tabela, o comando `INSERT INTO TABLE` é o ideal, pois funciona com todos os tipos de tabela e chave, tornando mais fácil para você refatorar o tipo da tabela e as definições de chave se seus requisitos de desempenho mudarem.

```ABAP
INSERT VALUE #( ... ) INTO TABLE lt_itab.
```

Use `APPEND TO`apenas se você usar uma  tabela `STANDARD` de maneira semelhante a uma matriz e se desejar enfatizar que a entrada adicionada deve ser a última linha.

#### Preenchendo uma tabela durante a sua criação

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Inserção de dados](#Inserção-de-dados) > [Seção atual](#Preenchendo-uma-tabela-durante-a-sua-criação)

Caso haja a necessidade de preencher uma tabela durante a sua criação, utilize:

```ABAP
DATA(lt_material_saida) = VALUE ls_material_saida(
      "Primeira linha
      ( matnr = |{ CONV matnr('1') ALPHA = IN }| mtype = 'A' maktx = 'Material test' )
      "Segunda linha
      ( matnr = |{ CONV matnr('2') ALPHA = IN }| mtype = 'B' maktx = 'Material test 2' )
).
```

### Buscando linhas

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Buscando-linhas)

#### Por Índice

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Buscando linhas](#Buscando-linhas) > [Seção atual](#Por-Índice)

Utilize ` lt_my_table[ lv_index ]` para acessar uma linha pelo seu índice:

```ABAP
DATA(wa_material) = lt_material[ 70 ].
```

expressa a intenção de forma mais clara e mais curta do que:

```ABAP
READ TABLE lt_material INDEX 70 INTO DATA(wa_material).
```

#### Por valor de um ou mais campos específicos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Buscando linhas](#Buscando-linhas) > [Seção atual](#Por-valor-de-um-ou-mais-campos-específicos)

Utilize `lt_my_table[ campo1 = lv_valor campo2 = lv_valor2 ]` para buscar uma linha com base no valor de um ou mais campos:

```ABAP
DATA(wa_material) = lt_material[ matnr = lv_matnr mtype = lv_mtype ].
```

expressa a intenção de forma mais clara e mais curta do que:

```ABAP
READ TABLE lt_material INTO DATA(wa_material)  WITH KEY matnr = lv_matnr mtype = lv_mtype.
```

Porém, em casos em que a tabela a ser lida contenha um enorme volume de dados é recomendado o uso de `READ TABLE` e `BINARY SEARCH`. 

```ABAP
SORT lt_material BY matnr mtype.
READ TABLE lt_material INTO DATA(wa_material)  WITH KEY matnr = lv_matnr mtype = lv_mtype BINARY SEARCH.
```

**OBS:** Lembre-se de que é necessário utilizar o `SORT` na tabela a ser lida antes de aplicar o `READ TABLE`.

### Retornando o índice de uma linha

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Retornando-o-índice-de-uma-linha)

Utilize line_index para retornar o índice de uma linha:

```ABAP
DATA(lv_tabix) = line_index( gt_material[ matnr = lv_material_number ] ).
```

torna-se muito mais claro e mais curto do que:

```ABAP
READ TABLE gt_material WITH KEY matnr = lv_material_number TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  DATA(lv_tabix) = sy-tabix.
ENDIF.
```

### Acessando campos de uma linha diretamente

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Acessando-campos-de-uma-linha-diretamente)

É possível acessar diretamente qualquer campo da linha buscada sem a necessidade de definir uma Workarea para isso:

```ABAP
DATA(lv_maktx) = lt_material[ 70 ]-maktx.
```

expressa a intenção de forma mais clara e mais curta do que:

```ABAP
READ TABLE lt_material INDEX 70 INTO DATA(wa_material).
DATA(lv_maktx) = wa_material-maktx.
```

### Passando dados de uma tabela para outra

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Passando-dados-de-uma-tabela-para-outra)

#### Copiando uma tabela

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Passando dados de uma tabela para outra](#Passando-dados-de-uma-tabela-para-outra) > [Seção atual](#Copiando-uma-tabela)

Para gerar uma cópia de uma tabela utilize:

```ABAP
DATA(lt_material_aux) = lt_material[].
```

#### Adicionando linhas a uma tabela existente

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Passando dados de uma tabela para outra](#Passando-dados-de-uma-tabela-para-outra) > [Seção atual](#Adicionando-linhas-a-uma-tabela-existente)

Para adicionar linhas a uma tabela existente e preservar seu conteúdo utilize:

```ABAP
lt_material_saida = VALUE #( BASE lt_material_saida ( LINES OF lt_material_aux ) ).
```

#### Tabelas de tipos diferentes

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Passando dados de uma tabela para outra](#Passando-dados-de-uma-tabela-para-outra) > [Seção atual](#Tabelas-de-tipos-diferentes)

Para passar dados de uma tabela para outra de tipo diferente utilize:

```ABAP
DATA(lt_material_saida) = VALUE ls_material_saida( FOR lw_material IN lt_material (
      matnr = lw_material-matnr
      mtype = lw_material-mtype
      maktx = lw_material-maktx
      ) ).
```

#### Filtrando dados

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Passando dados de uma tabela para outra](#Passando-dados-de-uma-tabela-para-outra) > [Seção atual](#Filtrando-dados)

Para filtrar quais linhas serão transferidas utilize:

```ABAP
DATA(lt_material_saida) = VALUE ls_material_saida( FOR lw_material IN lt_material 
WHERE ( matnr >= lv_matnr and mtype = 'B' )
      (
      matnr = lw_material-matnr
      mtype = lw_material-mtype
      maktx = lw_material-maktx
      ) ).
```

### Utilizando LOOP AT

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Utilizando-LOOP-AT)

#### Quando utilizar

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Utilizando LOOP AT](#Utilizando-LOOP-AT) > [Seção atual](#Quando-utilizar)

Utilize `LOOP AT` somente em casos que necessitem que seja realizado algum processamento mais complexo para cada iteração de uma tabela interna.

```ABAP
IF p_path IS NOT INITIAL.
  OPEN DATASET gv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  LOOP AT lt_relatorio_txt ASSIGNING FIELD-SYMBOL(<lfs_relatorio_txt>).
    DATA(lv_linha) = COND ty_relatorio_txt-linha(
                           WHEN <lfs_relatorio_txt>-linha IS NOT INITIAL THEN lfs_relatorio_txt>-linha
                           ELSE 'Linha vazia' ).
    TRANSFER lv_linha TO gv_path.
  ENDLOOP.
  CLOSE DATASET gv_path.
ENDIF.
```

#### Utilize FIELD SYMBOL ao invés de MODIFY

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Utilizando LOOP AT](#Utilizando-LOOP-AT) > [Seção atual](#Utilize-FIELD-SYMBOL-ao-invés-de-MODIFY)

Em situações em que é necessário fazer alteração de uma tabela utilizando `LOOP AT` , utilize:

 ```abap
LOOP AT gt_material ASSIGNING FIELD-SYMBOL(<lfs_material>).
  <lfs_material>-indesc = 'BENS DE CONSUMO'.
  <lfs_material>-mtgpdesc = 'SMARTPHONES'.
ENDLOOP. 
 ```

Ao invés de:

```ABAP
LOOP AT gt_material INTO DATA(lw_material).
  lw_material-indesc = 'BENS DE CONSUMO'.
  lw_material-mtgpdesc = 'SMARTPHONES'.

  MODIFY gt_material FROM lw_material.
ENDLOOP.
```

### Verificando a existência de uma linha

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Verificando-a-existência-de-uma-linha)

Utilize LINE_EXISTS ao invés de READ TABLE ou LOOP AT:

```ABAP
IF line_exists( lt_my_table[ key = 'A' ] ).
```

expressa a intenção de forma mais clara e mais curta do que:

```ABAP
" Fora do padrão
READ TABLE lt_my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

ou mesmo:

```ABAP
" Fora do padrão
LOOP AT lt_my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### Evite leituras desnecessárias de tabelas

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Evite-leituras-desnecessárias-de-tabelas)

Utilize `TRY CATCH` para lidar com uma possível linha inexistente e reaja à exceção:

```ABAP
TRY.
    DATA(row) = gt_material[ matnr = lv_material_number ].
  CATCH cx_sy_itab_line_not_found.
    WRITE 'Linha não encontrada'.
ENDTRY.
```

em vez de realizar uma leitura dupla desnecessária:

```ABAP
IF NOT line_exists( gt_material[ matnr = lv_material_number ] ).
  WRITE 'Linha não encontrada'.
ELSE.
  DATA(row) = gt_material[ matnr = lv_material_number ].
ENDIF.
```

### CORRESPONDING 

Utilize `CORRESPONDING` para copiar uma coluna de uma tabela em outra tabela que possui o mesmo nome da coluna.

````ABAP
lt_itab1 = VALUE #( ( col1 = 'A' col2 = 'B' )
                 ( col1 = 'P' col2 = 'Q' )
                 ( col1 = 'N' col2 = 'P' )
               ).
lt_itab2 = CORRESPONDING #( lt_itab1 ).
````

Caso ambas tabelas possuem colunas iguais e é necessário passar somente uma use `EXCEPT` para essa exceção.

````ABAP
lt_itab1 = VALUE #( ( col1 = 'A' col2 = 'B' )
                 ( col1 = 'P' col2 = 'Q' )
                 ( col1 = 'N' col2 = 'P' )
               ).
lt_itab2 = CORRESPONDING #( itab1 EXCEPT COL2 ).
````

Caso seja necessário passar o conteúdo de uma coluna para outra cujo nome seja diferente pode usar-se `MAPPING` para fazer o redirecionamento dela.

````ABAP
lt_itab1 = VALUE #( ( col1 = 'A' col2 = 'B' )
                 ( col1 = 'P' col2 = 'Q' )
                 ( col1 = 'N' col2 = 'P' )
               ).
lt_itab2 = CORRESPONDING #( lt_itab1 MAPPING COL3 = COL2 EXCEPT COL2 ).
````

## Strings

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Strings)

### Definindo textos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Strings](#Strings) > [Seção atual](#Definindo-textos)

Utilize` para definir literais:

```ABAP
CONSTANTS gc_constant TYPE string VALUE `ABC`.
DATA(gc_string) = `ABC`.  " --> TYPE string
```

### Concatenação

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Strings](#Strings) > [Seção atual](#Concatenação)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

Os modelos de string destacam melhor o que é literal e o que é variável, especialmente se você incorporar várias variáveis em um texto.

```ABAP
" Fora do padrão
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

### Adicionando zeros à esquerda

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Strings](#Strings) > [Seção atual](#Adicionando-zeros-à-esquerda)

Utilize:

```ABAP
DATA var_ext TYPE char10 VALUE '12345'.
DATA(var_int) = |{ var_ext ALPHA = IN }|.
```

Ao invés de:

```ABAP
DATA var_ext TYPE char10 VALUE '12345'.
DATA var_int TYPE char10.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' 
  EXPORTING
    input  = var_ext
  IMPORTING
    output = var_int.
```

### Removendo zeros à esquerda

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Strings](#Strings) > [Seção atual](#Removendo-zeros-à-esquerda)

Utilize:

```ABAP
DATA var_int TYPE char10 VALUE '0000098765'.
DATA(var_ext) = |{ var_int ALPHA = OUT }|.
```

Ao invés de:

```ABAP
DATA var_int TYPE char10 VALUE '0000098765'.
DATA var_ext TYPE char10.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT' 
  EXPORTING
    input  = var_int
  IMPORTING
    output = var_ext.  
```

### Largura, Alinhamento e Preenchimento

- Width = Determina a largura do texto.
- Align = Alinha o texto, para esquerda `LEFT` , centro `CENTER` e direita `RIGHT`.
- PAD = Preenche todo o espaço que sobrou do `Width`.

```ABAP
WRITE / |{ 'Left'     WIDTH = 20 ALIGN = LEFT   PAD = '0' }|.
WRITE / |{ 'Centre'   WIDTH = 20 ALIGN = CENTER PAD = '0' }|.
WRITE / |{ 'Right'    WIDTH = 20 ALIGN = RIGHT  PAD = '0' }|.
```

### CASE

Para formatar o campo texto utilizar `CASE` para usar a classe `cl_abap_format` :

- c_raw = O texto permanece inalterado.
- c_upper = O texto fica em caixa alta.
- c_lower = O texto fica em caixa baixa.

```abap
WRITE / |{ 'Text' CASE = (cl_abap_format=>c_raw) }|.
WRITE / |{ 'Text' CASE = (cl_abap_format=>c_upper) }|.
WRITE / |{ 'Text' CASE = (cl_abap_format=>c_lower) }|.
```



## Condições

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Condições)

### Tente tornar as condições positivas

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Condições](#Condições) > [Seção atual](#Tente-tornar-as-condições-positivas)

```ABAP
IF has_entries = abap_true.
```

Para comparação, veja como fica difícil entender a mesma declaração ao invertê-la:

```ABAP
" Fora do padrão
IF has_no_entries = abap_false.
```

A "tentativa" no título da seção significa que você não deve forçar isso a ponto de acabar com algo como ramificações IF vazias :

```ABAP
" Fora do padrão
IF has_entries = abap_true.
ELSE.
  " apenas faça algo no bloco ELSE, IF permanece vazio
ENDIF.
```

### Prefira IS NOT do que NOT IS

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Condições](#Condições) > [Seção atual](#Prefira-IS-NOT-do-que-NOT-IS)

```ABAP
IF variable IS NOT INITIAL.
IF variable NP 'TODO*'.
IF variable <> 42.
```

A negação é logicamente equivalente, mas requer uma "reviravolta mental" que a torna mais difícil de entender.

```ABAP
" Fora do padrão
IF NOT variable IS INITIAL.
IF NOT variable CP 'TODO*'.
IF NOT variable = 42.
```



## Estruturas Condicionais

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Estruturas-Condicionais)

### IF ELSE

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Estruturas-Condicionais) > [IF ELSE](#IF-ELSE)

Utilize `IF-ELSE` para comparações complexas, como comparar dois ou mais campos com múltiplos valores e operadores lógicos:

```ABAP
TRY.
    DATA(lw_material) = gt_material[ matnr = lv_material_number ].
    IF lw_material-mtype = 'I' AND lw_material-status <> 'I'.
      WRITE |Material: { lv_material_number } está ativo em PRD.|.
    ELSE.
      WRITE |Material: { lv_material_number } não está ativo em PRD.|.
      perform f_ativar_material using lv_material_number.
    ENDIF.
  CATCH cx_sy_itab_line_not_found.
    WRITE 'Linha não encontrada'.
ENDTRY.
```

### CASE

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Estruturas Condicionais](#Estruturas-Condicionais) > [Seção atual](#CASE)

Utilize `CASE` para comparações simples, de um campo com múltiplos valores:

```ABAP
CASE mtype.
  WHEN 'A'.
    " ...
  WHEN 'B'.
    " ...
  WHEN OTHERS.
    " ...
ENDCASE.
```

Ao invés de:

```ABAP
" Fora do padrão
IF mtype = 'A'.
  " ...
ELSEIF mtype = 'B'.
  " ...
ELSE.
  " ...
ENDIF.
```

### SWITCH

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Estruturas Condicionais](#Estruturas-Condicionais) > [Seção atual](#SWITCH)

Utilize `SWITH` em atribuições condicionais comparando um único campo com múltiplos valores:

```ABAP
lv_ambiente = SWITCH #( syst-sysid
  WHEN 'D01' THEN 'DEV'
  WHEN 'Q01' THEN 'QAS'
  WHEN 'P01' THEN 'PRD'
  ELSE 'Desconhecido'
).
```

Torna-se muito mais claro e mais curto do que:

```ABAP
CASE syst-sysid.
  WHEN 'D01'.
    lv_ambiente = 'DEV'.
  WHEN 'Q01'.
    lv_ambiente = 'QAS'.
  WHEN 'P01'.
    lv_ambiente = 'PRD'.
  WHEN OTHERS.
    lv_ambiente = 'Desconhecido'.
ENDCASE.
```

### COND

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Estruturas Condicionais](#Estruturas-Condicionais) > [Seção atual](#COND)

Utilize `COND` em atribuições condicionais comparando dois ou mais campos com múltiplos valores e operadores lógicos:

```ABAP
gv_erro = COND #( WHEN lw_material-mtype <> 'A' AND lw_material-price IS NOT INITIAL THEN abap_false ELSE abap_true ).
```

Torna-se muito mais claro e curto do que:

```ABAP
IF lw_material-mtype <> 'A' AND lw_material-price IS NOT INITIAL.
  gv_erro = abap_false.
ELSE.
  gv_erro = abap_true.
ENDIF.
```

### Retorno de funções e métodos em expressões lógicas

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Estruturas Condicionais](#Estruturas-Condicionais) > [Seção atual](#Retorno-de-funções-e-métodos-em-expressões-lógicas)

Não crie variáveis auxiliares para armazenar o retorno de funções e métodos a fim de utilizá-lo em uma expressão lógica, use-os diretamente:

```ABAP
IF STRLEN( lv_string ) > 10 THEN….
```

Ao invés de:

```ABAP
lv_helper = STRLEN( lv_string ).
IF lv_helper > 10 THEN…..
```

#### Omitindo ABAP_TRUE

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Estruturas Condicionais](#Estruturas-Condicionais) > [Retorno de funções e métodos em expressões lógicas](#Retorno-de-funções-e-métodos-em-expressões-lógicas) > [Seção atual](#Omitindo-ABAP_TRUE)

Omita comparações desnecessárias ao utilizar métodos e funções em expressões lógicas:

```ABAP
IF line_exists( lt_my_table[ key = 'A' ] ).
```

Ao invés de:

```ABAP
IF line_exists( lt_my_table[ key = 'A' ] ) = abap_true.
```

**OBS:** Esta regra aplica-se somente a parâmetros de retorno do tipo `ABAP_BOOL`.



## Classes

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Classes)

### Instanciando objetos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Classes](#Classes) > [Seção atual](#Instanciando-objetos)

Para criar novas instâncias de um objeto utilize:

```ABAP
" Caso o objeto já tenha sido declarado
DATA: lcl_material_master TYPE REF TO zcl_material_master.
lcl_material_master = NEW #( lv_matnr ).

" Caso o objeto seja declarado in-line
DATA(lcl_material_master) = NEW zcl_material_master( lv_matnr ).
```

Torna-se muito mais claro e mais curto do que:

```ABAP
DATA lcl_material_master TYPE REF TO zcl_material_master.
CREATE OBJECT lcl_material_master
  EXPORTING
    p_material_number = lv_matnr.
```

#### Tipo dinâmico

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Classes](#Classes) > [Instanciando objetos](#Instanciando-objetos) > [Seção atual](#Tipo-dinâmico)

Caso seja necessário instanciar um objeto com tipo dinâmico, utilize:

```ABAP
CREATE OBJECT lcl_order TYPE (dynamic_type)
  EXPORTING
    order_number = lv_docnum.
```



## Métodos

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Métodos)

### Chamadas

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Seção atual](#Chamadas)

#### Chamada de métodos estáticos 

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Chamadas](#Chamadas) > [Seção atual](#Chamada-de-métodos-estáticos)

Para chamar um método estático, use

```ABAP
cl_my_class=>static_method( ).
```

Em vez de chamá-lo por meio de uma variável de instância

```ABAP
" Fora do padrão
lo_my_instance->static_method( ).
```

Um método estático é anexado à própria classe e chamá-lo por meio de uma variável de instância é uma possível fonte de confusão.

Não há problema em chamar um método estático da mesma classe sem qualificá-lo em outro método estático.

```ABAP
METHOD static_method.
  another_static_method( ).
  yet_another( ).
ENDMETHOD.
```

No entanto, dentro de um método de instância, mesmo ao chamar um método estático da mesma classe, você ainda deve qualificar a chamada com o nome da classe:

```ABAP
CLASS cl_my_class IMPLEMENTATION.

  METHOD instance_method.
    cl_my_class=>a_static_method( ).
    another_instance_method( ).
  ENDMETHOD.

  ...
```

#### Prefira chamadas funcionais a processuais

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Chamadas](#Chamadas) > [Seção atual](#Prefira-chamadas-funcionais-a-processuais)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

em vez do desnecessariamente mais longo

```ABAP
" Fora do padrão
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Se a digitação dinâmica proibir chamadas funcionais, recorra ao estilo procedural

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Muitas das regras detalhadas abaixo são apenas variações mais específicas deste conselho.

#### Omita RECEIVING

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Chamadas](#Chamadas) > [Seção atual](#Omita-RECEIVING)

```ABAP
DATA(sum) = aggregate_values( values ).
```

em vez do desnecessariamente mais longo

```ABAP
" Fora do padrão
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### Omita EXPORTING

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Chamadas](#Chamadas) > [Seção atual](#Omita-EXPORTING)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

em vez do mais longo

```ABAP
" Fora do padrão
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### Omita o nome do parâmetro em chamadas de parâmetro único

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Chamadas](#Chamadas) > [Seção atual](#Omita-o-nome-do-parâmetro-em-chamadas-de-parâmetro-único)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

em vez do mais longo

```ABAP
" Fora do padrão
DATA(unique_list) = remove_duplicates( list = list ).
```

Há casos, no entanto, em que o nome do método sozinho não é claro o suficiente e repetir o nome do parâmetro pode facilitar a compreensão:

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### Omita a auto-referência ME

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Chamadas](#Chamadas) > [Seção atual](#Omita-a-auto-referência-ME)

Como a auto-referência `me->`é definida implicitamente pelo sistema, omita-a ao chamar um atributo ou método de instância

```ABAP
DATA(sum) = aggregate_values( values ).
```

em vez do desnecessariamente mais longo

```
" Fora do padrão
DATA(sum) = aggregate_values( me->values ).
" Fora do padrão
DATA(sum) = me->aggregate_values( values ).
```

a menos que haja um conflito de escopo entre uma variável local ou parâmetro de importação e um atributo de instância

```ABAP
me->logger = logger.
```

### Orientação a objetos

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) >  [Seção atual](#Orientação-a-objetos)

#### Prefira instância a métodos estáticos

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Orientação a objetos ](#Orientação-a-objetos) > [Seção atual](#Prefira-instância-a-métodos-estáticos)

Os métodos devem ser membros de instância por padrão. Os métodos de instância refletem melhor o "objeto" da classe. Eles podem ser ridicularizados mais facilmente em testes de unidade.

```ABAP
METHODS publish.
```

Os métodos devem ser estáticos apenas em casos excepcionais, como métodos de criação estáticos.

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### Métodos de instância pública

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Orientação a objetos ](#Orientação-a-objetos) > [Seção atual](#Métodos-de-instância-pública)

Os métodos de instância pública sempre devem fazer parte de uma interface.

```ABAP
METHOD /clean/blog_post~publish.
```

Na orientação a objetos limpa, ter um método público sem uma interface não faz muito sentido - com poucas exceções, como classes de enumeração que nunca terão uma implementação alternativa e nunca serão ridicularizadas em casos de teste.

### Número do parâmetro

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) >  [Seção atual](#Número-do-parâmetro)

#### Parâmetros de IMPORTING

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Número do parâmetro](#Número-do-parâmetro) > [Seção atual](#Parâmetros-de-IMPORTING)

É ideal a utilização de no máximo três parâmetro de IMPORTING.

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

é muito mais claro do que

```ABAP
" Fora do padrão
FUNCTION seo_class_copy
  IMPORTING
    clskey                 TYPE seoclskey
    new_clskey             TYPE seoclskey
    access_permission      TYPE seox_boolean DEFAULT seox_true
    VALUE(save)            TYPE seox_boolean DEFAULT seox_true
    VALUE(suppress_corr)   TYPE seox_boolean DEFAULT seox_false
    VALUE(suppress_dialog) TYPE seox_boolean DEFAULT seox_false
    VALUE(authority_check) TYPE seox_boolean DEFAULT seox_true
    lifecycle_manager      TYPE REF TO if_adt_lifecycle_manager OPTIONAL
    lock_handle            TYPE REF TO if_adt_lock_handle OPTIONAL
    VALUE(suppress_commit) TYPE seox_boolean DEFAULT seox_false
  EXPORTING
    ...
```

Muitos parâmetros de entrada deixam a complexidade de um método explodir porque ele precisa lidar com um número exponencial de combinações. Muitos parâmetros são um indicador de que o método pode fazer mais de uma coisa.

Você pode reduzir o número de parâmetros combinando-os em conjuntos significativos com estruturas e objetos.

#### Divida métodos

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Número do parâmetro](#Número-do-parâmetro) > [Seção atual](#Divida-métodos)

Divida os métodos em vez de adicionar parâmetros OPCIONAIS.

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

para obter a semântica desejada, pois o ABAP não suporta [sobrecarga](https://en.wikipedia.org/wiki/Function_overloading) .

```ABAP
" Fora do padrão
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string OPTIONAL
    something_else TYPE i OPTIONAL.
```

Parâmetros opcionais confundem os chamadores:

- Quais são realmente necessários?
- Quais combinações são válidas?
- Quais se excluem mutuamente?

Vários métodos com parâmetros específicos para o caso de uso evitam essa confusão, fornecendo orientações claras sobre quais combinações de parâmetros são válidas e esperadas.

#### Use o PREFERRED PARAMETER com moderação

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Número do parâmetro](#Número-do-parâmetro) > [Seção atual](#Use-o-PREFERRED-PARAMETER-com-moderação)

A adição `PREFERRED PARAMETER`torna difícil ver qual parâmetro é realmente fornecido, dificultando a compreensão do código. Minimizar o número de parâmetros, especialmente os opcionais, reduz automaticamente a necessidade de arquivos `PREFERRED PARAMETER`.

#### RETURN, EXPORT ou CHANGE 

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Número do parâmetro](#Número-do-parâmetro) > [Seção atual](#RETURN,-EXPORT-ou-CHANGE)

Um bom método faz *uma coisa* , e isso deve ser refletido pelo método também retornando exatamente uma coisa. Se os parâmetros de saída do seu método *não* formarem uma entidade lógica, seu método faz mais de uma coisa e você deve dividi-lo.

Há casos em que a saída é uma entidade lógica que consiste em várias coisas. Estes são mais facilmente representados retornando uma estrutura ou objeto:

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```

em vez de

```ABAP
" Fora do padrão
METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  EXPORTING
    result            TYPE result_type
    failed_keys       TYPE /bobf/t_frw_key
    messages          TYPE /bobf/t_frw_message.
```

Especialmente em comparação com vários parâmetros de EXPORTING, isso permite que as pessoas usem o estilo de chamada funcional, poupa você de pensar sobre `IS SUPPLIED`e evita que as pessoas se esqueçam acidentalmente de recuperar uma informação vital  de `ERROR_OCCURRED`.

Em vez de vários parâmetros de saída opcionais, considere dividir o método de acordo com padrões de chamada significativos:

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE result_type.

METHODS check_and_report
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```

### Tipos de Parâmetros

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) >  [Seção atual](#Tipos-de-Parâmetros)

#### Prefira RETURNING a EXPORTING

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Tipos de Parâmetros](#Tipos-de-Parâmetros) > [Seção atual](#Prefira-RETURNING-a-EXPORTING)

```ABAP
METHODS square
  IMPORTING
    number        TYPE i
  RETURNING
    VALUE(result) TYPE i.

DATA(result) = square( 42 ).
```

Em vez do desnecessariamente mais longo

```ABAP
" Fora do padrão
METHODS square
  IMPORTING
    number TYPE i
  EXPORTING
    result TYPE i.

square(
  EXPORTING
    number = 42
  IMPORTING
    result = DATA(result) ).
```

`RETURNING` não apenas torna a chamada mais curta, mas também permite o encadeamento de métodos e evita erros de mesma entrada e saída.

#### RETURNING em tabelas grandes

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Tipos de Parâmetros](#Tipos-de-Parâmetros) > [Seção atual](#RETURNING-em-tabelas-grandes)

Embora a documentação da linguagem ABAP e os guias de desempenho digam o contrário, raramente encontramos casos em que entregar uma tabela grande ou profundamente aninhada em um parâmetro VALUE *realmente* causa problemas de desempenho. Portanto, recomendamos o uso geral

```ABAP
METHODS get_large_table
  RETURNING
    VALUE(result) TYPE /clean/some_table_type.

METHOD get_large_table.
  result = large_table.
ENDMETHOD.

DATA(lt_my_table) = get_large_table( ).
```

Somente se houver prova real ( = uma medição de desempenho ruim) para o seu caso individual, você deve recorrer ao estilo de procedimento mais complicado

```ABAP
" Fora do padrão
METHODS get_large_table
  EXPORTING
    result TYPE /dirty/some_table_type.

METHOD get_large_table.
  result = large_table.
ENDMETHOD.

get_large_table( IMPORTING result = DATA(lt_my_table) ).
```

#### Use RETURNING ou EXPORTING ou CHANGING

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Tipos de Parâmetros](#Tipos-de-Parâmetros) > [Seção atual](#Use-RETURNING-ou-EXPORTING-ou-CHANGING)

Use RETURNING ou EXPORTING ou CHANGING, mas não uma combinação deles.

```ABAP
METHODS copy_class
  IMPORTING
    old_name      TYPE seoclsname
    new name      TYPE secolsname
  RETURNING
    VALUE(result) TYPE copy_result
  RAISING
    /clean/class_copy_failure.
```

em vez de misturar-los como

```ABAP
" Fora do padrão
METHODS copy_class
  ...
  RETURNING
    VALUE(result)      TYPE vseoclass
  EXPORTING
    error_occurred     TYPE abap_bool
  CHANGING
    correction_request TYPE trkorr
    package            TYPE devclass.
```

Diferentes tipos de parâmetros de saída são um indicador de que o método faz mais de uma coisa. Isso confunde o leitor e torna a chamada do método desnecessariamente complicada.

Uma exceção aceitável a esta regra pode ser construtores que consomem sua entrada enquanto constroem sua saída:

```ABAP
METHODS build_tree
  CHANGING
    tokens        TYPE tokens
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

No entanto, mesmo esses podem ficar mais claros objetivando a entrada:

```ABAP
METHODS build_tree
  IMPORTING
    tokens        TYPE REF TO token_stack
  RETURNING
    VALUE(result) TYPE REF TO tree.
```

#### Use CHANGING quando adequado

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Tipos de Parâmetros](#Tipos-de-Parâmetros) > [Seção atual](#Use-CHANGING-quando-adequado)

`CHANGING`deve ser reservado para casos em que uma variável local existente que já está preenchida é atualizada apenas em alguns lugares:

```ABAP
METHODS update_references
  IMPORTING
    new_reference TYPE /bobf/conf_key
  CHANGING
    bo_nodes      TYPE root_nodes.

METHOD update_references.
  LOOP AT bo_nodes REFERENCE INTO DATA(bo_node).
    bo_node->reference = new_reference.
  ENDLOOP.
ENDMETHOD.
```

Não force seus chamadores a introduzir variáveis locais desnecessárias apenas para fornecer seu parâmetro `CHANGING`. Não use parâmetros `CHANGING` para preencher inicialmente uma variável previamente vazia.

#### Divisão de métodos ao em vez de parâmetro de entrada booleano

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Tipos de Parâmetros](#Tipos-de-Parâmetros) > [Seção atual](#Divisão-de-métodos-ao-em-vez-de-parâmetro-de-entrada-booleano)

Parâmetros de entrada booleanos geralmente são um indicador de que um método faz *duas* coisas em vez de uma.

```ABAP
" Fora do padrão
METHODS update
  IMPORTING
    do_save TYPE abap_bool.
```

Além disso, chamadas de método com um único - e, portanto, sem nome - parâmetro booleano tendem a obscurecer o significado do parâmetro.

```ABAP
" Fora do padrão
update( abap_true ).  " o que significa 'verdadeiro'? 
```

Dividir o método pode simplificar o código dos métodos e descrever melhor as diferentes intenções

```ABAP
update_without_saving( ).
update_and_save( ).
```

A percepção comum sugere que os setters para variáveis booleanas estão bem:

```ABAP
METHODS set_is_deleted
  IMPORTING
    new_value TYPE abap_bool.
```

#### Parâmetros do tipo TYPE REF TO DATA

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Tipos de Parâmetros](#Tipos-de-Parâmetros) > [Seção atual](#Parâmetros-do-tipo-TYPE-REF-TO-DATA)

Utilize `REF #` para preencher parâmetros do tipo `TYPE REF TO DATA` ao invés de `GET REFERENCE OF`.

```ABAP
"VALUE é um parâmetroo IMPORTING de tipo ANY
IF value IS SUPPLIED.
lo_material_log->log_value( REF#( value )).
ENDIF.
```

torna-se muito mais claro e mais curto do que:

```ABAP
DATA: lo_do_value TYPE REF TO DATA.
"VALUE é um parâmetroo IMPORTING de tipo ANY
IF value IS SUPPLIED.
CREATE DATA lo_do_value LIKE value.
GET REFERENCE OF value INTO lo_do_value.
lo_material_log->log_value( ld_do_value ).
ENDIF.
```

### Nomes de Parâmetros

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Nomes-de-Parâmetros)

#### Parâmetro RETURNING RESULT

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Nomes de Parâmetros ](#Nomes-de-Parâmetros) > [Seção atual](#Parâmetro-RETURNING-RESULT)

Bons nomes de método geralmente são tão bons que o parâmetro  `RETURNING` não precisa de um nome próprio. O nome faria pouco mais do que repetir o nome do método ou repetir algo óbvio.

A repetição de um nome de membro pode até produzir conflitos que precisam ser resolvidos com a adição de um exagerado `me->`.

```ABAP
" Fora do padrão
METHODS get_name
  RETURNING
    VALUE(name) TYPE string.

METHOD get_name.
  name = me->name.
ENDMETHOD.
```

Nesses casos, basta chamar o parâmetro `RESULT`, ou algo parecido `RV_RESULT` se preferir a notação húngara.

Nomeie o parâmetro `RETURNING` se não for *óbvio* o que ele representa, por exemplo, em métodos que retornam `me` para encadeamento de métodos ou em métodos que criam algo, mas não retornam a entidade criada, mas apenas sua chave ou algo assim.

### Inicialização de Parâmetros

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Seção atual](#Inicialização-de-Parâmetros)

#### Parâmetros de referência EXPORTING

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Inicialização de Parâmetros](#Inicialização-de-Parâmetros) > [Seção atual](#Parâmetro-de-referência-EXPORTING)

Os parâmetros de referência referem-se a áreas de memória existentes que podem ser preenchidas previamente. Limpe-os ou substitua-os para fornecer dados confiáveis:

```abap
METHODS square
  EXPORTING
    result TYPE i.

" clear
METHOD square.
  CLEAR result.
  " ...
ENDMETHOD.

" Substituir
METHOD square.
  result = cl_abap_math=>square( 2 ).
ENDMETHOD.
```

##### Tome cuidado! 

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Inicialização de Parâmetros](#Inicialização-de-Parâmetros) > [Parâmetros de referência EXPORTING](#Parâmetro-de-referência-EXPORTING) > [Seção atual](#Tome-cuidado!)

Geralmente, é uma boa ideia limpar o parâmetro como primeira coisa no método após as declarações de tipo e dados. Isso torna a instrução fácil de detectar e evita que o valor ainda contido seja acidentalmente usado por instruções posteriores.

No entanto, algumas configurações de parâmetros podem usar a mesma variável como entrada e saída. Nesse caso, um simples `CLEAR` excluiria o valor de entrada antes que ele pudesse ser usado, produzindo resultados incorretos.

```abap
" Fora do padrão
DATA value TYPE i.

square_dirty(
  EXPORTING
    number = value
  IMPORTING
    result = value ).

METHOD square_dirty.
  CLEAR result.
  result = number * number.
ENDMETHOD.
```

Considere redesenhar esses métodos substituindo os `EXPORTING` por `RETURNING`. Considere também sobrescrever o parâmetro `EXPORTING` em uma única instrução de cálculo de resultado. Se nenhum dos dois se encaixar, recorra a um `CLEAR`.

#### Não limpe parâmetros VALUE

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Inicialização de Parâmetros](#Inicialização-de-Parâmetros) > [Seção atual](#Não-limpe-parâmetros-VALUE)

Parâmetros declarados no VALUE são vazias por definição desta forma não precisa ser limpo novamente 

```ABAP
METHODS square
  EXPORTING
    VALUE(result) TYPE i.

METHOD square.
  " Não há necessidade de limpar o resultado
ENDMETHOD.
```

O mesmo se aplica ao RETURNING, pois os parâmetros RETURNING são sempre parâmetro VALUE 

```ABAP
METHODS square
  RETURNING
    VALUE(result) TYPE i.

METHOD square.
  " Não há necessidade de limpar o resultado
ENDMETHOD.
```

### Corpo do método

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Seção atual](#Corpo-do-método)

#### Regras

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Corpo-do-método](#Corpo-do-método) > [Seção atual](#Regras)

##### Características 

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Corpo-do-método](#Corpo-do-método) > [Regras](#Regras) >[Seção atual](#Características)

Um método deve fazer uma coisa, e apenas uma coisa. Deve fazê-lo da melhor maneira possível.

Um método provavelmente faz uma coisa se:

- Tem poucos parâmetros de entrada
- Não inclui parâmetros booleanos
- Tem exatamente um parâmetro de saída
- É pequeno
- Ele desce um nível de abstração
- Ele apresenta apenas um tipo de exceção
- Você não pode extrair outros métodos significativos
- Você não pode agrupar significativamente suas declarações em seções

##### Método deve seguir apenas um propósito

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Corpo-do-método](#Corpo-do-método) > [Regras](#Regras) > [Seção atual](#Método-deve-seguir-apenas-um-propósito)

Um método deve seguir o seu propósito pelo qual foi criado ou o tratamento de erros caso não possa, mas não ambos.

```ABAP
" Fora do padrão
METHOD append_xs.
  IF input > 0.
    DATA(remainder) = input.
    WHILE remainder > 0.
      result = result && `X`.
      remainder = remainder - 1.
    ENDWHILE.
  ELSEIF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSE.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.
```

Pode ser decomposto em

```abap
METHOD append_xs.
  validate( input ).
  DATA(remainder) = input.
  WHILE remainder > 0.
    result = result && `X`.
    remainder = remainder - 1.
  ENDWHILE.
ENDMETHOD.

METHOD validate.
  IF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSEIF input < 0.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.
```

ou, para enfatizar a parte de validação

```ABAP
METHOD append_xs.
  IF input > 0.
    result = append_xs_without_check( input ).
  ELSEIF input = 0.
    RAISE EXCEPTION /dirty/sorry_cant_do( ).
  ELSE.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
ENDMETHOD.

METHOD append_xs_without_check.
  DATA(remainder) = input.
  WHILE remainder > 0.
    result = result && `X`.
    remainder = remainder - 1.
  ENDWHILE.
ENDMETHOD.
```

##### Desça um nível de abstração

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Corpo-do-método](#Corpo-do-método) > [Regras](#Regras) > [Seção atual](#Desça-um-nível-de-abstração)

As instruções em um método devem estar um nível de abstração abaixo do próprio método. Da mesma forma, todos devem estar no mesmo nível de abstração

```ABAP
METHOD create_and_publish.
  post = create_post( user_input ).
  post->publish( ).
ENDMETHOD.
```

em vez de confundir misturas de conceitos de baixo nível ( `trim`, `to_upper`, ...) e alto nível ( , ...) como`publish`

```ABAP
" Fora do padrão
METHOD create_and_publish.
  post = NEW blog_post( ).
  DATA(user_name) = trim( to_upper( sy-uname ) ).
  post->set_author( user_name ).
  post->publish( ).
ENDMETHOD.
```

Uma maneira confiável de descobrir qual é o nível correto de abstração é esta: Deixe o autor do método explicar o que o método faz em poucas palavras curtas, sem olhar para o código. Os marcadores que ele(a) numera são os sub-métodos que o método deve chamar ou as instruções que ele deve executar.

##### Mantenha os métodos pequenos

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Corpo-do-método](#Corpo-do-método) > [Regras](#Regras) > [Seção atual](#Mantenha-os-métodos-pequenos)

Os métodos devem ter menos de 20 instruções, ideal em torno de 3 a 5 instruções.

```ABAP
METHOD read_and_parse_version_filters.
  DATA(active_model_version) = read_random_version_under( model_guid ).
  DATA(filter_json) = read_model_version_filters( active_model_version-guid ).
  result = parse_model_version_filters( filter_json ).
ENDMETHOD.
```

A declaração `DATA` a seguir sozinha é suficiente para ver que o método circundante faz muito mais do que uma coisa:

```ABAP
" Fora do padrão
DATA:
  class           TYPE vseoclass,
  attributes      TYPE seoo_attributes_r,
  methods         TYPE seoo_methods_r,
  events          TYPE seoo_events_r,
  types           TYPE seoo_types_r,
  aliases         TYPE seoo_aliases_r,
  implementings   TYPE seor_implementings_r,
  inheritance     TYPE vseoextend,
  friendships     TYPE seof_friendships_r,
  typepusages     TYPE seot_typepusages_r,
  clsdeferrds     TYPE seot_clsdeferrds_r,
  intdeferrds     TYPE seot_intdeferrds_r,
  attribute       TYPE vseoattrib,
  method          TYPE vseomethod,
  event           TYPE vseoevent,
  type            TYPE vseotype,
  alias           TYPE seoaliases,
  implementing    TYPE vseoimplem,
  friendship      TYPE seofriends,
  typepusage      TYPE vseotypep,
  clsdeferrd      TYPE vseocdefer,
  intdeferrd      TYPE vseoidefer,
  new_clskey_save TYPE seoclskey.
```

Claro que há ocasiões em que não faz sentido reduzir ainda mais um método maior. Isso é perfeitamente aceitável, desde que o método permaneça focado em uma coisa:

```ABAP
METHOD decide_what_to_do.
  CASE temperature.
    WHEN burning.
      result = air_conditioning.
    WHEN hot.
      result = ice_cream.
    WHEN moderate.
      result = chill.
    WHEN cold.
      result = skiing.
    WHEN freezing.
      result = hot_cocoa.
  ENDCASE.
ENDMETHOD.
```

No entanto, ainda faz sentido validar se o código detalhado oculta um padrão mais adequado:

```ABAP
METHOD decide_what_to_do.
  result = VALUE #( spare_time_activities[ temperature = temperature ] OPTIONAL ).
ENDMETHOD.
```

### Controle de fluxo

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Seção atual](#Controle-de-fluxo)

#### Valide mais cedo

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Controle de fluxo ](#Controle-de-fluxo) > [Seção atual](#Valide-mais-cedo)

Valide e falhe o mais cedo possível:

```ABAP
METHOD do_something.
  IF input IS INITIAL.
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  DATA(massive_object) = build_expensive_object_from( input ).
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

As validações posteriores são mais difíceis de detectar e entender e podem já ter desperdiçado recursos para chegar lá.

```ABAP
" Fora do padrão
METHOD do_something.
  DATA(massive_object) = build_expensive_object_from( input ).
  IF massive_object IS NOT BOUND. " happens if input is initial
    RAISE EXCEPTION cx_sy_illegal_argument( ).
  ENDIF.
  result = massive_object->do_some_fancy_calculation( ).
ENDMETHOD.
```

#### CHECK vs. RETURN

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Controle de fluxo ](#Controle-de-fluxo) > [Seção atual](#CHECK-vs.-RETURN)

Não há consenso sobre se deve usar `CHECK` ou `RETURN ` ao sair de um método se a entrada não atender às expectativas.

Embora `CHECK`definitivamente forneça a sintaxe mais curta

```ABAP
METHOD read_customizing.
  CHECK keys IS NOT INITIAL.
  " faça o que precisa ser feito
ENDMETHOD.
```

o nome da instrução não revela o que acontece se a condição falhar, de modo que as pessoas provavelmente entenderão melhor a forma longa:

```ABAP
METHOD read_customizing.
  IF keys IS INITIAL.
    RETURN.
  ENDIF.
  " faça o que precisa ser feito
ENDMETHOD.
```

Você poderia evitar completamente a questão invertendo a validação e adotando um fluxo de controle de retorno único. Isso é considerado fora do padrão porque introduz uma profundidade de alinhamento desnecessária.

```ABAP
METHOD read_customizing.
  " Fora do padrão
  IF keys IS NOT INITIAL.
  	" faça o que precisa ser feito
  ENDIF.
ENDMETHOD.
```

De qualquer forma, considere se não retornar nada é realmente o comportamento apropriado. Os métodos devem fornecer um resultado significativo, ou seja, um parâmetro de retorno preenchido ou uma exceção. Retornar nada é, em muitos casos, semelhante a retornar `null`, o que deve ser evitado.

#### Evite o CHECK em outras posições

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Controle de fluxo ](#Controle-de-fluxo) > [Seção atual](#Evite-o-CHECK-em-outras-posições)

Não use `CHECK`fora da seção de inicialização de um método. A declaração se comporta de maneira diferente em diferentes posições e pode levar a efeitos imprecisos e inesperados.

Por exemplo, [`CHECK`em um `LOOP`termina a iteração atual e continua com a próxima](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapcheck_loop.htm) ; as pessoas podem acidentalmente esperar que ele termine o método ou saia do loop. Prefira usar um instrução `IF` em combinação com `CONTINUE`, pois `CONTINUE`só pode ser usada em loops.

### Retorno

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Seção atual](#Retorno)

#### Passando retorno de um método para outro

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Retorno](#Retorno) > [Seção atual](#Passando-retorno-de-um-método-para-outro)

Utilize o conceito de **method chaining** para evitar a criação de variáveis auxiliares para armazenar retorno de métodos. 

```ABAP
cl_demo_output=>display( zcl_material_master->get_materials( ) ).
```

Torna-se muito mais claro e mais curto do que:

```ABAP
DATA(lt_material) = zcl_material_master->get_materials( ).
cl_demo_output=>display( lt_material ).
```

#### Utilize declarações in-line nos parâmetros de retorno

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) > [Retorno](#Retorno) > [Seção atual](#Utilize-declarações-in-line-nos-parâmetros-de-retorno)

A fim de evitar dumps decorrentes de tipagem de dados incorreta ao chamar funções, utilize declarações in-line em seus parâmetros de retorno.

```ABAP
lcl_calculo=>get_imc(
        EXPORTING peso = lv_peso
				  altura = lv_altura
        IMPORTING imc = DATA(lv_imc) ).
```

Ao invés de:

```ABAP
DATA lv_imc(4) type p DECIMALS 2.
lcl_calculo=>get_imc(
        EXPORTING peso = lv_peso
				  altura = lv_altura
        IMPORTING imc = lv_imc ).
```
