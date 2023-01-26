# Clean Code

Para que os desenvolvimentos feitos pela Infinitfy possuam um código limpo (Clean Code), é imprescindível a utilização das seguintes práticas.

## Conteúdo

-  [Diretrizes](#Diretrizes)
- [Constantes](#Constantes)
  - [Estruturas Constantes](#Estruturas-Constantes)
- [Variáveis](#Variáveis)
  - [Declarações](Declarações)
    - [Variáveis](#Variáveis)
    - [LOOP AT](#LOOP-AT)
    - [Read Table](#Read-Table)
    - [Métodos](#Métodos)
    - [Field-Symbol](#Field-Symbol)
    - [Inserir Dados](#Inserir-Dados)
    - [Ler Índice](#Ler-Índice)
    - [Concatenar](#Concatenar)
  - [Conversão de dados](#Conversão-de-dados)
- [Selects](#Selects)
  - [Minimizando a criação de TYPES](#Minimizando-a-criação-de-TYPES)
  - [Retornando valores dinâmicos](#Retornando-valores-dinâmicos)
  - [Realizando cálculos](#Realizando-cálculos)
  - [Agregações](#Agregações)
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
  - [Executando Queries](#Executando-Queries)
    - [Processando uma tabela e retornando um único valor](#Processando-uma-tabela-e-retornando-um-único-valor)
    - [FILTER](#FILTER)
      - [FILTER de valor único](#FILTER-de-valor-único)
      - [FILTER com um filtro de tabela](#FILTER-com-um-filtro-de-tabela)
  - [Utilizando LOOP AT](#Utilizando-LOOP-AT)
    - [Quando utilizar](#Quando-utilizar)
    - [Utilize FIELD SYMBOL ao invés de MODIFY](#Utilize-FIELD-SYMBOL-ao-invés-de-MODIFY)
    - [GROUP-BY](#GROUP-BY)
  - [Verificando a existência de uma linha](#Verificando-a-existência-de-uma-linha)
  - [Evite leituras desnecessárias de tabelas](#Evite-leituras-desnecessárias-de-tabelas)
- [Strings](#Strings)
  - [Definindo textos](#Definindo-textos)
  - [Concatenação](#Concatenação)
  - [Adicionando zeros à esquerda](#Adicionando-zeros-à-esquerda)
  - [Removendo zeros à esquerda](#Removendo-zeros-à-esquerda)
  - [Largura, Alinhamento e Preenchimento](#Largura,-Alinhamento-e-Preenchimento)
  - [CASE](#CASE)
- [Estruturas Condicionais](#Estruturas-Condicionais)
  - [IF ELSE](#IF-ELSE)
  - [CASE](#CASE)
  - [SWITCH](#SWITCH)
  - [COND](#COND)
  - [Retorno de funções e métodos em expressões lógicas](#Retorno-de-funções-e-métodos-em-expressões-lógicas)
    - [Omitindo ABAP_TRUE](#Omitindo-ABAP_TRUE)
- [Classes](#Classes)
  - [Instanciando objetos](#Instanciando-objetos)
    - [Tipo dinâmico](#Tipo-dinâmico)
- [Métodos](#Métodos)
  - [Tipos de Parâmetros](#Tipos-de-Parâmetros)
    - [Parâmetros do tipo TYPE REF TO DATA](#Parâmetros-do-tipo-TYPE-REF-TO-DATA)
  - [Retorno](#Retorno)
    - [Passando retorno de um método para outro](#Passando-retorno-de-um-método-para-outro)
    - [Utilize declarações in-line nos parâmetros de retorno](#Utilize-declarações-in-line-nos-parâmetros-de-retorno)



## Diretrizes

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Diretrizes)

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

### Declarações

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) > [Seção atual](#Declarações)

#### Variáveis 

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#Variáveis)

```ABAP
DATA(lv_text) = 'Marcos'
```

#### LOOP AT

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#LOOP-AT)

```ABAP
DATA: lt_flight TYPE TABLE OF sflight.

LOOP AT lt_flight INTO DATA(ls_flight).
ENDLOOP.
```

#### Read Table

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#Read-Table)

```ABAP
READ TABLE lt_flight INTO DATA(ls_flight) index 1.
```

#### Métodos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#Métodos)

```ABAP
DATA(lv_node) = wd_context->get_child_node( name = wd_this->wdctx_node_detail ).
DATA(lv_element) = lv_node->get_element( ).
lv_element->get_attritute( EXPORTING name = 'DETAIL_TEXT'
						   IMPORTING value = DATA(lv_detail_text) ).
```

#### Field-Symbol

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#Field-Symbol)

```ABAP
READ TABLE lt_flight ASSINGNING FIELD-SYMBOL(<fs_string>) INDEX 1.
```

```ABAP
LOOP AT lt_flight ASSINGNING FIELD-SYMBOL(<fs_flight>).
ENDLOOP.
```

#### Inserir Dados

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#Inserir Dados)

```ABAP
TYPES ty_range TYPE RANGE OF string.

DATA(lt_dados) = VALUE ty_range ( ( sing = 'I' opition = 'EQ' low = 'Gabriel' ) 
								  ( sing = 'I' opition = 'EQ' low = 'Vitor' ) )
```

#### Ler Índice 

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#Ler-Índice)

```ABAP
DATA(lv_connid) = lt_sflight[ 1 ]-connid.
```

#### Concatenar

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) >[Declarações](Declarações) > [Seção atual](#Concatenar)

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

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Selects](#Selects) > [Seção atual](#Agregações)

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

##### CORRESPONDING 

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Passando dados de uma tabela para outra](#Passando-dados-de-uma-tabela-para-outra) > [Tabelas de tipos diferentes](#Tabelas-de-tipos-diferentes) > [Seção atual](#CORRESPONDING)

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

Para passar dados de várias tabelas para outra, utilize:

```ABAP
DATA(lt_material_saida) = VALUE ls_material_saida( 
							FOR <lw_material> IN gt_material
							FOR <lw_mat_type> IN gt_mat_type
                            WHERE ( mtart = <lw_material>-mtart )
                            LET wa_saida = VALUE ty_saida(
                               mtype = <lw_mat_type>-mtype
                            )
                            IN ( CORRESPONDING #( BASE ( wa_saida ) <lw_material> ) )
                            ).
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

### Executando Queries

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Executando-Queries)

#### Processando uma tabela e retornando um único valor

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Processando-uma-tabela-e-retornando-um-único-valor)

Utilize `REDUCE` para consultar uma tabela e ao mesmo tempo executar uma lógica para retornar um único valor como:

```ABAP
lv_stock_price = REDUCE #( INIT result = 0
                            FOR <lw_material> IN gt_material
                            NEXT result = result + <lw_material>-price ).
```

````ABAP
DATA GT_ITAB TYPE STANDARD TABLE OF I WITH EMPTY KEY.
GT_ITAB = VALUE #( FOR J = 1 WHILE J <= 10 ( J ) ).

DATA(LV_SUM) = REDUCE I( INIT X = 0 FOR WA IN  GT_ITAB NEXT X = X + WA ).

WRITE: / lv_sum.
````

Ele é menos e mais rápido do que: 

````ABAP
DATA GT_ITAB TYPE STANDARD TABLE OF I WITH EMPTY KEY.
GT_ITAB = VALUE #( FOR J = 1 WHILE J <= 10 ( J ) ).

DATA: LV_LINE TYPE I,
      LV_SUM  TYPE I.

LOOP AT GT_ITAB INTO LV_LINE.
  LV_SUM = LV_SUM + LV_LINE.
ENDLOOP.

WRITE: / lv_sum.
````

#### FILTER

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#FILTER)

Utilize para filtrar dados de uma tabela seguindo as seguintes condições .

- A tabela interna na qual o operador `FILTER` é usado deve ter pelo menos uma chave `SORTED` ou uma chave 

##### FILTER de valor único

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [FILTER](#FILTER) > [Seção atual](#FILTER-de-valor-único)

````ABAP
DATA: lt_flights_all TYPE STANDARD TABLE OF spfli 
                     WITH NON-UNIQUE SORTED KEY carrid
                     COMPONENTS carrid,
 
      lt_flight_lh   TYPE STANDARD TABLE OF spfli.

SELECT *  FROM spfli
          INTO TABLE @lt_flights_all.
IF sy-subrc = 0.

lt_flight_lh = FILTER #( lt_flights_all USING KEY carrid 
                                        WHERE carrid = 'LH ' ).

ENDIF.
````



##### FILTER com um filtro de tabela

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [FILTER](#FILTER) > [FILTER com um filtro de tabela](#FILTER-de-valor-único) > [Seção atual](#FILTER-com-um-filtro-de-tabela)

````ABAP
DATA: lt_flights_all TYPE STANDARD TABLE OF spfli
                     WITH NON-UNIQUE SORTED KEY carrid
                     COMPONENTS carrid,
      lt_flight_final TYPE STANDARD TABLE OF spfli.

SELECT *  FROM spfli
          INTO TABLE @lt_flights_all.
          
DATA filter_tab  TYPE SORTED TABLE OF scarr-carrid
                 WITH UNIQUE KEY table_line.
filter_tab = VALUE #( ( 'AA ' ) ( 'LH ' ) ).

lt_flight_final = FILTER #( lt_flights_all IN filter_tab
                                           WHERE carrid = table_line ).

cl_demo_output=>write_data( lt_flights_all ).
cl_demo_output=>write_data( lt_flight_final ).
cl_demo_output=>display( ).
````



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

#### GROUP BY

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Utilizando LOOP AT](#Utilizando-LOOP-AT) > [Seção atual](#GROUP-BY)

Utilize `GROUP BY`  junto com o `LOOP AT`para o agrupamento linhas dado uma condição especifica. 

````ABAP
DATA li_flights TYPE TABLE OF spfli WITH EMPTY KEY.

SELECT * FROM  spfli
         INTO TABLE @li_flights.

DATA ls_members LIKE li_flights.
LOOP AT li_flights INTO DATA(wa_flight)
     GROUP BY ( carrier = wa_flight-carrid cityfr = wa_flight-cityfrom )
              ASCENDING
              ASSIGNING FIELD-SYMBOL(<fs_group>).
  CLEAR ls_members.
  LOOP AT GROUP <fs_group> ASSIGNING FIELD-SYMBOL(<fs_flight>).
    ls_members = VALUE #( BASE ls_members ( <fs_flight> ) ).
  ENDLOOP.
  cl_demo_output=>write( ls_members ).
ENDLOOP.
cl_demo_output=>display( ).
````

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

Utilize `TRY CATCH` para lidar com uma possível linha inexistente e reaja à exceção:

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

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Strings](#Strings) > [Seção atual](#Largura,-Alinhamento-e-Preenchimento)

- Width = Determina a largura do texto.
- Align = Alinha o texto, para esquerda `LEFT` , centro `CENTER` e direita `RIGHT`.
- PAD = Preenche todo o espaço que sobrou do `Width`.

```ABAP
WRITE / |{ 'Left'     WIDTH = 20 ALIGN = LEFT   PAD = '0' }|.
WRITE / |{ 'Centre'   WIDTH = 20 ALIGN = CENTER PAD = '0' }|.
WRITE / |{ 'Right'    WIDTH = 20 ALIGN = RIGHT  PAD = '0' }|.
```

### CASE

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Strings](#Strings) > [Seção atual](#CASE)

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

Utilize `CASE` para comparações simples, de um campo com múltiplos valores:

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

### Tipos de Parâmetros

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Métodos](#Métodos) >  [Seção atual](#Tipos-de-Parâmetros)

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
