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
  - [Uso de nome padrão é opcional](#Uso-de-nome-padrão-é-opcional)
  - [Evite obscurecer as funções integradas](#Evite-obscurecer-as-funções-integradas)
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
  - [Atribuição condicional](#Atribuição-condicional)
- [Tabelas](#Tabelas)
  - [Utilize o tipo de tabela correto](#Utilize-o-tipo-de-tabela-correto)
    - [Hashed Table](#Hashed-Table)
    - [Sorted Table](#Sorted-Table)
    - [Standard Table](#Standard-Table)
  - [Chaves](#Chaves)
  - [Inserção de dados](#Inserção-de-dados)
  - [Buscando linhas](#Buscando-linhas)
    - [Índice](#Índice)
  - [Acessando campos de uma linha diretamente](#Acessando-campos-de-uma-linha-diretamente)
  - [Verificando a existência de uma linha](#Verificando-a-existência-de-uma-linha)
  - [Evite leituras de tabelas desnecessárias](#Evite-leituras-de-tabelas-desnecessárias)
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



## Como refatorar código legado

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Como-refatorar-código-legado)

Os tópicos Booleans , Conditions , Ifs e Methods são mais recompensadores se você estiver trabalhando em um projeto legado com toneladas de código que não pode ou não deseja alterar porque pode ser aplicado a um novo código sem conflitos.

O tópico Nomes é muito exigente para projetos legados, pois pode introduzir uma brecha entre o código antigo e o novo, a ponto de seções como Evitar codificações, esp. É melhor ignorar a notação e os prefixos húngaros .

Tente não misturar diferentes estilos de desenvolvimento dentro do mesmo objeto de desenvolvimento ao realizar uma refatoração. Se o código legado contiver apenas declarações iniciais e uma refatoração completa para usar declarações inline não for possível, provavelmente é melhor manter o estilo legado em vez de misturar os dois estilos. Existem várias situações semelhantes em que a mistura de estilos pode causar confusão, por exemplo:

- Mixagem `REF TO`e `FIELD-SYMBOL`quando em loop.
- Misturando `NEW`e `CREATE OBJECT`chamando um arquivo `CONSTRUCTOR`.
- Misturando `RETURNING`e `EXPORTING`nas assinaturas de método de métodos retornando/exportando apenas um parâmetro.

Observamos bons resultados com um plano de quatro etapas para refatoração:

1. Coloque a equipe a bordo. Comunique e explique o novo estilo e faça com que todos na equipe do projeto concordem com ele. Você não precisa comprometer todas as diretrizes de uma vez, apenas comece com um pequeno subconjunto indiscutível e evolua a partir daí.
2. Siga a *regra do escoteiro* para sua rotina diária de trabalho: *sempre deixe o código que você edita um pouco mais limpo do que você o encontrou* . Não fique obcecado com isso gastando horas "limpando o acampamento", apenas gaste alguns minutos extras e observe como as melhorias se acumulam com o tempo.
3. Construa *ilhas limpas*: de vez em quando, pegue um pequeno objeto ou componente e tente deixá-lo limpo em todos os aspectos. Essas ilhas demonstram o benefício do que você está fazendo e formam bases domésticas solidamente testadas para refatoração adicional.
4. Fale sobre isso. Não importa se você configura revisões de código Fagan da velha escola , realiza sessões de informações ou forma quadros de discussão em sua ferramenta de bate-papo favorita: você precisará falar sobre suas experiências e aprendizados para permitir que a equipe desenvolva um entendimento comum.

## Nomeando Objetos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Nomeando-Objetos)

### Use nomes descritivos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Use-nomes-descritivos)

Use nomes que transmitam o conteúdo e o significado das coisas.

```ABAP
CONSTANTS gc_max_wait_time_in_seconds TYPE i ...
DATA lt_customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

Não se concentre no tipo de dados ou na codificação técnica. Eles dificilmente contribuem para a compreensão do código.

```ABAP
" Fora do padrão
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

Não tente corrigir nomes ruins por meio de comentários.

### Use nomes pronunciáveis

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Use-nomes-pronunciáveis)

Pensamos e falamos muito sobre objetos, então use nomes que você consiga pronunciar, por exemplo prefira `detection_object_types`algo enigmático como `dobjt`.

### Evite abreviaturas

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Evite-abreviaturas)

Se você tiver espaço suficiente, escreva os nomes completos. Comece a abreviar apenas se exceder os limites de comprimento.

Se você tiver que abreviar, comece com as palavras *sem importância .*

Abreviar as coisas pode parecer eficiente à primeira vista, mas torna-se ambíguo muito rapidamente. Por exemplo, "cust" `cust`significa "personalizar", "cliente" ou "personalizar"? Todos os três são comuns em aplicativos SAP.

### Use as mesmas abreviações em todos os lugares

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Use-as-mesmas-abreviações-em-todos-os-lugares)

As pessoas pesquisarão por palavras-chave para encontrar códigos relevantes. Apoie isso usando a mesma abreviação para a mesma coisa. Por exemplo, sempre abrevie "tipo de objeto de detecção" para "dobjt" em vez de misturar "dot", "dotype", "detobjtype" e assim por diante.

### Use substantivos para classes e verbos para métodos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Use-substantivos-para-classes-e-verbos-para-métodos)

Use substantivos ou locuções substantivas para nomear classes, interfaces e objetos:

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

Use verbos ou frases verbais para nomear métodos:

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

Iniciar métodos booleanos com verbos como `is_`e `has_`gera um bom fluxo de leitura:

```ABAP
IF is_empty( table ).
```

Recomendamos nomear funções como métodos:

```ABAP
FUNCTION /clean/read_alerts
```

### Evite palavras de ruído 

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Evite-palavras-de-ruído )

Omitir palavras de ruído como "data", "info", "object":

```ABAP
account  " instead of account_data
alert    " instead of alert_object
```

ou substituí-los por algo específico que realmente agregue valor

```ABAP
user_preferences          " instead of user_info
response_time_in_seconds  " instead of response_time_variable
```

### Escolha uma palavra por conceito

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Escolha-uma-palavra-por-conceito )

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

Escolha um termo para um conceito e atenha-se a ele; não misture outros sinônimos. Sinônimos farão o leitor perder tempo procurando uma diferença que não existe

```ABAP
" Fora do padrão
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

### Uso de nomes de design patterns

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Uso-de-nome-padrão-é-opcional)

Não use os nomes dos design patterns de software para classes e interfaces, a menos que você realmente queira dizer isso. Por exemplo, não chame sua classe `file_factory`a menos que ela realmente implemente o padrão de design de fábrica. Os padrões mais comuns incluem: [singleton](https://en.wikipedia.org/wiki/Singleton_pattern), [factory](https://en.wikipedia.org/wiki/Factory_method_pattern), [facade](https://en.wikipedia.org/wiki/Facade_pattern), [composite](https://en.wikipedia.org/wiki/Composite_pattern), [decorator](https://en.wikipedia.org/wiki/Decorator_pattern), [iterator](https://en.wikipedia.org/wiki/Iterator_pattern), [observer](https://en.wikipedia.org/wiki/Observer_pattern), e [strategy](https://en.wikipedia.org/wiki/Strategy_pattern).

### Evite utilizar nomes de funções built-in

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Nomes](#Nomes) > [Seção atual](#Evite-obscurecer-as-funções-integradas)

Dentro de uma classe, uma função built-in é sempre obscurecida pelos métodos da classe se eles tiverem o mesmo nome, independentemente do número e tipo de argumentos na função. A função também é obscurecida independentemente do número e tipo de parâmetros do método. As funções incorporadas são, por exemplo, `condense( )`, `lines( )`, `line_exists( )`, `strlen( )`, etc.

```ABAP
" Fora do padrão
METHODS lines RETURNING VALUE(result) TYPE i.    
METHODS line_exists RETURNING VALUE(result) TYPE i.  
```

```ABAP
" Fora do padrão 
CLASS-METHODS condense RETURNING VALUE(result) TYPE i.   
CLASS-METHODS strlen RETURNING VALUE(result) TYPE i.  
```

## Linguagem

### Cuidado com o legado

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo)  > [Seção atual](#Linguagem)

Se você codificar para releases ABAP mais antigos, siga os conselhos deste guia com cuidado: Muitas recomendações abaixo fazem uso de sintaxe e construções relativamente novas que podem não ser suportadas em releases ABAP mais antigos. Valide as diretrizes que deseja seguir na versão mais antiga à qual você deve dar suporte. Não descarte simplesmente o Código Limpo como um todo - a grande maioria das regras (por exemplo, nomeação, comentários) funcionará em *qualquer* versão ABAP.

### Prefira a orientação a objetos à programação processual

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Linguagem](#Linguagem) > [Seção atual](#Prefira-a-orientação-a-objetos-à-programação-processual)

Programas orientados a objetos (classes, interfaces) são melhor segmentados e podem ser refatorados e testados mais facilmente do que códigos procedurais (funções, programas). Embora existam situações em que você deve fornecer objetos procedurais (uma função para um RFC, um programa para uma transação), esses objetos devem fazer pouco mais do que chamar uma classe correspondente que forneça o recurso real:

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

### Prefira construções de linguagem funcional a procedimental

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Linguagem](#Linguagem) > [Seção atual](#Prefira-construções-de-linguagem-funcional-a-procedimental)

Eles geralmente são mais curtos e são mais naturais para os programadores modernos.

```ABAP
DATA(variable) = 'A'.
" MOVE 'A' TO variable.

DATA(uppercase) = to_upper( lowercase ).
" TRANSLATE lowercase TO UPPER CASE.

index += 1.         " >= NW 7.54
index = index + 1.  " < NW 7.54
" ADD 1 TO index.

DATA(object) = NEW /clean/my_class( ).
" CREATE OBJECT object TYPE /dirty/my_class.

result = VALUE #( FOR row IN input ( row-text ) ).
" LOOP AT input INTO DATA(row).
"  INSERT row-text INTO TABLE result.
" ENDLOOP.

DATA(line) = value_pairs[ name = 'A' ]. " entry must exist
DATA(line) = VALUE #( value_pairs[ name = 'A' ] OPTIONAL ). " entry can be missing
" READ TABLE value_pairs INTO DATA(line) WITH KEY name = 'A'.

DATA(exists) = xsdbool( line_exists( value_pairs[ name = 'A' ] ) ).
IF line_exists( value_pairs[ name = 'A' ] ).
" READ TABLE value_pairs TRANSPORTING NO FIELDS WITH KEY name = 'A'.
" DATA(exists) = xsdbool( sy-subrc = 0 ).
```

Muitas das regras detalhadas abaixo são apenas reiterações específicas deste conselho geral.

### Evite elementos de linguagem obsoletos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Linguagem](#Linguagem) > [Seção atual](#Evite-elementos-de-linguagem-obsoletos)

Ao atualizar sua versão ABAP, verifique se há elementos de linguagem obsoletos e evite usá-los.

Por exemplo, as `@`variáveis "host" com escape na instrução a seguir deixam um pouco mais claro o que é uma variável de programa e o que é uma coluna no banco de dados,

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = @carrid AND
        connid = @connid
  INTO TABLE @itab.
```

em comparação com a [forma obsoleta sem escape](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenopen_sql_hostvar_obsolete.htm)

```ABAP
SELECT *
  FROM spfli
  WHERE carrid = carrid AND
        connid = connid
  INTO TABLE itab.
```

Alternativas mais recentes tendem a melhorar a legibilidade do código e reduzir os conflitos de design com os paradigmas de programação modernos, de modo que mudar para eles pode limpar seu código automaticamente.

Enquanto continuam a funcionar, elementos obsoletos podem deixar de se beneficiar de otimizações em termos de velocidade de processamento e consumo de memória.

Com elementos de linguagem modernos, você pode integrar jovens ABAPers mais facilmente, que podem não estar mais familiarizados com as construções desatualizadas porque não são mais ensinadas nos treinamentos da SAP.

A documentação do SAP NetWeaver contém uma seção estável que lista elementos de linguagem obsoletos, por exemplo [NW 7.50](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/index.htm?file=abenabap_obsolete.htm) , [NW 7.51](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_obsolete.htm) , [NW 7.52](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenabap_obsolete.htm) , [NW 7.53](https://help.sap.com/doc/abapdocu_753_index_htm/7.53/en-US/index.htm?file=abenabap_obsolete.htm) .

### Use padrões de design com sabedoria

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Linguagem](#Linguagem) > [Seção atual](#Use-padrões-de-design-com-sabedoria)

Onde eles são apropriados e fornecem benefícios perceptíveis. Não aplique padrões de design em todos os lugares apenas por aplicar.

## Constantes

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Constantes)

### Use constantes ao invés de hardcode

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Constantes](#Constantes) > [Seção atual](#Use-constantes-ao-invés-de-hardcode)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

pois é mais claro que 

```ABAP
" Fora do padrão
IF abap_type = 'D'.
```

### Agrupamento

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Constantes](#Constantes) > [Seção atual](#Agrupamento)

Para o agrupamento de constantes utilize uma das seguintes alternativas.

#### Classes de Enumeração

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Constantes](#Constantes) > [Agrupamento](#Agrupamento) >[Seção atual](#Classes-de-Enumeração)

Utilize classes ao invés de interfaces para agrupar constantes.

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      gc_warning TYPE symsgty VALUE 'W',
      gc_error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

ou:

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.v
    CLASS-DATA:
      gc_warning TYPE REF TO /clean/message_severity READ-ONLY,
      gc_error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

Ao invés de:

```ABAP
" Fora do padrão
INTERFACE /dirty/common_constants.
  CONSTANTS:
    lc_warning      TYPE symsgty VALUE 'W',
    lc_transitional TYPE i       VALUE 1,
    lc_error        TYPE symsgty VALUE 'E',
    lc_persisted    TYPE i       VALUE 2.
ENDINTERFACE.
```

#### Estruturas Constantes

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Constantes](#Constantes) > [Agrupamento](#Agrupamento) >[Seção atual](#Estruturas-Constantes)

Utilize estruturas constantes ao invés de constantes soltas.

```ABAP
CONSTANTS:
  BEGIN OF message_severity,
    lc_warning TYPE symsgty VALUE 'W',
    lc_error   TYPE symsgty VALUE 'E',
  END OF message_severity,
  BEGIN OF message_lifespan,
    lc_transitional TYPE i VALUE 1,
    lc_persisted    TYPE i VALUE 2,
  END OF message_lifespan.
```

Torna a relação mais clara do que:

```ABAP
" Fora do padrão
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

Dessa forma é possível realizar a seguinte validação:

```ABAP
DO.
  ASSIGN COMPONENT sy-index OF STRUCTURE message_severity TO FIELD-SYMBOL(lfs_<constant>).
  IF sy-subrc IS INITIAL.
    IF input = lfs_<constant>.
      DATA(is_valid) = abap_true.
      RETURN.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.
ENDDO.
```

## Variáveis

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Variáveis)

#### Declarações em Métodos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) > [Seção atual](#Declarações-em-Métodos)

Utilize declarações in-line:

```ABAP
METHOD do_something.
  DATA(lv_name) = 'something'.
  DATA(lv_reader) = /clean/reader=>get_instance_for( lv_name ).
  result = lv_reader->read_it( ).
ENDMETHOD.
```

ao invés de um bloco `DATA` separado no início do método:

```ABAP
" Fora do padrão
METHOD do_something.
  DATA:
    lv_name   TYPE seoclsname,
    lv_reader TYPE REF TO /dirty/reader.
  lv_name = 'something'.
  lv_reader = /dirty/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

#### Declarações dentro de IF-ELSE

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) > [Seção atual](#Declarações-dentro-de-IF-ELSE)

Não utilize declarações in-line dentro de IF-ELSE's.

```ABAP
" Fora do padrão
IF has_entries = abap_true.
  DATA(lv_value) = 1.
ELSE.
  lv_value = 2.
ENDIF.
```

Isso funciona bem porque o ABAP lida com declarações inline como se estivessem no início do método. No entanto, é extremamente confuso para os leitores, especialmente se o método for mais longo e você não identificar a declaração imediatamente. Nesse caso, interrompa o inline e coloque a declaração na frente:

```ABAP
DATA lv_value TYPE i.
IF has_entries = abap_true.
  lv_value = 1.
ELSE.
  lv_value = 2.
ENDIF.
```

#### Não encadeie declarações iniciais

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) > [Seção atual](#Não-encadeie-declarações-iniciais)

```ABAP
DATA lv_name TYPE seoclsname.
DATA lv_reader TYPE REF TO reader.
```

O encadeamento sugere que as variáveis definidas estão relacionadas em um nível lógico. Para usá-lo de forma consistente, você teria que garantir que todas as variáveis encadeadas pertençam juntas e introduzir grupos de cadeia adicionais para adicionar variáveis. Embora isso seja possível, geralmente não vale o esforço.

O encadeamento também complica desnecessariamente a reformatação e a refatoração porque cada linha parece diferente e alterá-las requer a interferência de dois-pontos, pontos e vírgulas, que não valem o esforço.

```ABAP
" Fora do padrão
DATA:
  lv_name   TYPE seoclsname,
  lv_reader TYPE REF TO reader.
```

#### Atribuição condicional

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Variáveis](#Variáveis) > [Seção atual](#Atribuição-condicional)

Utilize `COND` ao invés de `IF-ELSE` para atribuições condicionais:

```ABAP
gv_erro = COND #( WHEN lv_matnr IS NOT INITIAL THEN abap_false ELSE abap_true ).
```

Torna-se muito mais claro e curto do que:

```ABAP
IF lv_matnr IS NOT INITIAL.
  gv_erro = abap_false.
ELSE.
  gv_erro = abap_true.
ENDIF.
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

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Utilize o tipo de tabela correto](#Utilize-o-tipo-de-tabela-correto) > [Seção atual](#Standard-Table)

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

### Buscando linhas

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Buscando-linhas)

#### Índice

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Buscando linhas](#Buscando-linhas) > [Seção atual](#Índice)

Utilize ` lt_my_table[ lv_index ]` para acessar uma linha pelo seu índice:

```ABAP
DATA(wa_material) = lt_material[ 70 ].
```

expressa a intenção de forma mais clara e mais curta do que:

```ABAP
READ TABLE lt_material INDEX 70 INTO DATA(wa_material).
```

### Acessando campos de uma linha diretamente

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Acessando-campos-de-uma-linha-diretamente)

É possível acessar diretamente qualquer campo da linha buscada sem a necessidade de definir uma Workarea para isso:

```ABAP
DATA(lv_maktx) = lt_material[ 70 ]-maktx.
```

expressa a intenção de forma mais clara e mais curta do que:

```ABAP
READ TABLE lt_material INDEX 70 INTO DATA(wa_material).
DATA(lv_maktx) = wa_material-maktx.
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

### Evite leituras de tabelas desnecessárias

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Tabelas](#Tabelas) > [Seção atual](#Evite-leituras-de-tabelas-desnecessárias)

Caso você espere que uma linha esteja lá, leia uma vez e reaja à exceção,

```ABAP
TRY.
    DATA(row) = lt_my_table[ key = input ].
  CATCH cx_sy_lt_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

em vez de desarrumar e desacelerar o fluxo de controle principal com uma leitura dupla

```ABAP
" Fora do padrão
IF NOT line_exists( lt_my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDIF.
DATA(row) = lt_my_table[ key = input ].
```

## Strings

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Strings)

### Definindo textos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Strings](#Strings) > [Seção atual](#Definindo-textos)

Utilize ` para definir literais:

```ABAP
CONSTANTS gc_some_constant TYPE string VALUE `ABC`.
DATA(gc_some_string) = `ABC`.  " --> TYPE string
```

Evite usar `'`, pois adiciona uma conversão de tipo supérflua e confunde o leitor se ele está lidando com um `CHAR`ou `STRING`:

```ABAP
" Fora do padrão
DATA gc_some_string TYPE string.
gc_some_string = 'ABC'.
```

`|`geralmente está bem, mas não pode ser usado `CONSTANTS`e adiciona sobrecarga desnecessária ao especificar um valor fixo:

```ABAP
" Fora do padrão
DATA(gc_some_string) = |ABC|.
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

## Booleanos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Seção atual](#Booleanos)

### Use booleanos com sabedoria

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Booleanos](#Booleanos) > [Seção atual](#Use-booleanos-com-sabedoria)

Frequentemente encontramos casos em que booleanos parecem ser uma escolha natural

```ABAP
" fora do padrão
is_archived = abap_true.
```

até que uma mudança de ponto de vista sugere que deveríamos ter escolhido uma enumeração

```ABAP
archiving_status = /clean/archivation_status=>archiving_in_process.
```

Geralmente, os booleanos são uma má escolha para distinguir tipos de coisas porque você quase sempre encontrará casos que não são exclusivamente um ou outro

```ABAP
assert_true( xsdbool( document->is_archived( ) = abap_true AND
                      document->is_partially_archived( ) = abap_true ) ).
```

### Use ABAP_BOOL para booleanos

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Booleanos](#Booleanos) > [Seção atual](#Use-ABAP_BOOL-para-booleanos)

```ABAP
DATA has_entries TYPE abap_bool.
```

Não use o tipo genérico `char1`. Embora seja tecnicamente compatível, obscurece o fato de estarmos lidando com uma variável booleana.

Evite também outros tipos booleanos, pois eles costumam ter efeitos colaterais estranhos, por exemplo, `boolean`suporta um terceiro valor "indefinido" que resulta em erros sutis de programação.

Em alguns casos, você pode precisar de um elemento de dicionário de dados, por exemplo, para campos DynPro. `abap_bool`não pode ser usado aqui porque é definido no tipo pool `abap`, não no dicionário de dados. Neste caso, recorra a `boole_d`ou `xfeld`. Crie seu próprio elemento de dados se precisar de uma descrição personalizada.

### Use ABAP_TRUE e ABAP_FALSE para comparações

> [Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Booleanos](#Booleanos) > [Seção atual](#Use-ABAP_TRUE-e-ABAP_FALSE-para-comparações)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

Não use os equivalentes de caracteres `'X'`e `' '`ou `space`; eles tornam difícil ver que esta é uma expressão booleana:

```ABAP
" Fora do padrão
has_entries = 'X'.
IF has_entries = space.
```

Evite comparações com `INITIAL`- isso força os leitores a lembrar que `abap_bool`o padrão é `abap_false`:

```ABAP
" Fora do padrão
IF has_entries IS NOT INITIAL.
```

### Use XSDBOOL para definir variáveis booleanas

[Clean Code](#Clean-Code) > [Conteúdo](#Conteúdo) > [Booleanos](#Booleanos) > [Seção atual](#Use-XSDBOOL-para-definir-variáveis-booleanas)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

O equivalente `IF`- `THEN`- `ELSE`é muito mais longo para nada:

```ABAP
" Fora do padrão
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool`é o melhor método para o nosso propósito, pois produz diretamente um `char1`, que se ajusta melhor ao nosso tipo booleano `abap_bool`. As funções equivalentes `boolc`e `boolx`produzem tipos diferentes e adicionam uma conversão de tipo implícita desnecessária.

Concordamos que o nome `xsdbool`é azarado e enganoso; afinal, não estamos nem um pouco interessados nas partes "XML Schema Definition" sugeridas pelo prefixo "xsd".

Uma alternativa possível `xsdbool`é a `COND`forma ternária. Sua sintaxe é intuitiva, mas um pouco mais longa porque repete desnecessariamente o `THEN abap_true`segmento, e requer conhecimento do valor padrão implícito `abap_false`- por isso sugerimos apenas como solução secundária.

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## Condições

### Tente tornar as condições positivas

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

### Considere o uso de chamadas de método predicativo para métodos booleanos

O método predicativo chama métodos booleanos, por exemplo

```ABAP
IF [ NOT ] condition_is_fulfilled( ).
```

não é apenas muito compacto, mas também permite manter o código mais próximo da linguagem natural como a expressão de comparação:

```ABAP
" Fora do padrão
IF condition_is_fulfilled( ) = abap_true / abap_false.
```

Lembre-se de que a chamada de método predicativo `... meth( ) ...`é apenas uma forma abreviada de `... meth( ) IS NOT INITIAL ...`, consulte [Chamada de método predicativo](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenpredicative_method_calls.htm) na Documentação de palavras-chave ABAP. É por isso que a forma abreviada só deve ser usada para métodos que retornam tipos em que o valor não inicial tem o significado de "verdadeiro" e o valor inicial tem o significado de "falso".

### Considere a decomposição de condições complexas

As condições podem se tornar mais fáceis ao decompô-las nas partes elementares que as compõem:

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

em vez de deixar tudo no lugar:

```ABAP
" Fora do padrão
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

### Considere extrair condições complexas

É quase sempre uma boa ideia extrair condições complexas para métodos próprios:

```ABAP
IF is_provided( example ).

METHOD is_provided.
  DATA(is_filled) = xsdbool( example IS NOT INITIAL ).
  DATA(is_working) = xsdbool( applies( example ) = abap_true OR
                              fits( example ) = abap_true ).
  result = xsdbool( is_filled = abap_true AND
                    is_working = abap_true ).
ENDMETHOD.
```



## Ifs

### Nenhuma ramificação IF vazia

```ABAP
IF has_entries = abap_false.
  " do some magic
ENDIF.
```

é mais curto e mais claro do que

```ABAP
" Fora do padrão
IF has_entries = abap_true.
ELSE.
  " faça alguma mágica
ENDIF.
```

### Prefira CASE a ELSE IF para várias condições alternativas

```ABAP
CASE type.
  WHEN type-some_type.
    " ...
  WHEN type-some_other_type.
    " ...
  WHEN OTHERS.
    RAISE EXCEPTION NEW /clean/unknown_type_failure( ).
ENDCASE.
```

`CASE`torna fácil ver um conjunto de alternativas que se excluem mutuamente. Ele pode ser mais rápido do que uma série de `IF`s porque pode traduzir para um comando de microprocessador diferente em vez de uma série de condições avaliadas posteriormente. Você pode introduzir novos casos rapidamente, sem ter que repetir a variável perspicaz repetidamente. A instrução ainda evita alguns erros que podem ocorrer ao aninhar acidentalmente os `IF`-s `ELSEIF`.

```ABAP
" Fora do padrão
IF type = type-some_type.
  " ...
ELSEIF type = type-some_other_type.
  " ...
ELSE.
  RAISE EXCEPTION NEW /dirty/unknown_type_failure( ).
ENDIF.
```

### Mantenha a profundidade de nidificação baixa

```ABAP
" Fora do padrão 
IF <this>.
  IF <that>.
  ENDIF.
ELSE.
  IF <other>.
  ELSE.
    IF <something>.
    ENDIF.
  ENDIF.
ENDIF.
```

Os aninhados `IF`ficam difíceis de entender muito rapidamente e requerem um número exponencial de casos de teste para uma cobertura completa.

As árvores de decisão geralmente podem ser desmontadas formando submétodos e introduzindo variáveis auxiliares booleanas.

Outros casos podem ser simplificados pela fusão de IFs, como

```ABAP
IF <this> AND <that>.
```

em vez do aninhado desnecessariamente

```ABAP
" Fora do padrão
IF <this>.
  IF <that>.
```

## Expressões regulares

### Prefira métodos mais simples a expressões regulares

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

As expressões regulares tornam-se difíceis de entender muito rapidamente. Casos simples geralmente são mais fáceis sem eles.

As expressões regulares também costumam consumir mais memória e tempo de processamento porque precisam ser analisadas em uma árvore de expressão e compiladas em tempo de execução em um correspondente executável. Soluções simples podem funcionar com um loop direto e uma variável temporária.

### Prefira verificações de base a expressões regulares

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
  EXPORTING
    cls_name = class_name
  EXCEPTIONS
    ...
```

em vez de reinventar as coisas

```ABAP
" Fora do padrão
DATA(is_valid) = matches( val     = class_name
                          pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

### Considere montar expressões regulares complexas

```ABAP
CONSTANTS class_name TYPE string VALUE `CL\_.*`.
CONSTANTS interface_name TYPE string VALUE `IF\_.*`.
DATA(object_name) = |{ class_name }\|{ interface_name }|.
```

Algumas expressões regulares complexas tornam-se mais fáceis quando você demonstra ao leitor como elas são construídas a partir de partes mais elementares.

## Classes

### Classes: Orientação a objetos

#### Prefira objetos a classes estáticas

As classes estáticas desistem de todas as vantagens obtidas pela orientação a objetos em primeiro lugar. Eles especialmente tornam quase impossível substituir dependências produtivas por dublês de teste em testes de unidade.

Se você pensar em tornar uma classe ou método estático, a resposta quase sempre será: não.

Uma exceção aceita a essa regra são as classes utils de tipo simples. Seus métodos facilitam a interação com certos tipos ABAP. Eles não são apenas completamente sem estado, mas tão básicos que se parecem com instruções ABAP ou funções internas. O fator discriminatório é que seus consumidores os amarram em seu código de forma tão forte que eles realmente não querem zombar deles em testes de unidade.

```ABAP
CLASS /clean/string_utils DEFINITION [...].
  CLASS-METHODS trim
   IMPORTING
     string        TYPE string
   RETURNING
     VALUE(result) TYPE string.
ENDCLASS.

METHOD retrieve.
  DATA(trimmed_name) = /clean/string_utils=>trim( name ).
  result = read( trimmed_name ).
ENDMETHOD.
```

#### Prefira a composição à herança

Evite construir hierarquias de classes com herança. Em vez disso, favoreça a composição.

Herança limpa é difícil de projetar porque você precisa respeitar regras como o princípio de [substituição de Liskov](https://en.wikipedia.org/wiki/Liskov_substitution_principle) . Também é difícil de entender porque as pessoas precisam perceber e digerir os princípios orientadores por trás da hierarquia. A herança reduz a reutilização porque os métodos tendem a ser disponibilizados apenas para subclasses. Também complica a refatoração porque mover ou alterar membros tende a exigir alterações em toda a árvore hierárquica.

Composição significa que você projeta objetos pequenos e independentes, cada um servindo a um propósito específico. Esses objetos podem ser recombinados em objetos mais complexos por delegação simples e padrões de fachada. A composição pode produzir mais classes, mas não tem outras desvantagens.

Não deixe que esta regra o desencoraje a usar a herança nos lugares certos. Existem boas aplicações para herança, por exemplo, o [padrão de projeto Composite](https://en.wikipedia.org/wiki/Composite_pattern) . Apenas pergunte a si mesmo criticamente se a herança no seu caso realmente trará mais benefícios do que desvantagens. Em caso de dúvida, a composição geralmente é a escolha mais segura.

#### Não misture com estado e sem estado na mesma classe

Não misture os paradigmas de programação sem estado e com estado na mesma classe.

Na programação sem estado, os métodos recebem entrada e produzem saída, *sem nenhum efeito colateral* , resultando em métodos que produzem o mesmo resultado, independentemente de quando e em que ordem são chamados.

```ABAP
CLASS /clean/xml_converter DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS convert
      IMPORTING
        file_content  TYPE xstring
      RETURNING
        VALUE(result) TYPE /clean/some_inbound_message.
ENDCLASS.

CLASS /clean/xml_converter IMPLEMENTATION.
  METHOD convert.
    cl_proxy_xml_transform=>xml_xstring_to_abap(
      EXPORTING
        xml       = file_content
        ext_xml   = abap_true
        svar_name = 'ROOT_NODE'
      IMPORTING
        abap_data = result ).
   ENDMETHOD.
ENDCLASS.
```

Na programação com estado, manipulamos o estado interno dos objetos por meio de seus métodos, o que significa que está *cheio de efeitos colaterais* .

```ABAP
CLASS /clean/log DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS add_message IMPORTING message TYPE /clean/message.
  PRIVATE SECTION.
    DATA messages TYPE /clean/message_table.
ENDCLASS.

CLASS /clean/log IMPLEMENTATION.
  METHOD add_message.
    INSERT message INTO TABLE messages.
  ENDMETHOD.
ENDCLASS.
```

Ambos os paradigmas estão bem e têm suas aplicações. No entanto, *misturá* -los no mesmo objeto produz um código difícil de entender e com certeza falhará com erros obscuros de transporte e problemas de sincronicidade. Não faça isso.

### Alcance

#### Global por padrão, local somente quando apropriado

Trabalhe com classes globais como padrão. Use classes locais somente quando apropriado.

As classes locais são adequadas

- para estruturas de dados particulares muito específicas, por exemplo, um iterador para os dados da classe global, que só serão necessários aqui,
- para extrair um algoritmo de peça privada complexo, por exemplo, para separar aquele algoritmo de agregação de classificação multimétodo de finalidade especial do restante do código de sua classe,
- para permitir a simulação de certos aspectos da classe global, por exemplo, extraindo todo o acesso ao banco de dados para uma classe local separada que pode ser substituída por um teste duplo nos testes de unidade.

As classes locais impedem a reutilização porque não podem ser usadas em outro lugar. Embora sejam fáceis de extrair, as pessoas geralmente não conseguem encontrá-los, levando à duplicação indesejada de código. Orientação, navegação e depuração em classes locais muito longas são tediosas e irritantes. Como o ABAP bloqueia no nível de inclusão, as pessoas não poderão trabalhar em diferentes partes do local de inclusão simultaneamente (o que seria possível se fossem classes globais separadas).

Reconsidere o uso de classes locais se

- sua inclusão local abrange dezenas de classes e milhares de linhas de código,
- você pensa em classes globais como "pacotes" que contêm outras classes,
- suas classes globais degeneram em cascos vazios,
- você encontra código duplicado repetido em inclusões locais separadas,
- seus desenvolvedores começam a bloquear uns aos outros e se tornam incapazes de trabalhar em paralelo,
- suas estimativas de lista de pendências aumentam muito porque suas equipes não conseguem entender as sub-árvores locais umas das outras.

#### FINAL se não for projetado para herança

Faça classes que não são explicitamente projetadas para herança `FINAL`.

Ao projetar a cooperação de classe, sua primeira escolha deve ser a composição, não a herança . Habilitar a herança não é algo que deve ser feito levianamente, pois exige que você pense em coisas como `PROTECTED`vs. `PRIVATE` e o princípio de [substituição de Liskov](https://en.wikipedia.org/wiki/Liskov_substitution_principle) e congela muitos aspectos internos do design. Se você não considerou essas coisas em seu design de classe, você deve evitar a herança acidental tornando sua classe `FINAL`.

Existem alguns bons aplicativos para herança, *é* claro, por exemplo, o padrão de design [composto](https://en.wikipedia.org/wiki/Composite_pattern) . Os Business Add-Ins também podem se tornar mais úteis permitindo subclasses, permitindo que o cliente reutilize a maior parte do código original. No entanto, observe que todos esses casos têm herança incorporada por design desde o início.

Classes impuras que não implementam interfaces devem ser deixadas como não `FINAL`-para permitir que os consumidores zombem delas em seus testes de unidade.

#### Membros PRIVADOS por padrão, PROTEGIDOS somente se necessário

Torne atributos, métodos e outros membros de classe `PRIVATE`por padrão.

Faça-os apenas `PROTECTED`se quiser habilitar subclasses que os substituem.

Internos de classes devem ser disponibilizados para outros apenas em uma base de necessidade de conhecimento. Isso inclui não apenas chamadores externos, mas também subclasses. Tornar as informações superdisponíveis pode causar erros sutis por redefinições inesperadas e dificultar a refatoração porque pessoas de fora congelam membros em um lugar que ainda deveria ser líquido.

#### Considere usar imutável em vez de getter

Um imutável é um objeto que nunca muda após sua construção. Para esse tipo de objeto, considere o uso de atributos públicos somente leitura em vez de métodos getter.

```
CLASS /clean/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        a TYPE i
        b TYPE c
        c TYPE d.
    DATA a TYPE i READ-ONLY.
    DATA b TYPE c READ-ONLY.
    DATA c TYPE d READ-ONLY.
ENDCLASS.
```

em vez de

```ABAP
CLASS /dirty/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS get_a ...
    METHODS get_b ...
    METHODS get_c ...
  PRIVATE SECTION.
    DATA a TYPE i.
    DATA b TYPE c.
    DATA c TYPE d.
ENDCLASS.
```

> **Cuidado** : Para objetos que **possuem** valores variáveis, não use atributos públicos somente leitura. Caso contrário, esses atributos sempre devem ser mantidos atualizados, independentemente de seu valor ser necessário para qualquer outro código ou não.

#### Use READ-ONLY com moderação

Em segundo lugar, a adição funciona sutilmente diferente do que as pessoas podem esperar de outras linguagens de programação: dados READ-ONLY ainda podem ser modificados livremente de qualquer método dentro da própria classe, seus amigos e suas subclasses. Isso contradiz o comportamento mais difundido de escrever uma vez-modificar-nunca encontrado em outros idiomas. A diferença pode levar a surpresas ruins.

> Para evitar mal-entendidos: Proteger variáveis contra modificações acidentais é uma boa prática. Recomendamos aplicá-lo também ao ABAP se houver uma declaração apropriada.

### Constructors

#### Prefira NEW a CREATE OBJECT

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

em vez do desnecessariamente mais longo

```ABAP
" Fora do padrão
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

exceto onde você precisa de tipos dinâmicos, é claro

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### Se sua classe global for CREATE PRIVATE, deixe o CONSTRUCTOR public

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

Concordamos que isso se contradiz. Porém, de acordo com o artigo [*Instance Constructor* do ABAP Help](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abeninstance_constructor_guidl.htm) , especificar o `CONSTRUCTOR`no `PUBLIC SECTION`é necessário para garantir a correta compilação e validação da sintaxe.

Isso se aplica apenas a classes globais. Nas classes locais, torne o construtor privado, como deveria ser.

#### Prefira vários métodos de criação estáticos a parâmetros opcionais

```
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

ABAP não suporta [sobrecarga](https://en.wikipedia.org/wiki/Function_overloading) . Use variações de nomes e não parâmetros opcionais para obter a semântica desejada.

```ABAP
" Fora do padrão
METHODS constructor
  IMPORTING
    data       TYPE any OPTIONAL
    name       TYPE any OPTIONAL
    object_ref TYPE REF TO object OPTIONAL
    data_ref   TYPE REF TO data OPTIONAL
  [...]
```

A diretriz geral [*Dividir métodos em vez de adicionar parâmetros OPCIONAIS*](https://github.com/SMELGES-Infinit/styleguides/blob/main/clean-abap/CleanABAP.md#split-methods-instead-of-adding-optional-parameters) explica o raciocínio por trás disso.

Considere resolver construções complexas em uma construção de várias etapas com o [padrão de projeto Builder](https://en.wikipedia.org/wiki/Builder_pattern) .

#### Use nomes descritivos para vários métodos de criação

Boas palavras para iniciar os métodos de criação são `new_`, `create_`e `construct_`. As pessoas os conectam intuitivamente à construção de objetos. Eles também se encaixam bem em frases verbais como `new_from_template`, `create_as_copy`ou `create_by_name`.

```ABAP
CLASS-METHODS new_describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS new_describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS new_describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS new_describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

em vez de algo sem sentido como

```ABAP
" Fora do padrão
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

#### Faça singletons apenas onde várias instâncias não fazem sentido

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

Aplique o padrão singleton onde seu design orientado a objetos diz que ter uma segunda instância de algo não faz sentido. Use-o para garantir que cada consumidor esteja trabalhando com a mesma coisa, no mesmo estado e com os mesmos dados.

Não use o padrão singleton por hábito ou porque alguma regra de desempenho assim lhe diz. É o padrão mais usado e aplicado incorretamente e produz efeitos cruzados inesperados e complica desnecessariamente os testes. Se não houver motivos orientados ao design para um objeto unitário, deixe essa decisão para o consumidor - ele ainda pode alcançar o mesmo por meios fora do construtor, por exemplo, com uma fábrica.

## Métodos

### Calls

#### Não chame métodos estáticos por meio de variáveis de instância

Para chamar um método estático, use

```ABAP
cl_my_class=>static_method( ).
```

Em vez de chamá-lo por meio de uma variável de instância para`cl_my_class`

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

#### Omitir RECEIVING

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

#### Omita a palavra-chave opcional EXPORTING

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

em vez do desnecessariamente mais longo

```ABAP
" Fora do padrão
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### Omitir o nome do parâmetro em chamadas de parâmetro único

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

em vez do desnecessariamente mais longo

```ABAP
" Fora do padrão
DATA(unique_list) = remove_duplicates( list = list ).
```

Há casos, no entanto, em que o nome do método sozinho não é claro o suficiente e repetir o nome do parâmetro pode facilitar a compreensão:

```ABAP
car->drive( speed = 50 ).
update( asynchronous = abap_true ).
```

#### Omita a auto-referência ME ao chamar um atributo ou método de instância

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

### Métodos: Orientação a objetos

#### Prefira instância a métodos estáticos

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

#### Os métodos de instância pública devem fazer parte de uma interface

Os métodos de instância pública sempre devem fazer parte de uma interface. Isso separa as dependências e simplifica a zombaria delas em testes de unidade.

```ABAP
METHOD /clean/blog_post~publish.
```

Na orientação a objetos limpa, ter um método público sem uma interface não faz muito sentido - com poucas exceções, como classes de enumeração que nunca terão uma implementação alternativa e nunca serão ridicularizadas em casos de teste.

### Número do parâmetro

#### Apontar para poucos parâmetros de IMPORTING, na melhor das hipóteses, menos de três

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

seria muito mais claro do que

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

#### Dividir métodos em vez de adicionar parâmetros OPCIONAIS

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

A adição `PREFERRED PARAMETER`torna difícil ver qual parâmetro é realmente fornecido, dificultando a compreensão do código. Minimizar o número de parâmetros, especialmente os opcionais, reduz automaticamente a necessidade de arquivos `PREFERRED PARAMETER`.

#### RETURN, EXPORT ou CHANGE exatamente um parâmetro

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

Especialmente em comparação com vários parâmetros de EXPORTAÇÃO, isso permite que as pessoas usem o estilo de chamada funcional, poupa você de pensar `IS SUPPLIED`e evita que as pessoas se esqueçam acidentalmente de recuperar uma `ERROR_OCCURRED`informação vital.

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

#### Prefira RETURNING a EXPORTING

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

`RETURNING`não apenas torna a chamada mais curta, mas também permite o encadeamento de métodos e evita erros de mesma entrada e saída.

#### RETORNAR tabelas grandes geralmente é bom

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

Somente se houver prova real (= uma medição de desempenho ruim) para o seu caso individual, você deve recorrer ao estilo de procedimento mais complicado

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

> Esta seção contradiz as Diretrizes de Programação ABAP e as verificações do Inspetor de Código, que sugerem que tabelas grandes devem ser EXPORTADAS por referência para evitar déficits de desempenho. Falhamos consistentemente em reproduzir quaisquer déficits de desempenho e memória e recebemos um aviso sobre a otimização do kernel que geralmente melhora o desempenho do RETURNING, consulte [*Compartilhamento entre objetos de dados dinâmicos* na Ajuda da linguagem ABAP](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenmemory_consumption_3.htm) .
