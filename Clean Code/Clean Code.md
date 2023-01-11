# Clean Code



## Constantes

### Use constantes ao invés de hardcode

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

pois é mais claro que 

```ABAP
" Fora do padrão
IF abap_type = 'D'.
```

### Prefira classes de enumeração a interfaces constantes

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      gc_warning TYPE symsgty VALUE 'W',
      gc_error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

ou

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      gc_warning TYPE REF TO /clean/message_severity READ-ONLY,
      gc_error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

Ao em vez de misturar coisas não relacionadas ou enganar as pessoas para a conclusão de que coleções de constantes poderiam ser "implementadas":

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

### Se você não usa classes de enumeração, agrupe suas constantes 

Se você coletar constantes de forma solta, por exemplo em uma interface, agrupe-as:

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

O grupo também permite acesso em grupo, por exemplo, para validação de entrada:

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

### Prefira declarações inlinha a declarações antecipadas

```ABAP
METHOD do_something.
  DATA(lv_name) = 'something'.
  DATA(lv_reader) = /clean/reader=>get_instance_for( lv_name ).
  result = lv_reader->read_it( ).
ENDMETHOD.
```

do que declarar variáveis com uma `DATA` seção separada no início do método

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

### Não declare inline em ramificações opcionais

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

### Não encadeie declarações iniciais

```ABAP
DATA name TYPE seoclsname.
DATA reader TYPE REF TO reader.
```

O encadeamento sugere que as variáveis definidas estão relacionadas em um nível lógico. Para usá-lo de forma consistente, você teria que garantir que todas as variáveis encadeadas pertençam juntas e introduzir grupos de cadeia adicionais para adicionar variáveis. Embora isso seja possível, geralmente não vale o esforço.

O encadeamento também complica desnecessariamente a reformatação e a refatoração porque cada linha parece diferente e alterá-las requer a interferência de dois-pontos, pontos e vírgulas, que não valem o esforço.

```ABAP
" Fora do padrão
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO reader.
```



## Tabelas

### Use o tipo de tabela correto

- Normalmente, você usa `HASHED`tabelas para **tabelas grandes** que são **preenchidas em uma única etapa** , **nunca modificadas** e **lidas frequentemente por sua chave** . Sua memória inerente e sobrecarga de processamento tornam as tabelas de hash valiosas apenas para grandes quantidades de dados e muitos acessos de leitura. Cada mudança no conteúdo da tabela requer recálculo caro do hash, portanto, não use isso para tabelas que são modificadas com muita frequência.
- Normalmente, você usa `SORTED`tabelas para **tabelas grandes** que precisam ser **classificadas o tempo todo** , que são **preenchidas pouco a pouco** ou **precisam ser modificadas** e **lidas frequentemente por uma ou mais chaves completas ou parciais** ou processadas **em uma determinada ordem**. Adicionar, alterar ou remover conteúdo requer encontrar o ponto de inserção correto, mas não requer ajustar o restante do índice da tabela. As tabelas classificadas demonstram seu valor apenas para grandes números de acessos de leitura.
- Use `STANDARD`tabelas para tabelas **pequenas** , onde a indexação produz mais sobrecarga do que benefício, e **"arrays"** , onde você não se importa com a ordem das linhas ou deseja processá-las exatamente na ordem em que foram anexadas. Além disso, se for necessário um acesso diferente à tabela, por exemplo, acesso indexado e acesso classificado via `SORT`e `BINARY SEARCH`.

### Evite a CHAVE PADRÃO

```ABAP
" Fora do padrão
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

As chaves padrão geralmente são adicionadas apenas para fazer com que as instruções funcionais mais recentes funcionem. As próprias chaves, na verdade, geralmente são supérfluas e desperdiçam recursos por nada. Eles podem até levar a erros obscuros porque ignoram tipos de dados numéricos. As instruções `SORT`e `DELETE ADJACENT`sem lista de campos explícitos recorrerão à chave primária da tabela interna, que no caso de uso de `DEFAULT KEY`pode levar a resultados muito inesperados ao ter, por exemplo, campos numéricos como componentes da chave, em particular em combinação com `READ TABLE ... BINARY`etc.

Especifique os principais componentes explicitamente

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

ou recorra a `EMPTY KEY`ela se não precisar de uma chave.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```



### Prefira INSERT INTO TABLE a APPEND TO

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE`funciona com todos os tipos de tabela e chave, tornando mais fácil para você refatorar o tipo da tabela e as definições de chave se seus requisitos de desempenho mudarem.

Use `APPEND TO`apenas se você usar uma `STANDARD`tabela de maneira semelhante a uma matriz, se desejar enfatizar que a entrada adicionada deve ser a última linha.

### Prefira LINE_EXISTS a READ TABLE ou LOOP AT

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

expressa a intenção de forma mais clara e mais curta do que

```ABAP
" Fora do padrão
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

ou mesmo 

```ABAP
" Fora do padrão
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### Prefira READ TABLE a LOOP AT

```ABAP
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

expressa a intenção de forma mais clara e mais curta do que

```ABAP
" Fora do padrão
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

ou mesmo

```ABAP
" Fora do padrão
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Prefira LOOP AT WHERE a IF aninhado

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

expressa a intenção de forma mais clara e mais curta do que

```ABAP
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Evite leituras de tabelas desnecessárias

Caso você *espere* que uma linha esteja lá, leia uma vez e reaja à exceção,

```ABAP
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

em vez de desarrumar e desacelerar o fluxo de controle principal com uma leitura dupla

```ABAP
" Fora do padrão
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDIF.
DATA(row) = my_table[ key = input ].
```



## Strings

### Use ` para definir literais

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

### Usar | montar texto

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

Os modelos de string destacam melhor o que é literal e o que é variável, especialmente se você incorporar várias variáveis em um texto.

```ABAP
" Fora do padrão
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```



## Booleanos

### Use booleanos com sabedoria

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

```ABAP
DATA has_entries TYPE abap_bool.
```

Não use o tipo genérico `char1`. Embora seja tecnicamente compatível, obscurece o fato de estarmos lidando com uma variável booleana.

Evite também outros tipos booleanos, pois eles costumam ter efeitos colaterais estranhos, por exemplo, `boolean`suporta um terceiro valor "indefinido" que resulta em erros sutis de programação.

Em alguns casos, você pode precisar de um elemento de dicionário de dados, por exemplo, para campos DynPro. `abap_bool`não pode ser usado aqui porque é definido no tipo pool `abap`, não no dicionário de dados. Neste caso, recorra a `boole_d`ou `xfeld`. Crie seu próprio elemento de dados se precisar de uma descrição personalizada.

### Use ABAP_TRUE e ABAP_FALSE para comparações

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