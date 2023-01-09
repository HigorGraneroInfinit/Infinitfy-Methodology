# Clean Code







## Constantes

### Use constantes ao invés de hardcode

```{Abap}
IF abap_type = cl_abap_typedescr=>typekind_date.
```

pois é mais claro que 

```{abap}
" Fora do padrão
IF abap_type = 'D'.
```

### Prefira classes de enumeração a interfaces constantes

```{abap}
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      gc_warning TYPE symsgty VALUE 'W',
      gc_error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

ou

```{abap}
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      gc_warning TYPE REF TO /clean/message_severity READ-ONLY,
      gc_error   TYPE REF TO /clean/message_severity READ-ONLY.
  " ...
ENDCLASS.
```

Ao em vez de misturar coisas não relacionadas ou enganar as pessoas para a conclusão de que coleções de constantes poderiam ser "implementadas":

```{Abap}
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

```{abap}
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

```{abap}
" Fora do padrão
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

O grupo também permite acesso em grupo, por exemplo, para validação de entrada:

```{abap}
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

```{abap}
METHOD do_something.
  DATA(lv_name) = 'something'.
  DATA(lv_reader) = /clean/reader=>get_instance_for( lv_name ).
  result = lv_reader->read_it( ).
ENDMETHOD.
```

do que declarar variáveis com uma `DATA` seção separada no início do método

```{abap}
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

```{abap}
" Fora do padrão
IF has_entries = abap_true.
  DATA(lv_value) = 1.
ELSE.
  lv_value = 2.
ENDIF.
```

Isso funciona bem porque o ABAP lida com declarações inline como se estivessem no início do método. No entanto, é extremamente confuso para os leitores, especialmente se o método for mais longo e você não identificar a declaração imediatamente. Nesse caso, interrompa o inline e coloque a declaração na frente:

```{abap}
DATA lv_value TYPE i.
IF has_entries = abap_true.
  lv_value = 1.
ELSE.
  lv_value = 2.
ENDIF.
```

### Não encadeie declarações iniciais

```{abap}
DATA name TYPE seoclsname.
DATA reader TYPE REF TO reader.
```

O encadeamento sugere que as variáveis definidas estão relacionadas em um nível lógico. Para usá-lo de forma consistente, você teria que garantir que todas as variáveis encadeadas pertençam juntas e introduzir grupos de cadeia adicionais para adicionar variáveis. Embora isso seja possível, geralmente não vale o esforço.

O encadeamento também complica desnecessariamente a reformatação e a refatoração porque cada linha parece diferente e alterá-las requer a interferência de dois-pontos, pontos e vírgulas, que não valem o esforço.

```{abap}
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

### Prefira LINE_EXISTS a READ TABLE ou LOOP AT

```{abap}
IF line_exists( my_table[ key = 'A' ] ).
```

expressa a intenção de forma mais clara e mais curta do que

```{abap}
" Fora do padrão
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

ou mesmo 

```{abap}
" Fora do padrão
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### Prefira READ TABLE a LOOP AT

```{abap}
READ TABLE my_table REFERENCE INTO DATA(line) WITH KEY key = 'A'.
```

expressa a intenção de forma mais clara e mais curta do que

```{abap}
" Fora do padrão
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

ou mesmo

```{abap}
" Fora do padrão
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Prefira LOOP AT WHERE a IF aninhado

```{abap}
LOOP AT my_table REFERENCE INTO DATA(line) WHERE key = 'A'.
```

expressa a intenção de forma mais clara e mais curta do que

```{abap}
LOOP AT my_table REFERENCE INTO DATA(line).
  IF line->key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Evite leituras de tabelas desnecessárias

Caso você *espere* que uma linha esteja lá, leia uma vez e reaja à exceção,

```{abap}
TRY.
    DATA(row) = my_table[ key = input ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDTRY.
```

em vez de desarrumar e desacelerar o fluxo de controle principal com uma leitura dupla

```{abap}
" Fora do padrão
IF NOT line_exists( my_table[ key = input ] ).
  RAISE EXCEPTION NEW /clean/my_data_not_found( ).
ENDIF.
DATA(row) = my_table[ key = input ].
```