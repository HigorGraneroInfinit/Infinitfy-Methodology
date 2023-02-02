# Nomenclatura de Código

Os desenvolvimentos feitos pela Infinitfy, devem seguir as seguintes nomenclaturas, caso o cliente não tenha nomenclatura preestabelecida ou exigida. 

## Conteúdo

- [Legendas](#Legendas)
- [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) 
  - [Pacote](#Pacote)
  - [Dicionário de dados](#Dicionário-de-dados)
    - [Tabela](#Tabela)
    - [Elemento de dados](#Elemento-de-dados)
    - [Domínio](#Domínio)
    - [Estrutura](#Estrutura)
    - [Tipo de tabela](#Tipo-de-tabela)
    - [View](#View)
    - [Ajuda de pesquisa](#Ajuda-de-pesquisa)
  - [Programas](#Programas)
    - [Report e Module Pool](#Report-e-Module-Pool)
    - [Include](#Include ) 
  - [Grupo de Função](#Grupo-de-Função)
  - [Módulo de Função](#Módulo-de-Função)
  - [Adobe Forms Formulário](#Adobe-Forms-Formulário)
  - [Adobe Forms Interface](#Adobe-Forms-Interface)
  - [Adobe Interactive Forms](#Adobe-Interactive-Forms)
  - [Smartforms Formulário](#Smartforms-Formulário)
  - [Smartforms Estilo](#Smartforms-Estilo)
  - [Sapscript](#Sapscript)
  - [Classe Orientação a Objeto](#Classe-Orientação-a-Objeto)
- [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento)
  - [Objetos Globais](#Objetos-Globais)
  - [Objetos Locais](#Objetos-Locais)
  - [Elemento de Texto](#Elemento-de-Texto)
  - [Perform](#Perform)
  - [Module PBO & PAI](#Module-PBO--PAI)
  - [Projeto user-exit](#Projeto-user-exit)
  - [Transação](#Transação)
  - [Intervalo de Numeração](#Intervalo-de-Numeração)
  - [Interface](#Interface)
  - [Badi](#Badi)
  - [Enhancement](#Enhancement)
  - [Classe de Mensagem](#Classe-de-Mensagem)
  - [Imagem](#Imagem)
  - [Authority Check](#Authority-Check)
- [Comentários em Programas](#Comentários-em-Programas)
  - [Informações Gerais](#Informações-Gerais)
  - [Cabeçalho](#Cabeçalho)
  - [Comentário em Bloco](#Comentário-em-Bloco)
  - [Comentário em Linha](#Comentário-em-Linha)




## Legendas

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Seção atual](#Legendas)

Na informação de cada padrão, considere para todos os casos a tabela de cores a seguir: 

| **Cor**                                       | **Significado**                                |
| --------------------------------------------- | ---------------------------------------------- |
| <span style="color:red">**Vermelho**</span>   | Valores Fixos                                  |
| <span style="color:blue">**Azul**</span>      | Identificação do módulo                        |
| <span style="color:green">**Verde**</span>    | Descrição de uso livre do consultor            |
| <span style="color:orange">**Laranja**</span> | Uso específico, será identificado em cada caso |

## Nomenclatura de Objetos

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Seção atual](#Nomenclatura-de-Objetos)

### Pacote

>[Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Pacote)

Transação: SE21, SE80 

Tamanho máximo: 30 caracteres 

Padrão geral: <span style="color:red">**ZBR**</span><span style="color:blue">**XXXX**</span>

##### Onde:  

- <span style="color:red">**ZBR**</span> - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:blue">**XXXX**</span>- Módulo

### Dicionário de dados

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Dicionário-de-dados)

#### Tabela

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Dicionário de dados](#Dicionário-de-dados) > [Seção atual](#Tabela)

Tamanho máximo: 16 caracteres 

Padrão geral: <span style="color:red">**ZTB_**</span> <span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZTB_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

#### Elemento de dados

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Dicionário de dados](#Dicionário-de-dados) > [Seção atual](#Elemento-de-dados)

Tamanho máximo: 30 caracteres 

Padrão geral: <span style="color:red">**ZDE_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZDE_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

#### **Domínio** 

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Dicionário de dados](#Dicionário-de-dados) > [Seção atual](#Domínio)

Tamanho máximo: 30 caracteres 

Padrão geral: <span style="color:red">**ZDM_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZDM_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

#### Estrutura  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Dicionário de dados](#Dicionário-de-dados) > [Seção atual](#Estrutura)

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZST_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZST_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

#### Tipo de tabela  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Dicionário de dados](#Dicionário-de-dados) > [Seção atual](#Tipo-de-tabela)

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZTT_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZTT_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

#### **View** 

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Dicionário de dados](#Dicionário-de-dados) > [Seção atual](#View )

Tamanho máximo: 16 caracteres 

Padrão geral: <span style="color:red">**ZVW_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZVW_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

#### Ajuda de pesquisa  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Dicionário de dados](#Dicionário-de-dados) > [Seção atual](#Ajuda-de-pesquisa )

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZSH_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZSH_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

### **Programas**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Programas)

#### **Report e Module Pool** 

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Programas](#Programas) > [Seção atual](#Report-e-Module-Pool) 

Transação: SE38, SE80  

Tamanho máximo: 40 caracteres  

Padrão geral: <span style="color:red">**ZPR_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZPR_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

#### Include  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Programas](#Programas) > [Seção atual](#Include) 

Transação: SE38  

Tamanho máximo: 40 caracteres  

Padrão geral: <span style="color:red">**ZINC_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZINC_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

**OBS**.: A exceção se aplica aos programas Include que são criados automaticamente pelo SAP.  

### Grupo de Função

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Grupo de Função)

Transação: SE37  

Tamanho máximo: 26 caracteres  

Padrão geral: <span style="color:red">**ZFG_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZFG_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

**OBS**.: Utilize, sempre que possível, um Grupo de Função para cada Módulo de Função.  

### Módulo de Função  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Módulo-de-Função)

Transação: SE37  

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZFM_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZFM_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

**OBS**.: Utilize, sempre que possível, um Grupo de Função para cada Módulo de Função, utilizando em ambos o mesmo valor colocado em LIVRE.  

### Adobe Forms Formulário  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Adobe-Forms-Formulário)

Transação: SFP  

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZAF_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZAF_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

### Adobe Forms Interface  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Adobe-Forms-Interface)

Transação: SFP  

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZAI_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZAI_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

### Adobe Interactive Forms  

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Adobe-Interactive-Forms)

Transação: SFP  

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZIF_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZIF_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

### Smartforms Formulário

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Smartforms-Formulário)

Transação: SMARTFORMS  

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZSF_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZSF_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

### Smartforms Estilo

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Smartforms-Estilo)

Transação: SMARTFORMS, SMARTSTYLES  

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZSE_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZSE_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

### Sapscript

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Sapscript)

Transação: SE71  

Tamanho máximo: 30 caracteres  

Padrão geral: <span style="color:red">**ZSS_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZSS_**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

### Classe Orientação a Objeto

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura de Objetos](#Nomenclatura-de-Objetos) > [Seção atual](#Classe-Orientação-a-Objeto)

Transação: SE24  

Tamanho máximo: 30 caracteres  

Padrão geral: ZCLXXXX_LIVRE  

Padrão geral: <span style="color:red">**ZCL**</span><span style="color:blue">**XXXX_**</span><span style="color:green">**LIVRE**</span>

##### Onde:  

- <span style="color:red">**ZCL**</span>  - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3
- <span style="color:blue">**XXXX_**</span> - Módulo
- <span style="color:green">**LIVRE**</span> - Descrição de uso livre

**OBS**.: Para a criação de Classe local, utilize o padrão: <span style="color:red">**ZLC_**</span><span style="color:green">**LIVRE**</span>.



## Nomenclatura Desenvolvimento

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Seção atual](#Nomenclatura-Desenvolvimento)

### **Objetos Globais**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Objetos-Globais)

| **Tipo**       | **Iniciar Com** |
| :------------- | :-------------- |
| Variáveis      | gv_             |
| Constantes     | gc_             |
| Tabela Interna | it_             |
| Workarea       | wa_             |
| Range          | gr_             |
| Field Symbol   | <fs_>           |
| Types          | ty_             |
| Estruturas     | st_             |
| Parameters     | p_              |
| Select-Options | s_              |
| Checkbox       | cb_             |
| Radio-button   | rb_             |
| Objetos OO     | go_             |

### **Objetos Locais**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Objetos-Locais)

| **Tipo**       | **Iniciar Com** |
| -------------- | --------------- |
| Variáveis      | lv_             |
| Constantes     | lc_             |
| Tabela Interna | lt_             |
| Workarea       | lw_             |
| Range          | lr_             |
| Field Symbol   | <lfs_>          |
| Types          | ly_             |
| Estruturas     | ls_             |
| Classe         | lcl_            |
| Objetos OO     | lo_             |

### **Elemento de Texto**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Elemento-de-Texto)

Não deixar textos fixos dentro do programa, sempre utilizar os ‘Símbolos de texto’ em ‘Elementos de texto’. Para esse contexto, utilize o padrão: <span style="color:green">**XXXXXXXXXXXX**</span> (<span style="color:blue">**YYY**</span>).

##### Onde: 

-  <span style="color:green">**XXXXXXXXXXXX**</span> - Literal com o conteúdo do texto. 
-  <span style="color:blue">**YYY**</span> - Identificação de 3 caracteres para o texto. Com duplo clique, o texto é inserido

##### Exemplo:

```ABAP
REPORT zns_001.
DATA: vg_texto TYPE c LENGTH 100.
vg_texto = 'Olá &1, hoje é &2.'(001).
REPLACE ALL OCCURRENCES OF '&1' IN vg_texto with sy-uname.
REPLACE ALL OCCURRENCES OF '&2' IN vg_texto with sy-datum.
MESSAGE vg_texto
	TYPE 'I'.
```

### **Perform**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Perform)

Tamanho máximo: 30 caracteres 
Padrão geral:<span style="color:red"> **ZF_**</span><span style="color:green">**LIVRE**  </span> 

##### Onde:

- **ZF** - Valor fixo.
- **_**  - Valor fixo (underline), separador.
- **Livre** -  Descrição de uso livre do consultor.

##### **OBS**:

- Sempre que possível, ao criar um Perform, crie um parâmetro do tipo CHANGING chamado P_ERRO que servirá para validar se houve algum erro na execução desse Perform. Inicie o processamento desse Perform limpando o parâmetro P_ERRO e caso haja algum erro, atribuir um '**X**' à esse parâmetro. Não esquecer de fazer a verificação desse parâmetro P_ERRO após a execução do Perform.

### **Module PBO & PAI**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Module-PBO--PAI)

Tamanho máximo: 30 caracteres 
Padrão geral: <span style="color:ORANGE">**YYY**</span><span style="color:green">**LIVRE**  </span>

##### Onde:

- **YYY** - Código que identifica de onde é o Module: PBO ou PAI. 
- **_** - Valor fixo (underline), separador.
- **Livre** - Descrição livre para identificação do Module. Cerca de 25 caracteres. 

### **Projeto user-exit**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Projeto-user-exit)

Transação: CMOD
Tamanho máximo: 8 caracteres
Padrão geral: <span style="color:red"> **Z**</span><span style="color:blue">**XXXX**</span><span style="color:green">**LIVRE**  </span> 

##### Onde:

- **Z** - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3.

- **XX** - Módulo.

- **_** - Valor fixo (underline), separador.

- **Livre** - Descrição de uso livre do consultor.

### **Transação**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Transação)

Transação: SE93
Tamanho máximo: 20 caracteres
Padrão geral: <span style="color:red"> **Z**</span><span style="color:blue">**XXXX**</span><span style="color:green">**LIVRE**  </span> 

##### Onde:

- **Z** - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3.
- **XX** - Módulo.
- **Livre** -  Descrição de uso livre do consultor.

### **Intervalo de Numeração**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Intervalo-de-Numeração)

Transação: SRNO
Tamanho máximo: 10 caracteres
Padrão geral: <span style="color:red"> **ZNR**</span><span style="color:green">**LIVRE**  </span>

##### Onde:

- **ZNR** - Valor fixo.
- **Livre** - Descrição de uso livre do consultor.

### **Interface**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Interface)

Transação: SE24
Tamanho máximo: 30 caracteres
Padrão geral: <span style="color:red"> **ZINT_**</span><span style="color:blue">**XXXX**</span><span style="color:red"> **_**</span><span style="color:green">**LIVRE**  </span>

##### Onde:

- **ZINT** - Valor fixo.

- **XX** - Módulo.

- **Livre** - Descrição de uso livre do consultor.

### **Badi**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Badi)

Transação: SE18, SE19
Tamanho máximo: 30 caracteres
Padrão geral: <span style="color:red"> **Z**</span><span style="color:ORANGE">**YYYY**</span>

##### Onde:

-  **Z** - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3.

-  _ - Valor fixo (underline), separador.

-  **YYYY** - Nome da BADI original (standard).

### **Enhancement**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Enhancement)

Transação: SE20
Tamanho máximo: 30 caracteres
Padrão geral:  <span style="color:red"> **ZENHP_**</span><span style="color:blue">**XXXX**</span><span style="color:red"> **_**</span><span style="color:green">**LIVRE**  </span>

##### Onde:

- **ZENHP** - Valor fixo.
- **XX** - Módulo.
- **Livre** - Descrição de uso livre do consultor.

**IMPORTANTE:** Para todo Enhancement deve-se criar um programa Include que conterá toda a lógica.

### **Classe de Mensagem**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Classe-de-Mensagem)

Transação: SE91
Tamanho máximo: 20 caracteres
Padrão geral:<span style="color:red"> **ZCM_**</span><span style="color:blue">**XXXX**</span>

##### Onde:

- **ZENHP** - Valor fixo.
- **XX** - Módulo.

**IMPORTANTE:** Para toda Classe de mensagem criada, sempre incluir a mensagem número 000 com “& & & &”, ou seja, a primeira mensagem de código 000 poderá ser usada de forma genérica com até 4 parâmetros.

### **Imagem**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Imagem)

Transação: SE78
Tamanho máximo: 70 caracteres
Padrão geral: <span style="color:red"> **ZIMG**</span><span style="color:green">**LIVRE**  </span>

##### Onde:

- **ZIMG** - Valor fixo.
- **Livre** - Descrição de uso livre do consultor.

### **Authority Check**

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Nomenclatura Desenvolvimento](#Nomenclatura-Desenvolvimento) > [Seção atual](#Authority-Check)

Transação: SU21 Tamanho
Tamanho máximo: 70 caracteres
Padrão geral: <span style="color:red"> **Z**</span><span style="color:blue">**XXXX**</span><span style="color:green">**LIVRE**  </span>

- **Z** - Valor fixo, constante que identifica que o objeto não pertence ao standard do R/3.
- **XX** - Módulo.
- **Livre** - Descrição de uso livre do consultor.

 Comentários em Programas

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Seção atual](#Comentários-em-Programas)

### Informações Gerais

>[Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Comentários em Programas](#Comentários-em-Programas) > [Seção atual](#Informações-Gerais)

Em todos os comentários realizados nos programas, não utilize o nome do cliente.  

Fazer uso de comentários dentro dos programas para considerações realmente relevantes para o entendimento do código e lógica criados. 

Exemplo: 

```ABAP
*&---------------------------------------------------------------------* 
*&  Function           ZFM_CONSUME_BIGDATA 
*&---------------------------------------------------------------------* 
*& Nome: ZFM_CONSUME_BIGDATA 
*& Tipo: Function - RFC 
*& Objetivo: Consumo de BIGDATA 
*& Data/Hora: Thursday, July 28, 2021 (GMT-3) - 02:59 
*& Desenvolvedor: Higor Lopes(Burger King) 
*&---------------------------------------------------------------------* 
*& Versão 1: Higor Lopes(Burger King) - Inicio Desenvolvimento - BKDK984103 
*& Versão 2: ? 
*& Versão 3: ? 
*&---------------------------------------------------------------------* 

TABLES: /xnfe/innfehd. 

TYPES: 
*&---------------------------------------------------------------------* 
*&  GLOBAL TYPES(TY_) 
*&---------------------------------------------------------------------* 
  BEGIN OF ty_bkoffice, 
    nfeid TYPE c LENGTH 44, 
    check TYPE c LENGTH 1, 
  END OF ty_bkoffice, 
  BEGIN OF ty_innfeit, 
    guid_header TYPE /xnfe/innfeit-guid_header, 
    cfop        TYPE /xnfe/innfeit-cfop, 
  END OF ty_innfeit. 
  
DATA: 
*&---------------------------------------------------------------------* 
*&  GLOBAL INTERNAL TABLES(IT_) 
*&---------------------------------------------------------------------* 
it_innfehd TYPE STANDARD TABLE OF /xnfe/innfehd, 
it_innfeit TYPE STANDARD TABLE OF ty_innfeit, 
  
*&---------------------------------------------------------------------* 
*&  GLOBAL VARIABLES(VG_) 
*&---------------------------------------------------------------------* 
vg_status TYPE c LENGTH 1, 

*&---------------------------------------------------------------------* 
*&  GLOBAL OBJECT(OO_) 
*&---------------------------------------------------------------------* 
oo_bkoffice TYPE REF TO zbko_cl_bkoffice. 

*&---------------------------------------------------------------------* 
*&  GLOBAL CONSTANTS(C_) 
*&---------------------------------------------------------------------* 
CONSTANTS:  
  BEGIN OF c_bkoxml, 
    pend TYPE c LENGTH 1 VALUE 'P',   " XML não existe na base do BKOffice 
    succ TYPE c LENGTH 1 VALUE 'S',   " XML encontrado na base do BKOffice 
    erro TYPE c LENGTH 1 VALUE 'E',   " Erro na consulta de Status Integração XML 
    nrel TYPE c LENGTH 1 VALUE 'N',   " XML não relevante para BKOffice. 
  END OF c_bkoxml. 
```



### Cabeçalho

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Comentários em Programas](#Comentários-em-Programas) > [Seção atual](#Cabeçalho)

Utilizar esse padrão abaixo para os objetos do tipo: Programas Report, Include, Module Pool, Módulo de 

Função, BADI e Classe. 

```ABAP
*&---------------------------------------------------------------------* 
*& Nome: ZBKO_P_JOB_STATUS_XML_PLK 
*& Tipo: Report 
*& Objetivo: Integrar BKOffice 
*& Data/Hora: Thursday, July 8, 2021 (GMT-3) - 02:59 
*& Desenvolvedor: Higor Lopes(Burger King) 
*& Request: PD1K900537 
*&---------------------------------------------------------------------* 
*&                            VERSÕES 
*&---------------------------------------------------------------------* 
*& Request: ? 
*& Motivo/Correção: ? 
*& Nome: ? 
*&---------------------------------------------------------------------* 
```

### Comentário em Bloco

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Comentários em Programas](#Comentários-em-Programas) > [Seção atual](#Comentário-em-Bloco)

Para grandes trechos de modificações, utilizar os identificadores abaixo para destacar o início (BEGIN) e fim (END) do bloco modificado: 

```ABAP
* --->>> BEGIN >>> changed by Usuário(NomeDev.) in Data - v1[Chamado] - BKDK983899 

* <<<--- END <<< changed by Usuário(NomeDev.) in Data - v1[Chamado] - BKDK983899 
```

Exemplo:

```ABAP
* --->>> BEGIN >>> changed by HLOPES(Higor) in Data - v1[C0016 - BKDK983899 

* <<<--- END <<< changed by HLOPES(Higor) in Data - v1[C0016] - BKDK983899 
```

### Comentário em Linha

> [Nomenclatura de Código](#Nomenclatura-de-Código) > [Conteúdo](#Conteúdo) > [Comentários em Programas](#Comentários-em-Programas) > [Seção atual](#Comentário-em-Linha)

Para identificar individualmente as linhas que foram modificadas, use os padrões abaixo para cada caso: 

##### Linhas incluídas (ADD):  <span style="color:red">**"add**</span> **DD.MM.AAAA** – <span style="color:red">**v**</span>**XX** 

```ABAP
DATA: lv_addrn type adrc-ADDRNUMBER,
	  lv_matnr TYPE mara-matnr, "add 06.01.2023 - v02
	  lv_kunnr TYPE kna1-kunnr. "add 06.01.2023 - v02
```

##### Linhas excluídas (DEL): <span style="color:red">**"del**</span> **DD.MM.AAAA** – <span style="color:red">**v**</span>**XX** 

```ABAP
DATA: lv_addrn type adrc-ADDRNUMBER, "del 09.01.2023 - v03
	  lv_matnr TYPE mara-matnr, "add 06.01.2023 - v02 "del 09.01.2023 - v03
	  lv_kunnr TYPE kna1-kunnr. "add 06.01.2023 - v02
```

##### Onde: 

- <span style="color:red">**"**</span> - Valor fixo, identifica que é um comentário no final da linha. 
- <span style="color:red">**Add**</span> - Valor fixo, identifica que é uma linha que foi adicionada. 
- <span style="color:red">**Del**</span> - Valor fixo, identifica que é uma linha que foi excluída. 
- **dd.mm.aaaa** - Data, mês e ano da inclusão. 
- -- Valor fixo, separador. 
- v - Valor fixo, letra V, identificador para versão. 
- **XX** - Identifica o número da versão colocada no cabeçalho do programa. 

**Linhas modificadas:** copiar a linha que será alterada (CTRL+D). Colocar comentário com "DEL" no final da linha e na linha copiada (depois de alterada) colocar comentário "ADD". Desabilitar (comentar) a linha inteira que ficou como "DEL". 
