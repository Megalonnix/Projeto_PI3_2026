# **Tarefa Aula 03: Implementação – Índice Invertido + Stemming**

## FASE 1 - Testes feitos ANTES de usar o *Corpus Original*:

### 0. Pacotes + Corpus de teste:

Baixando pacote *SnowballC* para remoção de stopwords:

```r
if (!require(SnowballC)) install.package("SnowballC")
library(SnowballC)
```

```r
docs <- c(
  doc1 = "O gato comeu peixe",
  doc2 = "Os gatos comem peixe",
  doc3 = "O cachorro come carne"
)
```

### 1. Tokenização do texto + remoção de **stopwords**:

```r
processar_texto <- function(txt) {
    txt <- tolower(txt)
    txt <- gsub("[[:punct:]]", "", txt)
    tokens <- unlist(strsplit(txt, "\\s+"))
    tokens <- tokens[tokens != ""]
  
    stopwords_multilingual <- unique(c(
        stopwords::stopwords("pt", "snowball")))

    tokens <- tokens[
    !tokens %in% stopwords_multilingual]

    tokens <- wordStem(tokens, language = "portuguese")     # Separando RADICAL da palavra.
    return(tokens)
}
```

### 2. Código Índice Invertido + Resultados:

```r
postings <- list()

for (doc_id in names(docs)) {
  tokens <- processar_texto(docs[doc_id])
  tokens_unicos <- unique(tokens)
  for (tok in tokens_unicos) {
    postings[[tok]] <- c(postings[[tok]], doc_id) }
}

# Contagem de aparições por doc. de um >> PSEUDO-RADICAL <<:

print(postings[["gat"]])    # Retorna em quantos "docs"/unidades de texto 
print(postings[["peix"]])   # o PSEUDO-RADICAL "document" aparece.
print(postings[["com"]])
```

    === OUTPUT DO CÓDIGO ACIMA: ===

    [1] "doc1" "doc2"
    [1] "doc1" "doc2"
    [1] "doc1" "doc2" "doc3"

### 3. Buscas ((AND) , (OR)):
É realisada a busca do(s) pseudo-radical(is), verificando se ele está presente em *todos* os conjuntos de texto, ou apenas em *algum* deles.

```r
preparar_consulta <- function(texto) {
  processar_texto(texto)
}

buscar_AND <- function(consulta, indice = postings) {
  termos <- preparar_consulta(consulta)
  if (length(termos) == 0) return(character(0))
  
  listas <- indice[termos]
  listas <- listas[!sapply(listas, is.null)]
  if (length(listas) == 0) return(character(0))
  Reduce(intersect, listas)
}

buscar_OR <- function(consulta, indice = postings) {
  termos <- preparar_consulta(consulta)
  if (length(termos) == 0) return(character(0))
  
  listas <- indice[termos]
  listas <- listas[!sapply(listas, is.null)]
  if (length(listas) == 0) return(character(0))
  Reduce(union, listas)
}
```

```r
cat("Resultado_1:", buscar_AND(consulta = "peixe gato"),"\n") # 2 dos docs. possuem AMBOS os pseudo-radicais.
cat("Resultado_2:", buscar_OR(consulta = "gato comeu"))       # 3 documentos possuem ALGUM dos 2 pseudo-radicais.
```

    === OUTPUT DO CÓDIGO ACIMA: ===

    Resultado_1: doc1 doc2 
    Resultado_2: doc1 doc2 doc3

### 4. Tabela contabilizando frequência de índice por documento:

```r
tabela_indice <- data.frame(
  Radical = names(postings),                                     # 1ª coluna (pseudo-radical listado)
  Frequencia_Docs = lengths(postings),                           # 2ª coluna (quantos docs)
  Documentos = I(sapply(postings, paste, collapse = ", "))       # 3ª coluna (d1, d2...)
)

tabela_indice <- tabela_indice[order(tabela_indice$Radical), ]
print(tabela_indice)
```
    === OUTPUT DO CÓDIGO ACIMA: ===

            Radical Frequencia_Docs        Documentos
    cachorr cachorr               1              doc3
    carn       carn               1              doc3
    com         com               3  doc1, doc2, doc3
    gat         gat               2        doc1, doc2
    peix       peix               2        doc1, doc2

## FASE 2 - Resultados dos mesmos códigos, usando *Corpus Original*:

### 0. Corpus Original:

```r
docs <- c(
  d1 = "Recuperacao de Informacao: ORDENA documentos, por relevancia!",
  d2 = "O modelo de espaco-vetorial representa documentos (como vetores).",
  d3 = "BM25 e um modelo probabilistico de ranqueamento de texto.",
  d4 = "Aprendizado estatistico fundamenta a recuperacao moderna.",
  d5 = "O indice invertido acelera a busca em muitos documentos.",
  d6 = "Embeddings capturam a semantica de palavras e documentos.",
  d7 = "A avaliacao mede a relevancia dos resultados da busca.",
  d8 = "Ciencia de dados combina estatistica e programacao."
)
```

### 1. Código Índice Invertido + Resultados:

```r
print(postings[["recuperaca"]])    # Retorna em quantos "docs"/unidades de texto 
print(postings[["acel"]])          # o PSEUDO-RADICAL "document" aparece.
print(postings[["captur"]])
```

    === OUTPUT DO CÓDIGO ACIMA: ===

    [1] "d1" "d4"
    [1] "d5"
    [1] "d6"

### 2. Índice Invertido:

```r
cat("Resultado_1:", buscar_AND(consulta = "modelo probabilistico"),"\n") # 1 dos docs. possuem AMBOS os pseudo-radicais.
cat("Resultado_2:", buscar_OR(consulta = "modelo probabilistico"))       # 2 documentos possue ALGUM dos 2 pseudo-radicais.
```

    === OUTPUT DO CÓDIGO ACIMA: ===

    Resultado_1: d3 
    Resultado_2: d2 d3


### 3. Tabela contabilizando frequência de índice por documento:

**AVISO:** como haviam muitos pseudo-radicais, apenas os primeiros 5 foram mostrados!

```r
tabela_indice <- tabela_indice[order(tabela_indice$Radical), ]
View(head(tabela_indice, 5))
```
    === OUTPUT DO CÓDIGO ACIMA: ===

              Radical Frequencia_Docs     Documentos
    acel         acel               1             d5
    aprendiz aprendiz               1             d4
    avaliaca avaliaca               1             d7
    bm25         bm25               1             d3
    busc         busc               2         d5, d7
    captur     captur               1             d6
    cienc       cienc               1             d8
    combin     combin               1             d8
    dad           dad               1             d8
    document document               4 d1, d2, d5, d6

## FASE 3 - Resultados dos mesmos códigos, usando *documentos do web-scrapping*:

## 1. Ler dataframe vindo do scraping:

```R
# Caminho do arquivo
caminho <- "C:/Users/Ivan/Documents/Pasta-Documentos-PC-antigo/GITHUB-Meus-Repositorios/PesquisaPI3_2026/data/raw/noticias_santos.csv"

# Ler o CSV
noticias <- read.csv(caminho, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Pegar APENAS a coluna titulo
docs_noticias <- noticias$titulo
names(docs_noticias) <- paste0("n", 1:length(docs_noticias))

# Ver os 3 primeiros
head(docs_noticias, 3)
```

## 2. Código Índice Invertido + Resultados:

```r
postings_noticias <- list()
for (doc_id in names(docs_noticias)) {
  tokens <- processar_texto(docs_noticias[doc_id])
  tokens_unicos <- unique(tokens)
  for (tok in tokens_unicos) {
    postings_noticias[[tok]] <- c(postings_noticias[[tok]], doc_id) }
}

cat("Resultado_AND:", buscar_AND(consulta = "Santos", indice = postings_noticias), "\n")
cat("Resultado_OR:", buscar_OR(consulta = "morre ampliar", indice = postings_noticias))
```
    === OUTPUT CÓDIGO ACIMA: ===

    Resultado_AND: n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n15 n16 n17 n18 n19 n20 
    Resultado_OR: n2 n1

### 3. Tabela contabilizando frequência de índice por documento:

```r
tabela_noticias <- data.frame(
  Radical = names(postings_noticias),
  Frequencia_Docs = lengths(postings_noticias),
  Documentos = I(sapply(postings_noticias, paste, collapse = ", "))
)
tabela_noticias <- tabela_noticias[order(tabela_noticias$Radical), ]
View(tabela_noticias)
```

<table class="dataframe">
<caption>A data.frame: 144 × 3</caption>
<thead>
	<tr><th></th><th scope=col>Radical</th><th scope=col>Frequencia_Docs</th><th scope=col>Documentos</th></tr>
	<tr><th></th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;I&lt;chr&gt;&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>10</th><td>10       </td><td>1</td><td>n17     </td></tr>
	<tr><th scope=row>100</th><td>100      </td><td>2</td><td>n4, n8  </td></tr>
	<tr><th scope=row>2026</th><td>2026     </td><td>1</td><td>n9      </td></tr>
	<tr><th scope=row>2027</th><td>2027     </td><td>2</td><td>n3, n12 </td></tr>
	<tr><th scope=row>3</th><td>3        </td><td>1</td><td>n14     </td></tr>
	<tr><th scope=row>7</th><td>7        </td><td>1</td><td>n15     </td></tr>
	<tr><th scope=row>80</th><td>80       </td><td>1</td><td>n6      </td></tr>
	<tr><th scope=row>82</th><td>82       </td><td>1</td><td>n2      </td></tr>
	<tr><th scope=row>abre</th><td>abre     </td><td>2</td><td>n3, n20 </td></tr>
	<tr><th scope=row>acim</th><td>acim     </td><td>1</td><td>n14     </td></tr>
	<tr><th scope=row>alexandr</th><td>alexandr </td><td>1</td><td>n5      </td></tr>
	<tr><th scope=row>alme</th><td>alme     </td><td>1</td><td>n2      </td></tr>
	<tr><th scope=row>amarel</th><td>amarel   </td><td>1</td><td>n1      </td></tr>
	<tr><th scope=row>ampli</th><td>ampli    </td><td>1</td><td>n1      </td></tr>
	<tr><th scope=row>andré</th><td>andré    </td><td>1</td><td>n5      </td></tr>
	<tr><th scope=row>ano</th><td>ano      </td><td>1</td><td>n16     </td></tr>
	<tr><th scope=row>anos</th><td>anos     </td><td>2</td><td>n2, n17 </td></tr>
	<tr><th scope=row>após</th><td>após     </td><td>2</td><td>n16, n19</td></tr>
	<tr><th scope=row>aquári</th><td>aquári   </td><td>1</td><td>n16     </td></tr>
	<tr><th scope=row>áre</th><td>áre      </td><td>1</td><td>n15     </td></tr>
	<tr><th scope=row>artesanat</th><td>artesanat</td><td>1</td><td>n6      </td></tr>
	<tr><th scope=row>assist</th><td>assist   </td><td>1</td><td>n7      </td></tr>
	<tr><th scope=row>atend</th><td>atend    </td><td>1</td><td>n1      </td></tr>
	<tr><th scope=row>ating</th><td>ating    </td><td>1</td><td>n14     </td></tr>
	<tr><th scope=row>automát</th><td>automát  </td><td>1</td><td>n18     </td></tr>
	<tr><th scope=row>benefíci</th><td>benefíci </td><td>1</td><td>n8      </td></tr>
	<tr><th scope=row>busc</th><td>busc     </td><td>1</td><td>n1      </td></tr>
	<tr><th scope=row>cã</th><td>cã       </td><td>1</td><td>n7      </td></tr>
	<tr><th scope=row>cai</th><td>cai      </td><td>1</td><td>n19     </td></tr>
	<tr><th scope=row>candidat</th><td>candidat </td><td>1</td><td>n9      </td></tr>
	<tr><th scope=row>⋮</th><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><th scope=row>reforc</th><td>reforc   </td><td> 1</td><td>n15                                                                                 </td></tr>
	<tr><th scope=row>rend</th><td>rend     </td><td> 1</td><td>n20                                                                                 </td></tr>
	<tr><th scope=row>renov</th><td>renov    </td><td> 1</td><td>n4                                                                                  </td></tr>
	<tr><th scope=row>restaur</th><td>restaur  </td><td> 1</td><td>n2                                                                                  </td></tr>
	<tr><th scope=row>sabatin</th><td>sabatin  </td><td> 1</td><td>n9                                                                                  </td></tr>
	<tr><th scope=row>saib</th><td>saib     </td><td> 1</td><td>n8                                                                                  </td></tr>
	<tr><th scope=row>sant</th><td>sant     </td><td>19</td><td>n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n15, n16, n17, n18, n19, n20</td></tr>
	<tr><th scope=row>segu</th><td>segu     </td><td> 1</td><td>n18                                                                                 </td></tr>
	<tr><th scope=row>sen</th><td>sen      </td><td> 1</td><td>n9                                                                                  </td></tr>
	<tr><th scope=row>serrat</th><td>serrat   </td><td> 1</td><td>n13                                                                                 </td></tr>
	<tr><th scope=row>setembr</th><td>setembr  </td><td> 1</td><td>n1                                                                                  </td></tr>
	<tr><th scope=row>setor</th><td>setor    </td><td> 1</td><td>n20                                                                                 </td></tr>
	<tr><th scope=row>sigil</th><td>sigil    </td><td> 1</td><td>n5                                                                                  </td></tr>
	<tr><th scope=row>stf</th><td>stf      </td><td> 1</td><td>n5                                                                                  </td></tr>
	<tr><th scope=row>sul</th><td>sul      </td><td> 1</td><td>n14                                                                                 </td></tr>
	<tr><th scope=row>surd</th><td>surd     </td><td> 1</td><td>n11                                                                                 </td></tr>
	<tr><th scope=row>temát</th><td>temát    </td><td> 1</td><td>n4                                                                                  </td></tr>
	<tr><th scope=row>terap</th><td>terap    </td><td> 1</td><td>n7                                                                                  </td></tr>
	<tr><th scope=row>terçafeir</th><td>terçafeir</td><td> 1</td><td>n10                                                                                 </td></tr>
	<tr><th scope=row>tiláp</th><td>tiláp    </td><td> 1</td><td>n10                                                                                 </td></tr>
	<tr><th scope=row>tir</th><td>tir      </td><td> 1</td><td>n5                                                                                  </td></tr>
	<tr><th scope=row>trabalh</th><td>trabalh  </td><td> 1</td><td>n20                                                                                 </td></tr>
	<tr><th scope=row>vag</th><td>vag      </td><td> 1</td><td>n6                                                                                  </td></tr>
	<tr><th scope=row>vej</th><td>vej      </td><td> 4</td><td>n3, n6, n7, n18                                                                     </td></tr>
	<tr><th scope=row>vestibul</th><td>vestibul </td><td> 1</td><td>n17                                                                                 </td></tr>
	<tr><th scope=row>vis</th><td>vis      </td><td> 1</td><td>n20                                                                                 </td></tr>
	<tr><th scope=row>vlt</th><td>vlt      </td><td> 1</td><td>n18                                                                                 </td></tr>
	<tr><th scope=row>voluntári</th><td>voluntári</td><td> 1</td><td>n1                                                                                  </td></tr>
	<tr><th scope=row>vorcar</th><td>vorcar   </td><td> 1</td><td>n5                                                                                  </td></tr>
	<tr><th scope=row>vot</th><td>vot      </td><td> 1</td><td>n4                                                                                  </td></tr>
</tbody>
</table>