# **Tarefa Aula 03: Implementação – Índice Invertido + Stemming**

## 0. Pacotes + Corpus de teste:

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

## 1. Tokenização do texto + remoção de **stopwords**:

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

## 2. Código Índice Invertido + Resultados:

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

## 3. Buscas ((AND) , (OR)):
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

## 

```R
tabela_indice <- data.frame(
  Radical = names(postings),                                     # 1ª coluna (pseudo-radical listado)
  Frequencia_Docs = lengths(postings),                           # 2ª coluna (quantos docs)
  Documentos = I(sapply(postings, paste, collapse = ", "))       # 3ª coluna (d1, d2...)
)

tabela_indice <- tabela_indice[order(tabela_indice$Radical), ]
print(tabela_indice)
```

            Radical Frequencia_Docs       Documentos
    cachorr cachorr               1             doc3
    carn       carn               1             doc3
    com         com               3 doc1, doc2, doc3
    gat         gat               2       doc1, doc2
    peix       peix               2       doc1, doc2