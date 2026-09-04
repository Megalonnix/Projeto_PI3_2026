## **Implementação – Índice Invertido + Stemming**

---

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

### Código Índice Invertido + Resultados:

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

    === OUTPUT DO CÓDIGO: ===

    [1] "doc1" "doc2"
    [1] "doc1" "doc2"
    [1] "doc1" "doc2" "doc3"