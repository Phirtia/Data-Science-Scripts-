install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("tibble")
install.packages("wordcloud")
install.packages("stringr")
install.packages("SnowballC")


library("dplyr")
library("tidytext")
library("ggplot2")
library("tibble")
library("wordcloud")
library("stringr")
library("SnowballC")

## Separa os textos em tokens

DB <-TEXT_MINING
Dados_unnested <- DB %>% unnest_tokens(word, NARRATIVAS)

## Retira os acentos demais caracteres

for (i in 1:nrow(Dados_unnested))
{
  Dados_unnested$value[i] <- iconv(Dados_unnested$value[i], to = "ASCII//TRANSLIT")
}

## Retira palavras acessórias como preposições e conjunções

Violacoes <- Dados_unnested %>% anti_join(get_stopwords(language = 'pt'))

##Frequencia das palavras

Violacoes_cont <- Violacoes %>% select(word) %>% count(word, sort = TRUE)

install.packages("writexl")
library(writexl)
write_xlsx(Violacoes_cont,"Contagem_Violacoes.xlsx")

Viol_cont <- Contagem_Violencias

attach(Contagem_apenasviolencia)

##Word Cloud

pal <- brewer.pal(6, "YlOrRd")
Contagem_apenasviolencia %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

pal <- brewer.pal(6, "Dark2")
Contagem_Violacoes_V2 %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))


