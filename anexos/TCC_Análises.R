if(!require('pacman'))install.packages('pacman')
pacotes <- c('dplyr','tidyverse','lubridate','gmodels','writexl', 'ggplot2', 'readr', 'lubridate', 'sjPlot', 'readxl')
pacman::p_load(pacotes, character.only = TRUE)

## Frequencia das variáveis no Viewer e impressão em PDF
view_df(BD_V4[, c(2:3,7,9,10:34,36:38,40:44,46,47:59,62,63,65:75)],  
        show.na = TRUE, 
        show.type = TRUE, 
        show.frq = TRUE, 
        show.prc = TRUE, 
        show.string.values = TRUE, 
        show.id = TRUE)

attach(BD_V4)

##Recategorização de variáveis 

BD_V4 <- BD_V4 %>% mutate(RENDA_B = case_when(
  is.na(Renda_Familiar) ~ NA_character_,
  BD_V4$Renda_Familiar == "Sem renda" ~ "Sem renda",
  BD_V4$Renda_Familiar== "Até um salário mínimo (R$ 1.039)" ~ "Até um salário mínimo",
  BD_V4$Renda_Familiar== "De 1 a 2 salários mínimos (R$ 1.040 a R$ 2.078)" ~ "De 1 a 2 salários mínimos",
  TRUE ~ "Mais de 2 salários mínimos"))

BD_V4 <- BD_V4 %>% mutate(NATURALIDADE_B = case_when(
  is.na(Naturalidade_Recategorizada) ~ NA_character_,
  BD_V4$Naturalidade_Recategorizada == "Sudeste" |  BD_V4$Naturalidade_Recategorizada == "Outros municípios de SP" ~ "Sudeste, fora de São Paulo",
  BD_V4$Naturalidade_Recategorizada== "São Paulo" ~ "São Paulo",
  BD_V4$Naturalidade_Recategorizada== "Nordeste" ~ "Nordeste",
  TRUE ~ "Outros"))

BD_V4 <- BD_V4 %>% mutate(ESCOLARIDADE_B = case_when(
  is.na(Escolaridade) | BD_V4$Escolaridade == "Não Declarado" ~ NA_character_,
  BD_V4$Escolaridade == "Não Alfabetizada(o)"  |  BD_V4$Escolaridade == "Fundamental Incompleto" |  BD_V4$Escolaridade == "Fundamental Completo" |  BD_V4$Escolaridade == "Médio Incompleto" ~ "Baixa",
  BD_V4$Escolaridade == "Médio Completo"  |  BD_V4$Escolaridade == "Ensino Técnico Incompleto" | "Ensino Técnico Completo"  |  BD_V4$Escolaridade == "Superior Incompleto" ~ "Média",
  BD_V4$Escolaridade == "Superior Completo"  |  BD_V4$Escolaridade == "Pós Graduação Incompleta" |  BD_V4$Escolaridade == "Pós Graduação" ~ "Alta"))


BD_V4 <- BD_V4 %>% mutate(ESCOLARIDADE_B = case_when(
  BD_V4$ESCOLARIDADE_B == "Baixa"  ~ "Baixa",
  BD_V4$ESCOLARIDADE_B == "Média baixa"  |  BD_V4$ESCOLARIDADE_B == "Média alta" ~ "Média",
  BD_V4$ESCOLARIDADE_B == "Alta"  ~ "Alta"))
  
table(BD_V4$Escolaridade)
table(BD_V4$ESCOLARIDADE_B)

BD_V4 <- BD_V4 %>% mutate(RELAÇÃO_TRABALHO_B = case_when(
  is.na(Relação_Trabalho) ~ NA_character_,
  BD_V4$Relação_Trabalho == "Não trabalha"  |  BD_V4$Relação_Trabalho == "Aposentado" |  BD_V4$Relação_Trabalho == "Desempregada(o)" ~ "Não exerce atividade remunerada",
  BD_V4$Relação_Trabalho == "Empregada(o) com carteira" ~ "Empregada(o) com carteira",
  BD_V4$Relação_Trabalho == "Autônomo" |  BD_V4$Relação_Trabalho == "Empregada(o) sem carteira" |  BD_V4$Relação_Trabalho == "Trabalho informal" ~ "Regimes informais",
  TRUE ~ "Outros"))

BD_V4 <- BD_V4 %>% mutate(AUTOR_B = case_when(
  is.na(Relação_com_Agressor) ~ NA_character_,
  sapply(str_split(Relação_com_Agressor, ";"), function(x) any(grepl("\\bEx-cônjuge ouex-Companheiro\\b", x))) ~ "Ex-cônjuge, ex-companheiro, ex-namorado",
  sapply(str_split(Relação_com_Agressor, ";"), function(x) any(grepl("\\bEx-namorado\\b", x))) ~ "Ex-cônjuge, ex-companheiro, ex-namorado",
  sapply(str_split(Relação_com_Agressor, ";"), function(x) any(grepl("\\bFamília\\b", x))) ~ "Família",
  sapply(str_split(Relação_com_Agressor, ";"), function(x) any(grepl("\\bFamiliar\\b", x))) ~ "Familiar",
  sapply(str_split(Relação_com_Agressor, ";"), function(x) any(grepl("\\bCônjuge ou Companheiro\\b", x))) ~ "Cônjuge, companheiro, namorado",
  sapply(str_split(Relação_com_Agressor, ";"), function(x) any(grepl("\\bNamorado\\b", x))) ~ "Cônjuge, companheiro, namorado",
  TRUE ~ "Outros"))

BD_V4 <- mutate(BD_V4,
                AUTOR_B = recode(AUTOR_B, "Família" = "Familiar"))

BD_V4 <- mutate(BD_V4,
                Filhos_Menores = recode(Filhos_Menores, "TRUE" = "Sim",
                                        "FALSE" = "Não"))
BD_V4$Filhos_Menores

table(BD_Cluster2$AUTOR_B)
#Reimportação de IDADE devido inconsistência de processamento

IDADE_V2 <- read_excel("IDADE_V2.xlsx")


BD_V4 <- left_join(BD_V4, IDADE_V2,
                   by = 'ID')

BD_V4 <- subset(BD_V4, select = -Idade)

##Clusterização


BD_Cluster <- BD_V4 %>% select(ID, 
                               NATURALIDADE_B,
                               `Raça/cor`,
                               ESCOLARIDADE_B,
                               RELAÇÃO_TRABALHO_B,
                               Estado_Civil,
                               Filhos_Menores,
                               RENDA_B, 
                               IDADE, 
                               AUTOR_B)

table(BD_Cluster2$NATURALIDADE_B)
table(BD_Cluster2$`Raça/cor`)
table(BD_Cluster2$ESCOLARIDADE_B)
table(BD_Cluster2$RELAÇÃO_TRABALHO_B)
table(BD_Cluster2$Filhos_Menores)
table(BD_Cluster2$RENDA_B)
table(BD_Cluster2$AUTOR_B)

BD_Cluster2 <- na.omit(BD_Cluster)

BD_Cluster2 <- BD_Cluster2 %>%
  mutate(`Raça/cor` = recode(`Raça/cor`, "Preta" = "Não Branca", 
                             "Parda" = "Não Branca", 
                             "Amarela" = "Não Branca",
                             "Indígena" = "Não Branca",
                             "Branca" = "Branca"))


install.packages("clustMixType")
library(clustMixType)

#Peparando as variaveis para a clusterização

BD_Cluster2$NATURALIDADE_B <- as.factor(BD_Cluster2$NATURALIDADE_B)
BD_Cluster2$ESCOLARIDADE_B <- as.factor(BD_Cluster2$ESCOLARIDADE_B)
BD_Cluster2$RELAÇÃO_TRABALHO_B <- as.factor(BD_Cluster2$RELAÇÃO_TRABALHO_B)
BD_Cluster2$Filhos_Menores <- as.factor(BD_Cluster2$Filhos_Menores)
BD_Cluster2$RENDA_B <- as.factor(BD_Cluster2$RENDA_B)
BD_Cluster2$Estado_Civil <- as.factor(BD_Cluster2$Estado_Civil)
BD_Cluster2$AUTOR_B <- as.factor(BD_Cluster2$AUTOR_B)
BD_Cluster2$IDADE <- as.numeric(BD_Cluster2$IDADE)

str(BD_Cluster2)

#Descobrindo o melhor valor de k

set.seed(7)

total_withinss <- c()

for(i in 1:8) {
  kproto <- clustMixType::kproto(BD_Cluster2 [ ,2:10],
                                 k = i,
                                 nstart = 25)
  total_withinss[i] <- kproto$tot.withinss
}



#plotando Elbow no gráfico


tibble(k = 1:length(total_withinss),
       total_error = total_withinss) %>% 
  ggplot(aes(x = k,
             y = total_error)) + 
  geom_point(size = 2) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Número de Clusters",
       y = "Total Withinss") + 
  geom_text(x = 3,
            y = total_withinss[3],
            label = "Elbow",
            alpha = 0.5,
            color = "blue",
            size = 5)


# Execução da Clusterização

set.seed(7)

Kprototypes <- BD_Cluster2 [ ,2:10] %>% 
  clustMixType::kproto(k = 3,
                       nstart = 25)

Kprototypes$size

summary(Kprototypes)

#Para adicionar os grupos no database

BD_Cluster2$Grupo <- Kprototypes$cluster

#Análise gráfica

BD_Cluster2 <- mutate(BD_Cluster2,
                Grupo = recode(Grupo,  "1"="Grupo 1",
                             "2"="Grupo 2",
                             "3"="Grupo 3"))

BD_Cluster2 <- mutate(BD_Cluster2,
                      RENDA_B = recode(RENDA_B, "Até um salário mínimo (R$ 1.039)" = "Até um salário mínimo",
                                       "De 1 a 2 salários mínimos (R$ 1.040 a R$ 2.078)" = "De 1 a 2 salários mínimos"))

ggplot(BD_Cluster2 , aes(x = Grupo, fill = `Raça/cor`)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..)*100, 0), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Grupos da clusterização", y = "Raça/cor", fill = "Raça/cor", 
       title = "Raça/cor por grupo") +
  scale_fill_brewer(palette = "Set1")


ggplot(BD_Cluster2 , aes(x = Grupo, fill = `Raça/cor`)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Raça/cor", fill = "Raça/cor", 
       title = "Raça/cor por grupo") +
  scale_fill_brewer(palette = "Set1")

ggplot(BD_Cluster2 , aes(x = Grupo, fill = ESCOLARIDADE_B)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Escolaridade", fill = "Escolaridade", 
       title = "Escolaridade por grupo") +
  scale_fill_brewer(palette = "Set1")

ggplot(BD_Cluster2 , aes(x = Grupo, fill = NATURALIDADE_B)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Naturalidade", fill = "Naturalidade", 
       title = "Naturalidade por grupo") +
  scale_fill_brewer(palette = "Set1")


ggplot(BD_Cluster2 , aes(x = Grupo, fill = RELAÇÃO_TRABALHO_B)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Relação de Trabalho", fill = "Relação de Trabalho", 
       title = "Relação de Trabalho por grupo") +
  scale_fill_brewer(palette = "Set1")

ggplot(BD_Cluster2 , aes(x = Grupo, fill = Filhos_Menores)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Menção a filhos e/ou dependentes menores", fill = "Menção a filhos e/ou dependentes menores", 
       title = "Menção a filhos e/ou dependentes menores") +
  scale_fill_brewer(palette = "Set1")


ggplot(BD_Cluster2 , aes(x = Grupo, fill =RENDA_B)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Renda Familiar", fill = "Renda Familiar", 
       title = "Renda Familiar por grupo") +
  scale_fill_brewer(palette = "Set1")

ggplot(BD_Cluster2 , aes(x = Grupo, fill =AUTOR_B)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Autor", fill = "Autor", 
       title = "Autor por grupo") +
  scale_fill_brewer(palette = "Set1")

ggplot(BD_Cluster2 , aes(x = Grupo, y = IDADE, fill =Grupo)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Grupo", y = "Idade", fill = "Grupo", 
       title = "Idade por grupo")

ggplot(BD_Cluster2 , aes(x = Grupo, fill =Estado_Civil)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Estado Civil", fill = "Estado Civil", 
       title = "Estado Civil por grupo") +
  scale_fill_brewer(palette = "Set1")

## Teste de correlações dentro de cada grupo 

Grupo1 <- BD_Cluster2 %>% filter(BD_Cluster2$Grupo == "Grupo 1")
Grupo2 <- BD_Cluster2 %>% filter(BD_Cluster2$Grupo == "Grupo 2")
Grupo3 <- BD_Cluster2 %>% filter(BD_Cluster2$Grupo == "Grupo 3")

## Correlação de Naturalidade em cada grupo 

contingency_table_1 <- table(Grupo1$NATURALIDADE_B, Grupo1$Grupo)
print(contingency_table_1)
cont_coeff <- sqrt(chisq.test(contingency_table_1)$statistic /
                     (sum(contingency_table_1) + chisq.test(contingency_table_1)$statistic))
print(paste("Coeficiente de Contingência:", cont_coeff))

contingency_table_1 <- table(Grupo2$NATURALIDADE_B, Grupo2$Grupo)
print(contingency_table_1)
cont_coeff <- sqrt(chisq.test(contingency_table_1)$statistic /
                     (sum(contingency_table_1) + chisq.test(contingency_table_1)$statistic))
print(paste("Coeficiente de Contingência:", cont_coeff))

contingency_table_1 <- table(Grupo3$NATURALIDADE_B, Grupo3$Grupo)
print(contingency_table_1)
cont_coeff <- sqrt(chisq.test(contingency_table_1)$statistic /
                     (sum(contingency_table_1) + chisq.test(contingency_table_1)$statistic))
print(paste("Coeficiente de Contingência:", cont_coeff))


#Compreendendo intersecção com outras variáveis que 
#não entraram na clusterização por conta dos missining values

table(BD_Cluster2$`Raça/cor`)


BD_V5 <- subset(BD_V4, select = c(1,7,8,12,13,15,20,48,49,50,61,57,62:66,68:71,74))


Clusters_Análise_detalhada <- left_join(BD_Cluster2, BD_V5,
                           by = 'ID')
 
Grupo1_Detalhado <- Clusters_Análise_detalhada %>% filter(Clusters_Análise_detalhada$Grupo == "Grupo 1")
Grupo2_Detalhado <- Clusters_Análise_detalhada %>% filter(Clusters_Análise_detalhada$Grupo == "Grupo 2") 
Grupo3_Detalhado <- Clusters_Análise_detalhada %>% filter(Clusters_Análise_detalhada$Grupo == "Grupo 3") 


view_df(Grupo1_Detalhado[, c(1:32)],  
        show.na = TRUE, 
        show.type = TRUE, 
        show.frq = TRUE, 
        show.prc = TRUE, 
        show.string.values = TRUE, 
        show.id = TRUE)

view_df(Grupo2_Detalhado[, c(1:32)],  
        show.na = TRUE, 
        show.type = TRUE, 
        show.frq = TRUE, 
        show.prc = TRUE, 
        show.string.values = TRUE, 
        show.id = TRUE)


view_df(Grupo3_Detalhado[, c(1:32)],  
        show.na = TRUE, 
        show.type = TRUE, 
        show.frq = TRUE, 
        show.prc = TRUE, 
        show.string.values = TRUE, 
        show.id = TRUE)


summary(Grupo1_Detalhado$Tempo_Vinculada)
summary(Grupo2_Detalhado$Tempo_Vinculada)
summary(Grupo3_Detalhado$Tempo_Vinculada)

summary(Grupo1_Detalhado$QUANT_VIOLÊNCIAS)
summary(Grupo2_Detalhado$QUANT_VIOLÊNCIAS)
summary(Grupo3_Detalhado$QUANT_VIOLÊNCIAS)

summary(Grupo1_Detalhado$Tempo_Relacionamento)
summary(Grupo2_Detalhado$Tempo_Relacionamento)
summary(Grupo3_Detalhado$Tempo_Relacionamento)

summary(Grupo1_Detalhado$IDADE)
summary(Grupo2_Detalhado$IDADE)
summary(Grupo3_Detalhado$IDADE)

table(BD_Cluster2$Estado_Civil)

ggplot(Clusters_Análise_detalhada , aes(x = Grupo, y = Tempo_Vinculada, fill =Grupo)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Grupo", y = "Tempo vinculada", fill = "Grupo", 
       title = "Tempo vinculada por grupo")



#Pista da regressão logística

ggplot(Clusters_Análise_detalhada , aes(x = Grupo, fill = REGRESSAO_MULTINOMIAL)) +
  geom_bar(position = "dodge") +
  labs(x = "Grupos da clusterização", y = "Resultados do atendimento", fill = "Resultados do atendimento", 
       title = "Resultado do atendimento por grupo") +
  scale_fill_brewer(palette = "Set1")

write.csv(Clusters_Análise_detalhada, "BD_cluster_final.csv")
write.csv(BD_V5_regressao, "BD_V5_regressao.csv")

## Parte a ser executada no Python..........................................


