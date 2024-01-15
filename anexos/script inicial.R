library(readr)  #Ler pacote
FU_V1 <- read_csv("FU_V1.csv")  #importar database

View(FU_V1) #visualizar database

install.packages("dplyr")  #A maior parte das funções estão no dplyr
library('dplyr')

install.packages("lubridate") #Para usar a função dmy
library('lubridate')

#Para filtrar por ano. Neste caso, 2020 a 2023

FU_V1 <- mutate(FU_V1, Data_reprocessada = dmy(FU_V1$`Data do atendimento`), ) #Colocar a data em formato lido pelo R


FU_V1 <- FU_V1 %>% mutate(ano = year(FU_V1$Data_reprocessada)) #Criar variável ANO


FU_V1 <- FU_V1 %>% filter(ano>=2020 & ano<=2023)  #Filtrar pelos anos de interesse


#Inclusão de variável IDADE


FU_V1 <- mutate(FU_V1, Data_nasc_V2 = dmy(FU_V1$`Data de nascimento`), ) #Colocar a data em formato lido pelo R


FU_V1 <- mutate(FU_V1, IDADE = (FU_V1$Data_reprocessada - FU_V1$Data_nasc_V2) / 365.25) #Calculo de Idade

FU_V1 <- mutate(FU_V1, 
                   FAIXAS = case_when(FU_V1$IDADE < 25 ~  "Até 24 anos",
                                      IDADE >= 25 & IDADE <= 34 ~ "De 25 a 34 anos",
                                      IDADE >= 35 & IDADE <= 44 ~ "De 35 a 44 anos",
                                      IDADE >= 45 & IDADE <= 54 ~ "De 45 a 54 anos",
                                      IDADE >= 55 & IDADE <= 59 ~ "De 55 a 59 anos",
                                      IDADE >= 60 ~ "60 anos ou mais"))

# Separação de dataframe por equipamento

print(FU_V1$Equipamento) #Ver os nomes dos equipamentos no console

        #ou frequência de equipamentos para facilitar a visualização

table(FU_V1$Equipamento)


CMB <- FU_V1 %>% filter(Equipamento == "Casa da Mulher Brasileira") #Primeiro Dataframe


                           #Filtragem com conjunto de valores do tipo character 

CCMs <- FU_V1 %>% filter(Equipamento == "CCM Capela do Socorro" | 
                           Equipamento == "CCM Parelheiros" |  
                           Equipamento == "CCM Santo Amaro" |
                           Equipamento == "CCM Itaquera" |
                           Equipamento == "CCM Perus")

CRCMs <- FU_V1 %>% filter(Equipamento == "CRM 25 de Março" | 
                           Equipamento == "CRM Casa Eliane de Grammont" |  
                           Equipamento == "CRM Casa Brasilândia" |
                           Equipamento == "CRM Maria de Lourdes Rodrigues" )



# Cruzamentos bivariados com percentual e número absoluto

install.packages("gmodels")  #Pacote para usar função CrossTable
library(gmodels)

                            #Vincular a função ao dataframe
attach(CCMs)

CrossTable(Equipamento, ano, digits = 2, prop.r = TRUE,prop.c = TRUE,prop.t = FALSE,chisq = FALSE, fisher = FALSE,  mcnemar = FALSE, format = "SPSS")

