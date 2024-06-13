if(!require('pacman'))install.packages('pacman')
pacotes <- c('dplyr','tidyverse','lubridate','gmodels','writexl', 'stringr', 'sjmisc')
pacman::p_load(pacotes, character.only = TRUE)


BD_V1 <- BD_Descarrilhado
#Retirar observações sem nome 

BD_TRATADO <- BD_V1 %>% filter(!is.na(BD_V1$`Nome Civil:`))

#Compatibilizar nomes de colunas

BD_TRATADO <- BD_TRATADO %>% rename('Nome civil' = `Nome Civil:`,
                                    "Data de Nascimento" = `Data de Nascimento:`,
                                    "Nome da mãe" = `Nome da Mãe:`,
                                    "Nome do pai" = `Nome do Pai:`,
                                    "Município de Nascimento" = `Município de Nascimento:`,
                                    "País de Nascimento" = `País de Nascimento:`,
                                    "Tipo de Documento" = `Outro Documento:`,
                                    "Nº Documento" = `Número do Outro Documento:`,
                                    "Identidade de Gênero (autodeclaratório)" = `Identidade de Gênero:`,
                                    "Raça/Cor (autodeclaratório)" = `Raça/Cor`,
                                    "Orientação Sexual (autodeclaratório)" = `Orientação Sexual:`,
                                    "Estado civil" = `Estado Civil:`,
                                    "Situação conjugal" = `Situação Conjugal:`,
                                    "Escolaridade" = `Escolaridade:`,
                                    "Renda Familiar" = `Renda Familiar:`,
                                    "Relação de Trabalho" = `Relação de Trabalho:`,
                                    "Benefícios socioassistenciais e previdenciários" = `Benefícios socioassistenciais e previdenciários:`,
                                    "Possui deficiência?" = `É pessoa com deficiência?`,
                                    "Contato" = `Telefone de Contato`)

# Retirar observações sem CPF ou sem outro documento

BD_TRATADO_V2 <- BD_TRATADO %>% filter(!is.na(CPF) | !is.na(BD_TRATADO$`Nº Documento`))

#Tirar observações de CPF com quant de números menores ou maiores do que 11

## Primeiro identifica quais tem 11 dígitos, quais não possui 

BD_TRATADO_V2 <- BD_TRATADO_V2 %>% mutate(BD_TRATADO_V2, 
                                          TEM_CPF = case_when(
                                            nchar(as.character(CPF)) == 11 ~"Sim",
                                            TRUE ~ "Não"))

##Filtrar por quem tem CPF com 11 caracteres e quem tem outros documentos


BD_TRATADO_V3 <- BD_TRATADO_V2 %>% filter(TEM_CPF == "sim" | !is.na(BD_TRATADO_V2$`Nº Documento`))

# Ver se aquele label tem RG no meio

BD_TRATADO_V3 <- BD_TRATADO_V3 %>% 
  mutate(Outro_DOC_teste = grepl("RG", `Nº Documento`))

# Retirar reposta múltipla de RG

RG <- data.frame(
  ID = BD_TRATADO_V3$ID,
  Coluna2 = BD_TRATADO_V3$`Nº Documento`)

RG <- RG %>% separate(Coluna2, into = c("A1", "A2", "A3"), sep = "[;]")

RG <- subset(RG, select = -A3)
RG <- subset(RG, select = -A2)

RG <- RG %>% rename(N_DOCUMENTO_V2= A1)

#Join de variável sem multiplicidade

BD_TRATADO_V3 <- left_join(BD_TRATADO_V3, RG,
                           by = 'ID')


#Criando variável "Outro_DOC_teste

BD_TRATADO_V3 <- BD_TRATADO_V3 %>%
  mutate(Outro_DOC = ifelse(`Tipo de Documento` == "RG", gsub("[^0-9]", "", BD_TRATADO_V3$`Nº Documento`), NA))

BD_TRATADO_V3 <- BD_TRATADO_V3 %>% 
  mutate(OUTRO_DOC_V2 = case_when(
    BD_TRATADO_V3$`Tipo de Documento` %in% c("RNM/RNE", "Passaporte") ~ BD_TRATADO_V3$`Nº Documento`,
    TRUE ~ BD_TRATADO_V3$Outro_DOC
  ))

#Retirar letras e caracteres da variável 

BD_TRATADO_V3 <- BD_TRATADO_V3 %>%
  mutate(N_RG_TRATADO = ifelse(`Tipo de Documento` == "RG" | Outro_DOC_teste == "TRUE", gsub("[^0-9]", "", N_DOCUMENTO_V2), NA))


#Retirando números maiores do que 9 

BD_TRATADO_V3$N_RG_TRATADO <- ifelse(nchar(BD_TRATADO_V3$N_RG_TRATADO) > 9, NA, BD_TRATADO_V3$N_RG_TRATADO)

#Ver categorias de variável do tipo caracter no console
print(unique(BD_TRATADO_V3$`Tipo de Documento`))

BD_TRATADO_V3 <- BD_TRATADO_V3 %>% relocate(N_RG_TRATADO, .before = `Nº Documento`)


#Variável 'tipo de documento' tratada

BD_TRATADO_V3 <- BD_TRATADO_V3 %>% mutate(BD_TRATADO_V3, 
                                          TIPO_DOC_V2 = case_when(
                                            BD_TRATADO_V3$Outro_DOC_teste == TRUE ~ "RG",
                                            BD_TRATADO_V3$`Tipo de Documento` == "RG" ~ "RG",
                                            BD_TRATADO_V3$`Tipo de Documento` == "Passaporte" ~ "Passaporte",
                                            BD_TRATADO_V3$`Tipo de Documento` == "RNM/RNE" ~ "RNM/RNE",
                                            TRUE ~ NA))

BD_TRATADO_V3 <- BD_TRATADO_V3 %>% relocate(TIPO_DOC_V2, .before = `Tipo de Documento`)


#Criar variável com consolidação de RNM e Passaporte


BD_TRATADO_V3 <- BD_TRATADO_V3 %>%
  mutate(OUTRO_DOC_TRATADO_v2 = ifelse(TIPO_DOC_V2 == "RNM/RNE" | TIPO_DOC_V2 == "Passaporte", gsub("[^0-9]", "", OUTRO_DOC_V2), NA))


#Criar variável com consolidação de RG, RNM e Passaporte

BD_TRATADO_V3 <- BD_TRATADO_V3 %>%
  mutate(Outro_doc_final = ifelse(!is.na(N_RG_TRATADO), N_RG_TRATADO, OUTRO_DOC_TRATADO_v2))


BD_TRATADO_V3 <- BD_TRATADO_V3 %>% relocate(Outro_doc_final, .before = N_RG_TRATADO)


#Remover colunas criadas para o tratamento das variáveis de interesse


BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -`Tipo de Documento`)
BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -`Tipo de Documento`)
BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -N_RG_TRATADO)
BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -`Nº Documento`)
BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -Outro_DOC)
BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -N_DOCUMENTO_V2)
BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -OUTRO_DOC_TRATADO_v2)
BD_TRATADO_V3 <- subset(BD_TRATADO_V3, select = -OUTRO_DOC_V2)

#Filtrar BD quando tem CPF ou rg ou rnm ou passaporte

BD_TRATADO_V4 <- BD_TRATADO_V3 %>% filter(TEM_CPF == "sim" | !is.na(BD_TRATADO_V3$Outro_doc_final))


#Deletar colunas não utilizáveis neste projeto

BD_TRATADO_V5 <- BD_TRATADO_V4 %>% select(-`Quantas pessoas compõem o núcleo familiar?`,
                                          -`Faz uso de cadeira de rodas ou de outro meio auxiliar de locomoção próprio?`,
                                          -`Necessita de algum recurso de acessibilidade ambiental?`)


BD_TRATADO_V5 <- BD_TRATADO_V5 %>% select(-(`Qual o nome da entidade?`))
BD_TRATADO_V5 <- BD_TRATADO_V5 %>% select(-`A pessoa tem filhos? Quantos?`)

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename("Nº Documento" = Outro_doc_final)
BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename("Tipo de Documento" = TIPO_DOC_V2)

#Mudar posição planilha

#Coluna 1 antes da coluna 2

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% relocate(`Nº Documento`, .before = `Condição de Moradia:`)

#Últimas mudanças de nomes de variáveis

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename("Nome social" = `Nome Social:`)

#Transformar label de "X" para "Y"

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Sigilo de Correspondência e Contato:` = if_else(`Sigilo de Correspondência e Contato:`== "Falso", "Não", `Sigilo de Correspondência e Contato:`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Sigilo de Correspondência e Contato:` = if_else(`Sigilo de Correspondência e Contato:`== "True", "Sim", `Sigilo de Correspondência e Contato:`))

print(unique(BD_TRATADO_V5$`Sigilo de Correspondência e Contato:`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename('Demanda sigilo de correspondência/contato' = `Sigilo de Correspondência e Contato:`)

#Retirar Caracteres de um label 


BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Possui deficiência?` = str_replace_all(`Possui deficiência?`,'\\]', ''))
BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Possui deficiência?` = str_replace_all(`Possui deficiência?`,'\\[', ''))
BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Possui deficiência?` = str_replace_all(`Possui deficiência?`,'"', ''))

##Remover aspas

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Possui deficiência?` = str_replace_all(`Possui deficiência?`,'"', ''))


#Mudar letras de uma string para maiúsculo

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`País de Nascimento` = toupper(`País de Nascimento`))

#Excluir nome social idêntico ao nome civil 

BD_TRATADO_V5$`Nome social` <- ifelse(BD_TRATADO_V5$`Nome social` == BD_TRATADO_V5$`Nome civil`, NA, BD_TRATADO_V5$`Nome social`)

#Excluir números e caracteres de string Nome Civil


BD_TRATADO_V5$`Nome civil`  <- gsub("[0-9]", "", BD_TRATADO_V5$`Nome civil`)

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Nome civil` = str_replace_all(`Nome civil`,'\\.', ''))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Nome civil` = toupper(`Nome civil`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Nome social` = toupper(`Nome social`))

#Investigar compatibilidade de categorias com as do SIAD

## Ver categorias da variável no console

print(unique(BD_TRATADO_V5$`País de Nascimento`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`País de Nascimento` = if_else(`País de Nascimento` == "BR", "BRASIL", `País de Nascimento`))

print(unique(BD_TRATADO_V5$`Município de Nascimento`))

#Deletar tudo depois do caracter "-" em Município de Nascimento

BD_TRATADO_V5$`Município de Nascimento` <- sub("-.*", "", BD_TRATADO_V5$`Município de Nascimento`)

#Colocar todos os nomes dos municípios em maiúsculo

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Município de Nascimento` = toupper(`Município de Nascimento`))

#Colocar todos os nomes em maiúsculo também

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Nome do pai` = toupper(`Nome do pai`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Nome do pai` = if_else(`Nome do pai` == "NOME DO PAI", NA, `Nome do pai`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate_all(~str_replace(., "NOME DO PAI", ""))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Nome da mãe` = toupper(`Nome da mãe`))

#Tratando condição de moradia

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename('Condição de Moradia' = `Condição de Moradia:`)

print(unique(BD_TRATADO_V5$`Condição de Moradia`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Condição de Moradia` = if_else(`Condição de Moradia` == "Acolhimento Institucional/Centro de Acolhida", NA, `Condição de Moradia`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Condição de Moradia` = if_else(`Condição de Moradia` == "SITUAÇÃO DE RUA", "Situação de Rua", `Condição de Moradia`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Condição de Moradia` = if_else(`Condição de Moradia` == "Situação de rua/calçada sem acolhimento institucional", "Situação de Rua", `Condição de Moradia`))


#Criação de variável acolhimento institucional 

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Em acolhimento institucional?` = if_else(`Condição de Moradia` == "Acolhimento Institucional/Centro de Acolhida", "Sim", NA))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Condição de Moradia` = if_else(`Condição de Moradia` == "ESTRANGEIRA .", NA, `Condição de Moradia`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% relocate(`Em acolhimento institucional?`, .before = `Demanda sigilo de correspondência/contato`)

#Tratando sigilo de correspondência

print(unique(BD_TRATADO_V5$`Demanda sigilo de correspondência/contato`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Demanda sigilo de correspondência/contato` = if_else(`Demanda sigilo de correspondência/contato` == "Não", NA , `Demanda sigilo de correspondência/contato`))

#Tratando identidade de gênero

print(unique(BD_TRATADO_V5$`Identidade de Gênero (autodeclaratório)`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Identidade de Gênero (autodeclaratório)` = if_else(`Identidade de Gênero (autodeclaratório)` == "Homem Transexual", "Homem Trans", `Identidade de Gênero (autodeclaratório)`))

#Tratando orientação sexual 

print(unique(BD_TRATADO_V5$`Orientação Sexual (autodeclaratório)`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Orientação Sexual (autodeclaratório)` = if_else(`Orientação Sexual (autodeclaratório)` == "Homosexual", "Lésbica", `Orientação Sexual (autodeclaratório)`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Orientação Sexual (autodeclaratório)` = if_else(`Orientação Sexual (autodeclaratório)` == "Não declarada", NA, `Orientação Sexual (autodeclaratório)`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Orientação Sexual (autodeclaratório)` = if_else(`Orientação Sexual (autodeclaratório)` == "TRANS", NA, `Orientação Sexual (autodeclaratório)`))

#Tratando raça/cor

print(unique(BD_TRATADO_V5$`Raça/Cor (autodeclaratório)`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Raça/Cor (autodeclaratório)` = if_else(`Raça/Cor (autodeclaratório)` == "Não Declarada", NA, `Raça/Cor (autodeclaratório)`))

#Tratando Estado Civil

print(unique(BD_TRATADO_V5$`Estado civil`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Estado civil` = if_else(`Estado civil` == "União Estável com registro em cartório", "União Estável (com registro em cartório)", `Estado civil`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Situação Conjugal` = ifelse(`Estado civil` == "União Estável sem registro em cartório", "União Estável sem registro em cartório", NA))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% relocate(`Situação Conjugal`, .before = `Situação conjugal`)

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% select(-`Situação conjugal`)

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Situação Conjugal` = if_else(`Situação Conjugal` == "União Estável sem registro em cartório", "União Estável (sem registro em cartório)", `Situação Conjugal`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Estado civil` = if_else(`Estado civil` == "União Estável sem registro em cartório", NA, `Estado civil`))

#Tratando Escolaridade

print(unique(BD_TRATADO_V5$`Renda Familiar`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Renda Familiar` = if_else(`Renda Familiar` == "De meio até 1 salário mínimo", "Mais de meio salário mínimo até 1 salário mínimo", `Renda Familiar`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Renda Familiar` = if_else(`Renda Familiar` == "De 1 até 2 salários mínimos", "Mais de 1 salário mínimo até 2 salários mínimos", `Renda Familiar`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Renda Familiar` = if_else(`Renda Familiar` == "De 2 até 3 salários mínimos", "Mais de 2 salários mínimos até 3 salários mínimos", `Renda Familiar`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Renda Familiar` = if_else(`Renda Familiar` == "Mais de 3 salários mínimos", "Mais de 3 salários mínimos", `Renda Familiar`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Renda Familiar` = if_else(`Renda Familiar` == "Escolha 7", NA, `Renda Familiar`))

#Tratando Relação de Trabalho

print(unique(BD_TRATADO_V5$`Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "Não declarado", NA, `Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "POT - Programa Operação Trabalho", NA, `Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "PENSIONISTA", NA, `Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "DO LAR", NA, `Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "CATADOR DE ENTULHO", NA, `Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "APOSENTADA", "Não exerce atividade regular remunerada", `Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "Aposentado", "Não exerce atividade regular remunerada", `Relação de Trabalho`))

#Tratando Ocupação

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Ocupação` = str_replace_all(`Ocupação`,'"', ''))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Ocupação` = str_replace_all(`Ocupação`,'\\[', ''))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Ocupação` = str_replace_all(`Ocupação`,'\\]', ''))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% 
  mutate(`Ocupação` = toupper(`Ocupação`))

#Tratando Deficiência

print(unique(BD_TRATADO_V5$`Possui deficiência?`))


BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Não", NA, `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Visual parcial (baixa visão)" , "Visual", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Visual total (cegueira);" , "Visual;", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Visual total (cegueira);Surdez bilateral total" , "Visual;Auditiva", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Mobilidade Reduzida" , "Motora", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Mental/Psicossocial;" , "Intelectual;", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Mental/Psicossocial;CID F20" , "Intelectual;Outra", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Transtorno do Espectro Autista - TEA" , "Outra", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Visão monocular (cegueira em um olho)" , "Visual", `Possui deficiência?`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Possui deficiência?` = if_else(`Possui deficiência?` == "Não;Física" , "Física", `Possui deficiência?`))


#Deletando colunas

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% select(-`Endereço de Referência`)


write_xlsx(BD_TRATADO_V5,"BD_TRATADO_V5.xlsx")

#Tratando telefones contato 1


BD_CEL <- data.frame(
  ID = BD_TRATADO_V5$ID,
  Coluna3 = BD_TRATADO_V5$Contato_2)

BD_CEL <- BD_CEL %>% separate(Coluna3, into = c("A1", "A2", "A3", "A4"), sep = "[/]")

BD_CEL <- subset(BD_CEL, select = -A3)
BD_CEL <- subset(BD_CEL, select = -A2)

BD_CEL <- BD_CEL %>% rename(CEL_TRATADO_V1= A1)

BD_CEL <- BD_CEL %>%
  mutate(CEL_TRATADO_V2 = ifelse(!is.na(CEL_TRATADO_V1), gsub("[^0-9]", "", CEL_TRATADO_V1), NA))


BD_CEL <- subset(BD_CEL, select = -CEL_TRATADO_V1)

BD_CEL$CEL_TRATADO_V2 <- ifelse(nchar(BD_CEL$CEL_TRATADO_V2) > 12, NA, BD_CEL$CEL_TRATADO_V2)


BD_TRATADO_V5 <- left_join(BD_TRATADO_V5, BD_CEL,
                   by = 'ID')


write_xlsx(BD_V5,"BD_V5.xlsx")

names(BD_V5)

BD_V5 <- subset(BD_V5, select = -TEM_CPF)
BD_V5 <- subset(BD_V5, select = -Outro_DOC_teste)
BD_TRATADO_V5 <- subset(BD_TRATADO_V5, select = -Contato_2)

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename('Contato_2' = `CEL_TRATADO_V2`)


#Nome social

Nome_social <- data.frame(
  ID = BD_V1$ID,
  Coluna2 = BD_V1$`Nome Social:`)


Nome_social <- Nome_social %>% rename('Nome social' = `Coluna2`)


BD_V5 <- left_join(BD_V5, Nome_social,
                   by = 'ID') 

BD_V5 <- subset(BD_V5, select = -`Nome social`)

BD_V5 <- BD_V5 %>% rename('Nome social' = `Nome social.y`)

BD_V5$`Nome social` <- ifelse(BD_V5$`Nome social` == BD_V5$`Nome civil`, NA, BD_V5$`Nome social`)

BD_V5<- BD_V5 %>% 
  mutate(`Nome civil` = toupper(`Nome civil`))

BD_TRATADO_V5<- BD_TRATADO_V5 %>% 
  mutate(`Nome social` = toupper(`Nome social`))

#Idade



Nome_Social <- Nome_Social %>% rename('Nome social' = `BD_V1$`Nome Social:``)

BD_COPIA <- subset(BD_COPIA, select = -`Ocupação`)

#País de nascimento

BD_PAIS <- data.frame(
  ID = BD_TRATADO_V5$ID,
  Coluna2 = BD_TRATADO_V5$`País de Nascimento`)

BD_PAIS <- BD_PAIS %>% separate(Coluna2, into = c("A1", "A2", "A3"), sep = "[;]")

BD_PAIS  <- subset(BD_PAIS , select = -A2)
BD_PAIS  <- subset(BD_PAIS , select = -A3)


BD_TRATADO_V5 <- left_join(BD_TRATADO_V5, BD_PAIS,
                      by = 'ID')

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% relocate(A1, .before = `País de Nascimento`)

BD_TRATADO_V5  <- subset(BD_TRATADO_V5 , select = -`País de Nascimento`)

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename('País de Nascimento' = `A1`)

BD_TRATADO_V5 <- BD_TRATADO_V5 %>% rename('Tipos de Deficiência' = `Possui deficiência?`)

print(unique(BD_TRATADO_V5$`Tipos de Deficiência`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Tipos de Deficiência` = if_else(`Tipos de Deficiência` == "Surdez bilateral parcial", "Auditiva", `Tipos de Deficiência`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Tipos de Deficiência` = if_else(`Tipos de Deficiência` == "Motora", "Física", `Tipos de Deficiência`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Tipos de Deficiência` = if_else(`Tipos de Deficiência` == "Mental/Psicossocial,CID F20", "Intelectual", `Tipos de Deficiência`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Tipos de Deficiência` = if_else(`Tipos de Deficiência` == "Visual total (cegueira),Surdez bilateral total", "Visual;Auditiva", `Tipos de Deficiência`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Tipos de Deficiência` = if_else(`Tipos de Deficiência` == "Mental/Psicossocial", "Intelectual", `Tipos de Deficiência`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Tipos de Deficiência` = if_else(`Tipos de Deficiência` == "Visual total (cegueira)", "Visual", `Tipos de Deficiência`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Tipos de Deficiência` = if_else(`Tipos de Deficiência` == "Não,Física", "Física", `Tipos de Deficiência`))


#Relação de trabalho 

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "Empregada(o), sem carteira", "Empregada(o) sem carteira", `Relação de Trabalho`))

BD_TRATADO_V5 <- BD_TRATADO_V5 %>%
  mutate(`Relação de Trabalho` = if_else(`Relação de Trabalho` == "Empregada(o), com carteira", "Empregada(o) com carteira", `Relação de Trabalho`))


write.csv(BD_COPIA, "BD_OUVIDORIA_SIAD", row.names = FALSE)

write_xlsx(BD_COPIA,"BD_OUVIDORIA_SIAD.xlsx")

install.packages(sjPlot)

view_df(BD_TRATADO_V5[, c(2:32)],  
        show.na = TRUE, 
        show.type = TRUE, 
        show.frq = TRUE, 
        show.prc = TRUE, 
        show.string.values = TRUE, 
        show.id = TRUE)


print(unique(BD_TRATADO_V5$`É o responsável financeiramente pela família?`))


BD_TRATADO_V6 <- BD_TRATADO_V5 


BD_TRATADO_V6$CPF <- ifelse(is.na(BD_TRATADO_V6$CPF), NA, gsub("[^0-9]", "", BD_TRATADO_V6$CPF))

write_xlsx(BD_TRATADO_V6,"BD_TRATADO_V6.xlsx")

write.csv(BD_TRATADO_V6, "BD_OUVIDORIA_V3.csv", row.names = FALSE)
