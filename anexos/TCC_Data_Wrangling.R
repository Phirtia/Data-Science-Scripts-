if(!require('pacman'))install.packages('pacman')
pacotes <- c('dplyr','tidyverse','lubridate','gmodels','writexl', 'ggplot2', 'readr', 'lubridate', 'sjPlot', 'readxl')
pacman::p_load(pacotes, character.only = TRUE)


BD_ANONIMIZADO_V2 <- read_excel("BD_ANONIMIZADO_V2.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "text", "date", "date", "text", "text", 
                                              "numeric", "text", "text", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "text", "numeric", "text", 
                                              "text", "text", "numeric", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "numeric", "text", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "text", "text", "text", 
                                              "text"))

BD <- BD_ANONIMIZADO_V2

attach(BD)

#Convertendo códigos para labels identificáveis

BD_V2 <- mutate(BD,
                P1 = recode(P1, "1" = "CRCM Eliane de Grammont",
                            "2" = "CRCM 25 de Março",
                            "3" = "CRCM Brasilândia",
                            "4" = "CRCM Maria de Lourdes Rodrigues"))

BD_V2 <- mutate(BD_V2,
                P3 = recode(P3, "1" = "São Paulo",
                                 "2" = "Nordeste",
                            "3"  = "Centro-Oeste",
                            "4" = "Outros municípios de SP",
                            "5" = "Sul",
                            "6" = "Norte", 
                            "7" = "Sudeste", 
                            "8" = "Outro país"))


BD_V2 <- mutate(BD_V2,
                P8 = recode(P8, "5" = "Defesa em caso de violação de direitos",
                            "1" = "Acesso a serviços públicos",
                            "2" = "Acesso a programas de cidadania",
                            "6" = "Orientações diversas",
                            "4" = "Apoio à familia de pessoa desaparecida",
                            "3" = "Acolhimento em caso de situação de risco",
                            "1;2" = "Acesso a serviços públicos;Acesso a programas de cidadania",
                            "1;2;5;6" = "Acesso a serviços públicos;Acesso a programas de cidadania;Defesa em caso de violação de direitos;Orientações diversas",
                            "1;3;5" = "Acesso a serviços públicos;Acolhimento em caso de situação de risco;Defesa em caso de violação de direitos",
                            "1;5;6" = "Acesso a serviços públicos;Defesa em caso de violação de direitos;Orientações diversas",
                            "3;5" = "Acolhimento em caso de situação de risco;Defesa em caso de violação de direitos"))
table(BD_V2$P8)

BD_V2 <- mutate(BD_V2,
                P9 = recode(P9, "1"="Casada(o)",
                            "3"="Solteira(o)",
                            "2"="Divorciada(o)",
                            "5"="União Estável (sem registro no cartório)", 
                            "6"="Viúva(o)",
                            "4"="União Estável (com registro em cartório"))
table(BD_V2$P9)

BD_V2 <- mutate(BD_V2,
                P11 = recode(P11,  "1"="Com companheiro(a)",
                             "2"="Sem companheiro(a)", 
                             "SI" = NA_character_))

table(BD_V2$P11)

BD_V2 <- mutate(BD_V2,
                P12 = recode(P12, "2"="Branca",
                             "5"="Indígena",
                             "1"="Amarela",
                             "3"="Parda",
                             "4"="Preta"))
BD_V2[262, 10] <- NA
BD_V2[262, 11] <- "Branca"
BD_V2[262, 12] <- "Superior Completo"

table(BD_V2$P12)

BD_V2 <- mutate(BD_V2,
                P13 = recode(P13, "3"="Fundamental Completo",
                             "11"="Superior Completo",
                             "5"="Médio Completo",
                             "9"="Pós Graduação",
                             "99"="Não Alfabetizada(o)",
                             "1"="Ensino Técnico Completo",
                             "12"="Superior Incompleto",
                             "5"="Médio Incompleto",
                             "4"="Fundamental Incompleto",
                            "10"="Pós Graduação Incompleta",
                            "97"="Não Declarado",
                            "2" = "Ensino Técnico Incompleto",
                            "6" = "Médio Incompleto",
                            "NA" = NA_character_))

table(BD_V2$P13)

BD_V2 <- mutate(BD_V2,
                P14 = recode(P14,  "1"="Sim",
                             "2"="Não",
                             "SI" = NA_character_,
                             "3" = NA_character_))

BD_V2 <- mutate(BD_V2,
              P15 = recode(P15,
                           "2" = "Candomblé",
                           "1" = "Budista",
                           "3" = "Católica",
                           "4" = "Espírita",
                           "5" = "Espírita/Kardecista",
                           "6" = "Evangélica Pentecostal",
                           "7" = "Evangélica Tradicional/De Missão",
                           "8" = "Islâmica",
                           "13" = "Tradições indígenas",
                           "99" = "Sem religião/Agnóstico/Ateu",
                           "97" = "Sem religião/Agnóstico/Ateu",
                           "12" = "Testemunha de Jeová",
                           "14" = "Umbanda",
                           "98" = "Outra"))

table(BD_V2$P15)

BD_V2 <- mutate(BD_V2,
                P17 = recode(P17,  "1"="Profissão de nível médio",
                             "2"="Profissão de nível especializado",
                             "3"="Estudante",
                             "4"="Profissional do lar",
                             "5"="Profissão de nível técnico",
                             "98" = "Outra"))

BD_V2 <- mutate(BD_V2,
                P18 = recode(P18,
                             "99" = "Não trabalha",
                             "4" = "Desempregada(o)",
                             "5" = "Empregada(o) com carteira",
                             "1" = "Autônomo",
                             "2" = "Aposentado",
                             "3" = "Cooperado(a)",
                             "11" = "Pessoa Jurídica sem empregada(o)s",
                             "10" = "Pessoa Jurídica com empregada(o)s",
                             "7" = "Funcionária(o) Pública(o)",
                             "12" = "Trabalho informal",
                             "13" = "Trabalho intermitente",
                             "6" = "Empregada(o) sem carteira"))

BD_V2 <- mutate(BD_V2,
                P19 = recode(P19,  "1"="Sim",
                             "2"="Não"))

BD_V2 <- mutate(BD_V2,
                P20 = recode(P20, "1"="Até 1 dependente",
                             "2"="De 2 à 3 dependentes",
                             "3"="Mais que 3 dependentes",
                             "4"="Nenhum dependente",
                             "99" = "Nenhum dependente"))

BD_V2 <- mutate(BD_V2,
                P21 = recode(P21, 
                             "1" = "Criança/adolescente", 
                             "2" = "Pessoa idosa",
                             "3" = "Pessoa com deficiência",
                             "4" = "Pessoa com restrição para trabalho",
                             "98" = "Outro",
                             "1;3" = "Criança/adolescente;Pessoa com deficiência"))
table(BD_V2$P21)

BD_V2 <- mutate(BD_V2,
                P22 = recode(P22, "1" = "Acolhimento Institucional (Abrigo)",
                             "2" = "Unidades de reclusão",
                             "3" = "Aldeia (indígena)",
                             "4" = "Alugado",
                             "5" = "Cedido",
                             "6" = "Próprio",
                             "7" = "Ocupação",
                             "8" = "Situação de rua/calçada",
                             "98" = "Outra"))

BD_V2 <- mutate(BD_V2,
                P23 = recode(P23, "3"="Casa/Apartamento",
                             "8"="Hotel, pensão e similares",
                             "1"="Acolhimento institucional/Abrigo",
                             "4"="Cortiço",
                             "9"="Ocupação",
                             "6"="Domicílio improvisado",
                             "96" = NA_character_))

BD_V2 <- mutate(BD_V2,
                P24 = recode(P24, "1" = "Aposentadoria",
                             "2" = "Auxílio Emergencial (COVID-19)",
                             "3" = "Auxílio acidente",
                             "4" = "Auxílio aluguel",
                             "5" = "Auxílio doença",
                             "6" = "Auxílio reclusão",
                             "7" = "Bolsa Família",
                             "8" = "Benefício de Prestação Continuada (BPC/LOAS)",
                             "9" = "Bolsa Trabalho",
                             "12" = "Programa Operação Trabalho (POT)",
                             "11" = "Pensão",
                             "13" = "Renda cidadã",
                             "14" = "Renda mínima",
                             "15" = "Seguro desemprego",
                             "98" = "Outros",
                             "2;7" = "Auxílio Emergencial (COVID-19);Bolsa Família",
                             "2;8" = "Auxílio Emergencial (COVID-19);Benefício de Prestação Continuada (BPC/LOAS)",
                             "2;9" = "Auxílio Emergencial (COVID-19);Bolsa Trabalho",
                             "4;8" = "Auxílio aluguel;Benefício de Prestação Continuada (BPC/LOAS)",
                             "5;7" = "Auxílio doença;Bolsa Família",
                             "7;4" = "Bolsa Família;Auxílio aluguel"))
BD_V2 <- mutate(BD_V2,
                P25 = recode(P25, "1"="Sem renda",
                             "2"="Até um salário mínimo (R$ 1.039)",
                             "3"="De 1 a 2 salários mínimos (R$ 1.040 a R$ 2.078)",
                             "4"="De 2 a 3 salários mínimos (R$ 2.079 a R$ 3.117)",
                             "5"="De 3 a 5 salários mínimos (R$ 2.080 a R$ 5.195)",
                             "6"="De 5 a 10 salários mínimos (R$ 5.196 a R$ 10.390)",
                             "7"="De 10 a 20 salários mínimos (R$ 10.391 a R$ 20.780)"))


BD_V2 <- mutate(BD_V2,
                P26 = recode(P26,  "4"="Mulher Cisgênero",
                             "3"="Mulher Transexual",
                             "2" = "Mulher Cisgênero",
                             "5"= "Mulher Transexual"))

table(BD_V2$P26)

BD_V2 <- mutate(BD_V2,
                P27 = recode(P27, "2" = "Heterossexual",
                             "4" = "Lésbica",
                             "1" = "Bissexual",
                             "6" = "Assexual",
                             "5" = "Pansexual"))

table(BD_V2$P27)

BD_V2 <- mutate(BD_V2,
                P28 = recode(P28, "8"="Sexismo",
                             "2" = "Conflito Geracional",
                             "4" = "Homofobia",
                             "7" = "Racismo",
                             "2;8" = "Conflito Geracional;Sexismo",
                             "6;8" = "Lesbofobia;Sexismo",
                             "7;8" = "Racismo;Sexismo", 
                             "8;10" = "Sexismo,Transfobia"))

BD_V2 <- mutate(BD_V2,
                P30 = recode(P30, "1" = "Ambiente escolar",
                             "2" = "Ambiente familiar",
                             "3" = "Ambiente de trabalho",
                             "4" = "Estabelecimento comercial",
                             "5" = "Órgão Público",
                             "6" = "Via Pública",
                             "7" = "Transporte Público",
                             "2;3" = "Ambiente familiar;Ambiente de trabalho",
                             "2;5" = "Ambiente familiar;Órgão Público",
                             "2;6" = "Ambiente familiar;Via Pública",
                             "2;6;7" = "Ambiente familiar;Via Pública;Transporte Público"))

BD_V2 <- mutate(BD_V2,
                P31A = recode(P31A,  "1" = "Cônjuge ou companheiro",
                              "2" = "Ex-cônjuge ou ex-companheiro",
                              "3" = "Namorado",
                              "4" = "Ex-namorado",
                              "5" = "Familiar",
                              "6" = "Agente público",
                              "7" = "Cliente",
                              "8" = "Chefia",
                              "9" = "Funcionário(a)",
                              "10" = "Professor(a)",
                              "11" = "Relação institucional",
                              "12" = "Vizinho(a)",
                              "13" = "Outro",
                              "98" = "Outro",
                              "14" = "Desconhecido(a)",
                              "1;5" = "Cônjuge ou companheiro;Familiar",
                              "2;5" = "Ex-cônjuge ou ex-companheiro;Familiar",
                              "4;5" = "Ex-namorado;Familiar",
                              "8;9" = "Chefia;Funcionário(a)"))


BD_V2 <- mutate(BD_V2,
                P32 = recode(P32,  "1"="Sim",
                             "2"="Não"))


BD_V2 <- mutate(BD_V2,
                P33 = recode(P33, 
                             "1" = "BO",
                             "2" = "Medida protetiva",
                             "3" = "Outro",
                             "1;2" = "BO;Medida protetiva"))


BD_V2 <- mutate(BD_V2,
                P34 = recode(P34,  "1"="Sim",
                             "2"="Não"))


BD_V2 <- mutate(BD_V2,
                P35 = recode(P35,  "1"="Sim",
                             "2"="Não",
                             "96" = "Não se aplica"))


BD_V2 <- mutate(BD_V2,
                P36 = recode(P36,  "1" = "Desencorajou com argumento de improcedência da denúncia",
                             "2" = "Informou não ser de sua competência",
                             "3" = "Negou-se a registrar a denúncia"))
BD_V2 <- mutate(BD_V2,
                P37 = recode(P37,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P39 = recode(P39,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P40 = recode(P40,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P41 = recode(P41,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P43 = recode(P43,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P44 = recode(P44,  "Sim"="Medida protetiva"))

BD_V2 <- mutate(BD_V2,
                P45 = recode(P45,  "1"="Sim",
                             "2"="Não", 
                             "99" = "Não"))
 
BD_V2 <- subset(BD_V2, select = -P46)

BD_V2 <- mutate(BD_V2,
              P46A = recode(P46A, 
                            "1" = "Amigos",
                            "2" = "Entidades Religiosas",
                            "3" = "Familiares",
                            "4" = "Não procurou ajuda",
                            "99" = "Não procurou ajuda",
                            "5" = "Polícia/Justiça",
                            "6" = "Serviço especializado",
                            "98" = "Outro",
                            "1;2;3" = "Amigos;Entidades Religiosas;Familiares",
                            "1;2;3;5" = "Amigos;Entidades Religiosas;Familiares;Polícia/Justiça",
                            "1;3" = "Amigos;Familiares",
                            "1;3;5" = "Amigos;Familiares;Polícia/Justiça",
                            "1;3;5;6" = "Amigos;Familiares;Polícia/Justiça;Serviço especializado",
                            "1;5" = "Amigos;Polícia/Justiça",
                            "3;5" = "Familiares;Polícia/Justiça",
                            "3;5;6" = "Familiares;Polícia/Justiça;Serviço especializado",
                            "5;6" = "Polícia/Justiça;Serviço especializado"))
BD_V2 <- mutate(BD_V2,
                P47 = recode(P47,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P48 = recode(P48,  "1"="Sim",
                             "2"="Não"))

BD_V2 <- mutate(BD_V2,
                P49 = recode(P49,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P51 = recode(P51,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P52 = recode(P52,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P53 = recode(P53,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P54= recode(P54,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P55 = recode(P55,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P56 = recode(P56,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P57 = recode(P57,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P58 = recode(P58,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P59 = recode(P59,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P60 = recode(P60,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P61 = recode(P61,  "1"="Sim",
                             "2"="Não"))
BD_V2 <- mutate(BD_V2,
                P64 = recode(P64,  "1"="Sim",
                             "2"="Não"))


BD_V2 <- mutate(BD_V2,
                P7A = recode(P7A, "1" = "Direito",
                             "2" = "Empregabilidade",
                             "3" = "Multidisciplinar",
                             "4" = "Pedagogia",
                             "5" = "Psicossocial",
                             "6" = "Regularização Migratória",
                             "7" = "Serviço Social",
                             "8" = "Atividade Coletiva"))

#Tornar ID primeiro coluna

BD_V2 <- BD_V2 %>% relocate(ID, .before = P1)

#Excluir linha sem observações

BD_V2 <- subset(BD_V2, !(row.names(BD_V2) == 280))

#Criar outro Dataset com os nomes das variáveis 

BD_V3 <- BD_V2

#Renomear variáveis

BD_V3 <- BD_V3 %>% rename("Equipamento" = P1,
                          "Naturalidade_Recategorizada" = P3,
                          "Distrito" = P4,
                          "Data_Atendimento_Inicial" = P5,
                          "Data_Ultimo_Atendimento" = P6,
                          "Demanda" = P8,
                          "Estado_Civil" = P9,
                          "Tempo_Relacionamento" = P10,
                          "Situação_Conjugal" = P11,
                          "Raça/cor" = P12,
                          "Escolaridade" = P13,
                          "Está_Estudando" = P14,
                          "Religião" = P15,
                          "Profissão_Recategorizada"= P17,
                          "Relação_Trabalho" = P18,
                          "Responsável_Família" = P19,
                          "Possui_Dependentes" = P20,
                          "Perfil_Dependentes" = P21,
                          "Condição_Moradia" = P22,
                          "Tipo_Moradia" = P23,
                          "Benefícios_Socioassistenciais" = P24,
                          "Renda_Familiar" = P25,
                          "Gênero" = P26,
                          "Orientação_Sexual" = P27,
                          "Motivação_Violência" = P28,
                          "Local_Violação" = P30,
                          "Relação_com_Agressor" = P31A,
                          "Registro/Denúncia" = P32,
                          "Qual_Instrumento" = P33,
                          "Teve_Prosseguimento" = P34,
                          "Dificuldade_Registro" = P35,
                          "Motivo_Dificuldade" = P36,
                          "Primeiro_Vez_Violência_Acontece" = P37,
                          "Há_Quanto_Tempo_Acontece" = P38,
                          "Agressor_Vive_com_Usuária" = P39,
                          "Fez_corpo_delito" = P40,
                          "Já_Havia_Denunciado" = P41,
                          "Quantas vezes?" = P42,
                          "Deu_Sequencia_Processo" = P43,
                          "Resultado_Processo" = P44,
                          "Já_procurou_ajuda" = P45,
                          "Que_tipo_ajuda" = P46A,
                          "Serviços_Saúde_pela_Violência" = P47,
                          "Filhos"= P49,
                          "Idade_Filhos" = P50,
                          "Retornou" = P51,
                          "Reativou" = P52,
                          "Narrativa_Revitimização" = P53,
                          "Está_Situação_Violência" = P54,
                          "Violência_Doméstica" = P55,
                          "Presença_Violência_Indireta" = P56,
                          "Desejo_Reestabelecer_Laço" = P57,
                          "Saúde mental_Deficiência_Adicção" = P58,
                          "Não_Saiu_ou_Retornou_Ciclo" = P59,
                          "Demanda_Indireta" = P60,
                          "Revitimizada_Outro_Autor" = P61,
                          "Auxílio Aluguel" = P64,
                          "Área_Atendimento" = "P7A",
                          "Tipo_Violência" = P29A.y)

##RESPOSTAS MÚLTIPLAS 

#Motivação da violência

table(BD_V3$Motivação_Violência)

BD_V3$Motivação_Violência <- gsub("2", "Conflito Geracional", BD_V3$Motivação_Violência)
BD_V3$Motivação_Violência <- gsub("8", "Sexismo", BD_V3$Motivação_Violência)
BD_V3$Motivação_Violência <- gsub("6", "Lesbofobia", BD_V3$Motivação_Violência)
BD_V3$Motivação_Violência <- gsub("4", "Homofobia", BD_V3$Motivação_Violência)
BD_V3$Motivação_Violência <- gsub("7", "Racismo", BD_V3$Motivação_Violência)
BD_V3$Motivação_Violência <- gsub("10", "Transfobia", BD_V3$Motivação_Violência)

#Área de atendimento

table(BD_V3$Área_Atendimento)

BD_V3$Área_Atendimento <- gsub("\tDireito", "Direito", BD_V3$Área_Atendimento)
BD_V3$Área_Atendimento <- gsub("1", "Direito", BD_V3$Área_Atendimento)
BD_V3$Área_Atendimento <- gsub("2", "Empregabilidade", BD_V3$Área_Atendimento)
BD_V3$Área_Atendimento <- gsub("3", "Multidisciplinar", BD_V3$Área_Atendimento)
BD_V3$Área_Atendimento <- gsub("4", "Pedagogia", BD_V3$Área_Atendimento)
BD_V3$Área_Atendimento <- gsub("5", "Psicossocial", BD_V3$Área_Atendimento)
BD_V3$Área_Atendimento <- gsub("6", "Regularização Migratória", BD_V3$Área_Atendimento)
BD_V3$Área_Atendimento <- gsub("7", "Serviço Social", BD_V3$Área_Atendimento)

#Tipo de violência

table(BD_V3$Tipo_Violência)

#ATENÇÃO: EXECUTAR DE TRAZ PARA FRENTE, NESTE CASO

BD_V3$Tipo_Violência <- gsub("1", "Abandono", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("2", "Ameaça", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("3", "Apreensão Irregular de Bens", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("4", "Assédio Moral", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("5", "Assédio Sexual", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("6", "Cárcere Privado", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("7", "Dificuldade de Acesso a Direitos/Serviços Públicos", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("8", "Discriminação", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("9", "Exploração Sexual", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("10", "Importunação Sexual", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("11", "Incitação", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("12", "Irregularidades nos serviços para a Pop Rua", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("13", "Crime contra a vida", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("14", "Negligência", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("15", "Restrição de Liberdade", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("16", "Tentativa de Feminicídio", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("17", "Tentativa de Homicídio", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("18", "Trabalho Escravo", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("19", "Trabalho Infantil", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("20", "Tráfico de Pessoas", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("21", "Violência Digital", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("22", "Violência Física", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("23", "Violência Institucional", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("24", "Violência Moral", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("25", "Violência Obstétrica", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("26", "Violência Patrimonial", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("27", "Violência Psicológica", BD_V3$Tipo_Violência)
BD_V3$Tipo_Violência <- gsub("28", "Violência Sexual", BD_V3$Tipo_Violência)


#Frequência de tipos de violência 


VIOLÊNCIAS <- as.data.frame(BD_V3$Tipo_Violência)

VIOLÊNCIAS <- VIOLÊNCIAS %>% 
  mutate(value = "Sim") %>% 
  tidyr::separate_rows(`BD_V3$Tipo_Violência`, sep = ";")

VIOLÊNCIAS <- VIOLÊNCIAS %>% select(-`value`)

VIOLÊNCIAS <- VIOLÊNCIAS %>% rename("Tipo_Violência" = `BD_V3$Tipo_Violência`)

Tipo_Violência <- VIOLÊNCIAS %>% 
  group_by('Tipo_Violência') %>%
  count() 

table(VIOLÊNCIAS$Tipo_Violência)

##CRIAÇÕES DE NOVAS VARIÁVEIS 

#Quantidade de violências e presença de tipos principais

#Quantidade

QUANT_VIOLÊNCIAS <- data.frame(
  ID = BD_V3$ID,
  Coluna2 = BD_V3$Tipo_Violência)

QUANT_VIOLÊNCIAS <- QUANT_VIOLÊNCIAS %>% separate(Coluna2, into = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8","A9"), sep = "[;]")

QUANT_VIOLÊNCIAS$QUANT_VIOLÊNCIAS <- rowSums(!is.na(QUANT_VIOLÊNCIAS))


#Tipos principais


QUANT_VIOLÊNCIAS$Viol_Física <- apply(QUANT_VIOLÊNCIAS, 1, function(x) any(grepl("Violência Física", paste(x, collapse = " "))))
QUANT_VIOLÊNCIAS$Viol_Psicológica <- apply(QUANT_VIOLÊNCIAS, 1, function(x) any(grepl("Violência Psicológica", paste(x, collapse = " "))))
QUANT_VIOLÊNCIAS$Viol_Moral <- apply(QUANT_VIOLÊNCIAS, 1, function(x) any(grepl("Violência Moral", paste(x, collapse = " "))))
QUANT_VIOLÊNCIAS$Viol_Ameaça <- apply(QUANT_VIOLÊNCIAS, 1, function(x) any(grepl("Ameaça", paste(x, collapse = " "))))
QUANT_VIOLÊNCIAS$Viol_Patrimonial <- apply(QUANT_VIOLÊNCIAS, 1, function(x) any(grepl("Violência Patrimonial", paste(x, collapse = " "))))

QUANT_VIOLÊNCIAS <- QUANT_VIOLÊNCIAS[,-c(2:10)]


BD_V3 <- left_join(BD_V3, QUANT_VIOLÊNCIAS,
                           by = 'ID')


#Se possui filhos pequenos 

Idade_Filhos_V2 <- data.frame(
  ID = BD_V3$ID,
  Colune2 = BD_V3$Idade_Filhos)

Idade_Filhos_V2 <- Idade_Filhos_V2 %>% separate(Colune2, into = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8","A9"), sep = "[;]")


Idade_Filhos_V2$Filhos_Menores <- rowSums(sapply(Idade_Filhos_V2[, 2:10], function(x) grepl("\\bMenores\\b|\\b1\\b|\\b2\\b|\\b3\\b|\\b4\\b|\\b5\\b|\\b6\\b|\\b7\\b|\\b8\\b|\\b9\\b|\\b10\\b|\\b11\\b|\\b12\\b|\\b13\\b|\\b14\\b|\\b15\\b|\\b16\\b|\\b17\\b", x))) > 0


Idade_Filhos_V2 <- Idade_Filhos_V2[,-c(2:10)]

BD_V3 <- left_join(BD_V3, Idade_Filhos_V2,
                   by = 'ID')

BD_V3$Filhos_Menores <- ifelse(BD_V3$Filhos_Menores == "FALSE" | is.na(BD_V3$Filhos_Menores)| is.na(BD_V3$Idade_Filhos), "FALSE", BD_V3$Filhos_Menores)

#Separar Área de atendimento nas três principais (novas variáveis)

Área_Atendimento <- data.frame(
  ID = BD_V3$ID,
  Coluna2 = BD_V3$Área_Atendimento)

Área_Atendimento <- Área_Atendimento %>% separate(Coluna2, into = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8","A9"), sep = "[;]")

Área_Atendimento$Área_Jurídica <- apply(Área_Atendimento, 1, function(x) any(grepl("Direito", paste(x, collapse = " "))))
Área_Atendimento$Área_Psicossocial <- apply(Área_Atendimento, 1, function(x) any(grepl("Psicossocial", paste(x, collapse = " "))))
Área_Atendimento$Área_Socioassistencial <- apply(Área_Atendimento, 1, function(x) any(grepl("Serviço Social", paste(x, collapse = " "))))

Área_Atendimento <- Área_Atendimento[,-c(2:10)]

BD_V3 <- left_join(BD_V3, Área_Atendimento,
                   by = 'ID')

#Database sem as observações do Banco de Dados não-aleatório

BD_V4 <- BD_V3

BD_V4 <- BD_V4[-c(240:279),]

#Nova Variáveis 

BD_V4$Viol_Física <- ifelse(BD_V4$Viol_Física, "TRUE", "FALSE")
BD_V4$Viol_Psicológica <- ifelse(BD_V4$Viol_Psicológica, "TRUE", "FALSE")
BD_V4$Viol_Moral <- ifelse(BD_V4$Viol_Moral , "TRUE", "FALSE")
BD_V4$Viol_Ameaça <- ifelse(BD_V4$Viol_Ameaça, "TRUE", "FALSE")
BD_V4$Viol_Patrimonial <- ifelse(BD_V4$Viol_Patrimonial, "TRUE", "FALSE")
BD_V4$Filhos_Menores <- ifelse(BD_V4$Filhos_Menores, "TRUE", "FALSE")
BD_V4$Área_Jurídica <- ifelse(BD_V4$Área_Jurídica, "TRUE", "FALSE")
BD_V4$Área_Psicossocial <- ifelse(BD_V4$Área_Psicossocial, "TRUE", "FALSE")
BD_V4$Área_Socioassistencial <- ifelse(BD_V4$Área_Socioassistencial, "TRUE", "FALSE")
BD_V4$Tempo_Vinculada <- as.numeric(difftime(BD_V4$Data_Ultimo_Atendimento,BD_V4$Data_Atendimento_Inicial, units = "days"))

#Correção em IDs cujas datas estavam invertidas


BD_V4 <- left_join(BD_V4, Data_correta,
                   by = 'ID')

BD_V4 <- BD_V4[,-c(5:6)]


BD_V4 <- BD_V4 %>% rename("Data_Atendimento_Inicial" = P5,
                          "Data_Ultimo_Atendimento" = P6)

#Criando a variável da regressão logística

BD_V4 <- BD_V4 %>% mutate(REGRESSAO_MULTINOMIAL = case_when(
                                            BD_V4$Retornou == "Sim" & BD_V4$Reativou == "Não" ~ "Retornou",
                                            BD_V4$Reativou == "Sim" ~ "Reativou",
                                            BD_V4$Tempo_Vinculada != 0 & BD_V4$Reativou == "Não" ~ "Retornou",
                                            BD_V4$Tempo_Vinculada == 0 ~ "Primeiro_Atendimento",
                                            TRUE ~ NA_character_))

#Tratando variável possui dependentes e possui filhos

BD_V4 <- BD_V4 %>% mutate(DEPENDENTES_OU_FILHOSMENORES = case_when(
  is.na(Filhos_Menores) | is.na(Possui_Dependentes) ~ NA_character_,
  BD_V4$Filhos_Menores == "TRUE" | BD_V4$Possui_Dependentes == "Até 1 dependente" | BD_V4$Possui_Dependentes == "De 2 à 3 dependentes" | BD_V4$Possui_Dependentes == "Mais que 3 dependentes" ~ "Sim",
  TRUE ~ "Não"))


BD_V4$Filhos_Menores <- ifelse(BD_V4$Filhos_Menores == "FALSE" | is.na(BD_V4$Filhos_Menores)| is.na(BD_V4$Idade_Filhos), "FALSE", BD_V4$Filhos_Menores)

table(BD_V4$DEPENDENTES_OU_FILHOSMENORES)
table(BD_V4$Filhos_Menores)
table(BD_V4$Possui_Dependentes)

#Tratando nomes dos distritos

for (i in 1:nrow(Distritos_frequencia))
{
  Distritos_frequencia$`Rótulos de Linha`[i] <- iconv(Distritos_frequencia$`Rótulos de Linha`[i], to = "ASCII//TRANSLIT")
}


Distritos_frequencia <- Distritos_frequencia %>%
  mutate(`Rótulos de Linha` = toupper(`Rótulos de Linha`))

write.csv(Distritos_frequencia, "Distritos_frequencia.csv")

  
#Fim do script



