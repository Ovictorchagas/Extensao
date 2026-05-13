# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


#########################################################################
# SCRIPT COMPLETO - ETAPA 1: BANCO DE DADOS DO SINASC (Tarefas 1 a 11)
#########################################################################
library(dplyr)
library(stringr)

# =====================================================================
# Tarefa 1. Leitura do banco original do SINASC 2015
# =====================================================================
# Tente trocar o sep se o erro persistir (de ";" para ",")
dados_sinasc <- read.csv("SINASC_2015.csv", header = TRUE, sep = ";", dec = ",")

dados_sinasc_df <- dados_sinasc[substr(as.character(dados_sinasc$codmunres), 1, 2) == "53", ]

# Prepara a verificação para TNRC (Registros completos nas 61 variáveis originais)
completo_61 <- complete.cases(dados_sinasc_df)

# =====================================================================
# Tarefa 2. Reduzir para as 22 colunas selecionadas
# =====================================================================
colunas_selecionadas <- c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)
dados_sinasc_1 <- dados_sinasc_df[, colunas_selecionadas]

names(dados_sinasc_1) <- c("CONTADOR", "CODMUNNASC", "LOCNASC", "IDADEMAE", "ESTCIVMAE", 
                           "CODMUNRES", "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", 
                           "APGAR5", "RACACOR", "PESO", "IDANOMAL", "ESCMAE2010", 
                           "RACACORMAE", "SEMAGESTAC", "CONSPRENAT", "TPAPRESENT", 
                           "TPROBSON", "PARIDADE", "KOTELCHUCK")

# Prepara a verificação para TNRCR (Registros completos nas 22 variáveis)
completo_22 <- complete.cases(dados_sinasc_1)

# =====================================================================
# Tarefa 3. Criação do dados_sinasc_2 e marcadores de completude
# =====================================================================
dados_sinasc_2 <- dados_sinasc_1
dados_sinasc_2$COMPLETO_61 <- completo_61
dados_sinasc_2$COMPLETO_22 <- completo_22

# =====================================================================
# Tarefa 4. Frequências rápidas (opcional para visualização)
# =====================================================================
# table(dados_sinasc_2$LOCNASC, useNA = "always") 

# =====================================================================
# Tarefa 5. Tratamento de NAs (Categóricas e Quantitativas)
# =====================================================================
# Variáveis onde o código 9 (ou similar) representa 'Ignorado'
vars_ignorado_9 <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", 
                     "RACACOR", "RACACORMAE", "IDANOMAL", "ESCMAE2010", "TPAPRESENT", "KOTELCHUCK")

for(var in vars_ignorado_9) {
  dados_sinasc_2[[var]][dados_sinasc_2[[var]] == 9] <- NA
}

# Tratamento específico para as quantitativas e regras exatas
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] <- NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] <- NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] <- NA
dados_sinasc_2$SEMAGESTAC[dados_sinasc_2$SEMAGESTAC == 99] <- NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] <- NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON %in% c(11, 12)] <- NA

# SEXO (0 e 9 são ignorados)
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0 | dados_sinasc_2$SEXO == 9] <- NA

# =====================================================================
# Tarefa 6. Legendas das categorias (Transformar em Fatores)
# =====================================================================
dados_sinasc_2$LOCNASC <- factor(dados_sinasc_2$LOCNASC, levels = 1:5, labels = c("Hospital", "Outro estabelecimento", "Domicílio", "Outros", "Aldeia Indígena"))
dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE, levels = 1:5, labels = c("Solteira", "Casada", "Viúva", "Separada/Divorciada", "União consensual"))
dados_sinasc_2$GESTACAO <- factor(dados_sinasc_2$GESTACAO, levels = 1:6, labels = c("Menos de 22 sem", "22-27 sem", "28-31 sem", "32-36 sem", "37-41 sem", "42+ sem"))
dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ, levels = 1:3, labels = c("Única", "Dupla", "Tríplice+"))
dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO, levels = 1:2, labels = c("Vaginal", "Cesáreo"))
dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR, levels = 1:5, labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$RACACORMAE <- factor(dados_sinasc_2$RACACORMAE, levels = 1:5, labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL, levels = 1:2, labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 <- factor(dados_sinasc_2$ESCMAE2010, levels = 0:5, labels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", "Superior completo"))
dados_sinasc_2$TPAPRESENT <- factor(dados_sinasc_2$TPAPRESENT, levels = 1:3, labels = c("Cefálica", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$PARIDADE <- factor(dados_sinasc_2$PARIDADE, levels = 0:1, labels = c("Nenhuma", "Uma ou mais"))
dados_sinasc_2$KOTELCHUCK <- factor(dados_sinasc_2$KOTELCHUCK, levels = 1:5, labels = c("Não realizou", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))
dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO, levels = c(1, 2), labels = c("Masculino", "Feminino"))

# =====================================================================
# Tarefa 7. Criação das variáveis adicionais (F_PESO, F_IDADE, etc)
# =====================================================================
dados_sinasc_2$F_PESO <- factor(ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso", ifelse(dados_sinasc_2$PESO < 4000, "Peso normal", "Macrossomia")))
dados_sinasc_2$F_IDADE <- cut(dados_sinasc_2$IDADEMAE, breaks = c(0, 14.9, 19.9, 24.9, 29.9, 34.9, 39.9, 44.9, 49.9, 100), labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"))
dados_sinasc_2$F_APGAR5 <- factor(ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal"))

# Novas variáveis solicitadas na correção (Peregrinação e Estado Civil)
dados_sinasc_2$PERIG <- factor(ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$ESTCIV <- factor(ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União consensual"), "Com companheiro", "Sem companheiro"))

# =====================================================================
# Tarefa 8. Tabela PIG e criação de F_PIG (Completamente Corrigido)
# =====================================================================
tabela_pig <- read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep = ";")
tabela_pig$SEXO <- factor(trimws(tabela_pig$SEXO), levels = c("Masculino", "Feminino"))

# Merge funcionará agora porque o SEXO no dados_sinasc_2 já virou Fator na Tarefa 6
dados_sinasc_2 <- merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = TRUE)

# Criação de F_PIG respeitando Gravidez Única e lidando com NAs
dados_sinasc_2$F_PIG <- ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                               ifelse(is.na(dados_sinasc_2$PESO) | is.na(dados_sinasc_2$PESO_P10) | is.na(dados_sinasc_2$PESO_P90), NA,
                                      ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                             ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG", "GIG")
                                      )
                               )
)
dados_sinasc_2$F_PIG <- factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))

# Escrever o arquivo para eventuais validações (conclui Tarefa 8)
write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

# =====================================================================
# Tarefas 9, 10 e 11 (REFORMULADAS). Agregação de 103 colunas (SINASC_DF)
# =====================================================================

# Função com todas as 103 métricas obrigatórias
resumo_103_variaveis <- function(df) {
  df %>% summarise(
    TN = n(),
    TNRC = sum(COMPLETO_61, na.rm = TRUE),
    TNRCR = sum(COMPLETO_22, na.rm = TRUE),
    
    TGI_15 = sum(IDADEMAE < 15, na.rm = TRUE),
    TGI_15_19 = sum(IDADEMAE >= 15 & IDADEMAE <= 19, na.rm = TRUE),
    TGI_20_24 = sum(IDADEMAE >= 20 & IDADEMAE <= 24, na.rm = TRUE),
    TGI_25_29 = sum(IDADEMAE >= 25 & IDADEMAE <= 29, na.rm = TRUE),
    TGI_30_34 = sum(IDADEMAE >= 30 & IDADEMAE <= 34, na.rm = TRUE),
    TGI_35_39 = sum(IDADEMAE >= 35 & IDADEMAE <= 39, na.rm = TRUE),
    TGI_40_44 = sum(IDADEMAE >= 40 & IDADEMAE <= 44, na.rm = TRUE),
    TGI_45_49 = sum(IDADEMAE >= 45 & IDADEMAE <= 49, na.rm = TRUE),
    TGI_50 = sum(IDADEMAE >= 50, na.rm = TRUE),
    TGIF = sum(IDADEMAE >= 15 & IDADEMAE <= 49, na.rm = TRUE),
    
    IM_P25 = quantile(IDADEMAE, 0.25, na.rm = TRUE),
    IM_P50 = quantile(IDADEMAE, 0.50, na.rm = TRUE),
    IM_P75 = quantile(IDADEMAE, 0.75, na.rm = TRUE),
    IM_MD = mean(IDADEMAE, na.rm = TRUE),
    IM_DP = sd(IDADEMAE, na.rm = TRUE),
    
    EM_S = sum(ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
    EM_FI = sum(ESCMAE2010 == "Fundamental I", na.rm = TRUE),
    EM_FII = sum(ESCMAE2010 == "Fundamental II", na.rm = TRUE),
    EM_M = sum(ESCMAE2010 == "Médio", na.rm = TRUE),
    EM_SI = sum(ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(ESCMAE2010 == "Superior completo", na.rm = TRUE),
    
    TGRC_B = sum(RACACORMAE == "Branca", na.rm = TRUE),
    TGRC_PT = sum(RACACORMAE == "Preta", na.rm = TRUE),
    TGRC_A = sum(RACACORMAE == "Amarela", na.rm = TRUE),
    TGRC_PD = sum(RACACORMAE == "Parda", na.rm = TRUE),
    TGRC_I = sum(RACACORMAE == "Indígena", na.rm = TRUE),
    
    TGSC = sum(ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(ESTCIV == "Com companheiro", na.rm = TRUE),
    TGPRI = sum(PARIDADE == "Nenhuma", na.rm = TRUE),
    TGNPRI = sum(PARIDADE == "Uma ou mais", na.rm = TRUE),
    
    TGU = sum(GRAVIDEZ == "Única", na.rm = TRUE),
    TGG = sum(GRAVIDEZ %in% c("Dupla", "Tríplice+"), na.rm = TRUE),
    
    TGD_22 = sum(GESTACAO == "Menos de 22 sem", na.rm = TRUE),
    TGD_22_27 = sum(GESTACAO == "22-27 sem", na.rm = TRUE),
    TGD_28_31 = sum(GESTACAO == "28-31 sem", na.rm = TRUE),
    TGD_32_36 = sum(GESTACAO == "32-36 sem", na.rm = TRUE),
    TGD_37_41 = sum(GESTACAO == "37-41 sem", na.rm = TRUE),
    TGD_42 = sum(GESTACAO == "42+ sem", na.rm = TRUE),
    
    TGD_PRT = sum(SEMAGESTAC < 37, na.rm = TRUE),
    TGD_AT = sum(SEMAGESTAC >= 37 & SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_PST = sum(SEMAGESTAC >= 42, na.rm = TRUE),
    
    DG_P25 = quantile(SEMAGESTAC, 0.25, na.rm = TRUE),
    DG_P50 = quantile(SEMAGESTAC, 0.50, na.rm = TRUE),
    DG_P75 = quantile(SEMAGESTAC, 0.75, na.rm = TRUE),
    DG_MD = mean(SEMAGESTAC, na.rm = TRUE),
    DG_DP = sd(SEMAGESTAC, na.rm = TRUE),
    
    TKC_NR = sum(KOTELCHUCK == "Não realizou", na.rm = TRUE),
    TKC_ID = sum(KOTELCHUCK == "Inadequado", na.rm = TRUE),
    TKC_IT = sum(KOTELCHUCK == "Intermediário", na.rm = TRUE),
    TKC_AD = sum(KOTELCHUCK == "Adequado", na.rm = TRUE),
    TKC_MAD = sum(KOTELCHUCK == "Mais que adequado", na.rm = TRUE),
    
    TGPRG_S = sum(PERIG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(PERIG == "Não", na.rm = TRUE),
    
    TPV = sum(PARTO == "Vaginal", na.rm = TRUE),
    TPC = sum(PARTO == "Cesáreo", na.rm = TRUE),
    
    TRAP_C = sum(TPAPRESENT == "Cefálica", na.rm = TRUE),
    TRAP_P = sum(TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
    TRAP_T = sum(TPAPRESENT == "Transversa", na.rm = TRUE),
    
    TGROB_1 = sum(TPROBSON == 1, na.rm = TRUE),
    TGROB_2 = sum(TPROBSON == 2, na.rm = TRUE),
    TGROB_3 = sum(TPROBSON == 3, na.rm = TRUE),
    TGROB_4 = sum(TPROBSON == 4, na.rm = TRUE),
    TGROB_5 = sum(TPROBSON == 5, na.rm = TRUE),
    TGROB_6 = sum(TPROBSON == 6, na.rm = TRUE),
    TGROB_7 = sum(TPROBSON == 7, na.rm = TRUE),
    TGROB_8 = sum(TPROBSON == 8, na.rm = TRUE),
    TGROB_9 = sum(TPROBSON == 9, na.rm = TRUE),
    TGROB_10 = sum(TPROBSON == 10, na.rm = TRUE),
    
    TNLOC_H = sum(LOCNASC == "Hospital", na.rm = TRUE),
    TNLOC_ES = sum(LOCNASC == "Outro estabelecimento", na.rm = TRUE),
    TNLOC_D = sum(LOCNASC == "Domicílio", na.rm = TRUE),
    TNLOC_O = sum(LOCNASC == "Outros", na.rm = TRUE),
    TNLOC_AI = sum(LOCNASC == "Aldeia Indígena", na.rm = TRUE),
    
    TRS_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TRS_F = sum(SEXO == "Feminino", na.rm = TRUE),
    
    TRRC_B = sum(RACACOR == "Branca", na.rm = TRUE),
    TRRC_PT = sum(RACACOR == "Preta", na.rm = TRUE),
    TRRC_A = sum(RACACOR == "Amarela", na.rm = TRUE),
    TRRC_PD = sum(RACACOR == "Parda", na.rm = TRUE),
    TRRC_I = sum(RACACOR == "Indígena", na.rm = TRUE),
    
    TRP_BP = sum(F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N = sum(F_PESO == "Peso normal", na.rm = TRUE),
    TRP_M = sum(F_PESO == "Macrossomia", na.rm = TRUE),
    
    PESO_P25 = quantile(PESO, 0.25, na.rm = TRUE),
    PESO_P50 = quantile(PESO, 0.50, na.rm = TRUE),
    PESO_P75 = quantile(PESO, 0.75, na.rm = TRUE),
    PESO_MD = mean(PESO, na.rm = TRUE),
    PESO_DP = sd(PESO, na.rm = TRUE),
    
    TRPIG_P = sum(F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(F_PIG == "GIG", na.rm = TRUE),
    
    TRAPG5_B = sum(F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(F_APGAR5 == "Normal", na.rm = TRUE),
    
    APG5_MD = mean(APGAR5, na.rm = TRUE),
    APG5_DP = sd(APGAR5, na.rm = TRUE),
    
    TRAC = sum(IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE)
  )
}

# Agregação por Municípios com a correção de CODMUNRES como caractere (evita erro no bind)
resumo_municipios <- dados_sinasc_2 %>%
  mutate(CODMUNRES = as.character(CODMUNRES)) %>% 
  group_by(CODMUNRES) %>%
  resumo_103_variaveis() %>%
  mutate(ANO = 2015, NIVEL = "MUNICIPIO") %>%
  ungroup() %>%
  select(ANO, NIVEL, CODMUNRES, everything())

# Agregação da UF (agrupando todos os municípios)
resumo_uf <- dados_sinasc_2 %>%
  mutate(CODMUNRES = "53") %>% 
  group_by(CODMUNRES) %>%
  resumo_103_variaveis() %>%
  mutate(ANO = 2015, NIVEL = "UF") %>%
  ungroup() %>%
  select(ANO, NIVEL, CODMUNRES, everything())

# Empilhar linhas e Exportar
SINASC_DF <- bind_rows(resumo_uf, resumo_municipios)

# Salva o arquivo CSV definitivo das tarefas 9 a 11
write.csv(SINASC_DF, "SINASC_DF.csv", row.names = FALSE)

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com
# 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim
dados_sim <- read.csv("Mortalidade_Geral_2015.csv", header = TRUE, sep = ";", dec = ",")

# Verificar se a leitura foi feita corretamente (Imprime no console a estrutura)
str(dados_sim)
dim(dados_sim)

# ATENÇÃO: O cômputo de registros completos (TORC) com base nas 87 variáveis originais
# deve ser feito ANTES do subsetting da Tarefa 2 para evitar perda de dados.
completo_87 <- complete.cases(dados_sim[, 1:87])

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas,
# nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC,
# IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, OBITOGRAV,
# OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
colunas_sim <- c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)
dados_sim_1 <- dados_sim[, colunas_sim]

# Nomeando as variáveis rigorosamente conforme o solicitado
names(dados_sim_1) <- c("CONTADOR", "TIPOBITO", "DTOBITO", "DTNASC", "IDADE", "SEXO", 
                        "RACACOR", "ESC2010", "CODMUNRES", "TPMORTEOCO", "OBITOGRAV", 
                        "OBITOPUERP", "CAUSABAS", "TPOBITOCOR", "MORTEPARTO")

# Criar marcador de registros completos para as variáveis selecionadas (TORCR)
completo_selecionadas <- complete.cases(dados_sim_1)

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar
# (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de
# dados como dados_sim_2
dados_sim_2 <- subset(dados_sim_1, substr(as.character(CODMUNRES), 1, 2) == "53")

# Repassando os vetores de completude exclusivamente para as linhas filtradas
filtro_uf <- substr(as.character(dados_sim_1$CODMUNRES), 1, 2) == "53"
dados_sim_2$COMPLETO_87 <- completo_87[filtro_uf]
dados_sim_2$COMPLETO_SELECIONADAS <- completo_selecionadas[filtro_uf]

# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes
# variáveis: TIPOBITO, SEXO, RACACOR, TPMORTEOCO, OBITOGRAV, OBITOPUERP,
# CAUSABAS, TPOBITOCOR, MORTEPARTO
table(dados_sim_2$TIPOBITO, useNA = "always")
table(dados_sim_2$SEXO, useNA = "always")
table(dados_sim_2$RACACOR, useNA = "always")
table(dados_sim_2$TPMORTEOCO, useNA = "always")
table(dados_sim_2$OBITOGRAV, useNA = "always")
table(dados_sim_2$OBITOPUERP, useNA = "always")
table(dados_sim_2$CAUSABAS, useNA = "always")
table(dados_sim_2$TPOBITOCOR, useNA = "always")
table(dados_sim_2$MORTEPARTO, useNA = "always")

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria
# de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada
# variável. Em variáveis quantitativas como IDADE verificar se existem valores como
# 99 para NA
vars_ignorado_9 <- c("SEXO", "RACACOR", "ESC2010", "TPMORTEOCO", "OBITOGRAV", 
                     "OBITOPUERP", "TPOBITOCOR", "MORTEPARTO")

for(var in vars_ignorado_9) {
  dados_sim_2[[var]][dados_sim_2[[var]] == 9] <- NA
}

# Tratamento para as particularidades do OpenDatasus
dados_sim_2$SEXO[dados_sim_2$SEXO == 0] <- NA        # 0 em SEXO também é Ignorado
dados_sim_2$IDADE[dados_sim_2$IDADE == 999] <- NA    # 999 em IDADE é Ignorado

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas
#investigadas na tarefa 4. Exemplo: dados_sim_2$TIPOBITO = 
#factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")
dados_sim_2$TIPOBITO <- factor(dados_sim_2$TIPOBITO, levels = c(1, 2), labels = c("Fetal", "Não fetal"))
dados_sim_2$SEXO <- factor(dados_sim_2$SEXO, levels = c(1, 2), labels = c("Masculino", "Feminino"))
dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR, levels = 1:5, labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sim_2$ESC2010 <- factor(dados_sim_2$ESC2010, levels = 0:5, labels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", "Superior completo"))

dados_sim_2$TPMORTEOCO <- factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5, 8), 
                                 labels = c("Na gravidez", "No parto", "No abortamento", 
                                            "Até 42 dias após", "De 43 dias a 1 ano", "Não ocorreu nestas fases"))

dados_sim_2$OBITOGRAV <- factor(dados_sim_2$OBITOGRAV, levels = c(1, 2), labels = c("Sim", "Não"))
dados_sim_2$OBITOPUERP <- factor(dados_sim_2$OBITOPUERP, levels = 1:3, labels = c("Sim, até 42 dias", "Sim, de 43 dias a 1 ano", "Não"))
dados_sim_2$TPOBITOCOR <- factor(dados_sim_2$TPOBITOCOR, levels = 1:5, labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Via pública", "Outros"))
dados_sim_2$MORTEPARTO <- factor(dados_sim_2$MORTEPARTO, levels = 1:3, labels = c("Sim", "Não", "Não aplicável"))

# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv),
# contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da
# Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias
# e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE
dados_sim_2$IDADE_STR <- sprintf("%03d", as.numeric(dados_sim_2$IDADE))
dados_sim_2$IDADE_TIPO <- as.numeric(substr(dados_sim_2$IDADE_STR, 1, 1))
dados_sim_2$IDADE_VALOR <- as.numeric(substr(dados_sim_2$IDADE_STR, 2, 3))

# Converte Idade para Anos absolutos (útil para faixa fértil 15 a 49)
dados_sim_2$IDADE_ANOS <- ifelse(dados_sim_2$IDADE_TIPO == 4, dados_sim_2$IDADE_VALOR,
                                 ifelse(dados_sim_2$IDADE_TIPO == 5, dados_sim_2$IDADE_VALOR + 100,
                                        ifelse(dados_sim_2$IDADE_TIPO %in% c(0, 1, 2, 3), 0, NA)))

# Converte Idade para Dias absolutos (útil para a fase neonatal 0 a 27)
dados_sim_2$IDADE_DIAS <- ifelse(dados_sim_2$IDADE_TIPO %in% c(0, 1), 0,
                                 ifelse(dados_sim_2$IDADE_TIPO == 2, dados_sim_2$IDADE_VALOR,
                                        ifelse(dados_sim_2$IDADE_TIPO == 3, dados_sim_2$IDADE_VALOR * 30,
                                               ifelse(dados_sim_2$IDADE_TIPO == 4, dados_sim_2$IDADE_VALOR * 365,
                                                      ifelse(dados_sim_2$IDADE_TIPO == 5, (dados_sim_2$IDADE_VALOR + 100) * 365, NA)))))

# PREPARAÇÃO 2: CID-10 (Capturar o capítulo através da primeira letra)
dados_sim_2$CAP_CID <- substr(as.character(dados_sim_2$CAUSABAS), 1, 1)

# Função central para cálculo matemático exato das variáveis agregadas
resumo_sim_vars <- function(df) {
  df %>% summarise(
    # --- Identificadores e Registros Completos ---
    TO = n(),
    TORC = sum(COMPLETO_87, na.rm = TRUE),
    TORCR = sum(COMPLETO_SELECIONADAS, na.rm = TRUE),
    
    # --- 1. Informações Gerais (CAUSABAS) ---
    TO_NN = sum(CAP_CID %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_N = sum(!CAP_CID %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_CBI = sum(CAP_CID %in% c("A", "B"), na.rm = TRUE),
    TO_CB_N = sum(CAP_CID %in% c("C", "D"), na.rm = TRUE),
    TO_CB_C = sum(CAP_CID == "I", na.rm = TRUE),
    TO_CB_R = sum(CAP_CID == "J", na.rm = TRUE),
    TO_CB_O = sum(!CAP_CID %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y"), na.rm = TRUE),
    
    # --- Informações Gerais (SEXO e IDADE) ---
    TO_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TO_F = sum(SEXO == "Feminino", na.rm = TRUE),
    TO_F_IF = sum(SEXO == "Feminino" & IDADE_ANOS >= 15 & IDADE_ANOS <= 49, na.rm = TRUE),
    
    # --- 2 e 3. Fetais e Neonatais (TIPOBITO, IDADE_DIAS e RACACOR) ---
    TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE),
    TO_NT = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 0 & IDADE_DIAS <= 27, na.rm = TRUE),
    TO_NT_P = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 0 & IDADE_DIAS <= 6, na.rm = TRUE),
    TO_NT_T = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 7 & IDADE_DIAS <= 27, na.rm = TRUE),
    TO_PNT = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 28 & IDADE_DIAS <= 364, na.rm = TRUE),
    TONT_B = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 0 & IDADE_DIAS <= 27 & RACACOR == "Branca", na.rm = TRUE),
    TONT_PT = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 0 & IDADE_DIAS <= 27 & RACACOR == "Preta", na.rm = TRUE),
    TONT_A = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 0 & IDADE_DIAS <= 27 & RACACOR == "Amarela", na.rm = TRUE),
    TONT_PD = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 0 & IDADE_DIAS <= 27 & RACACOR == "Parda", na.rm = TRUE),
    TONT_I = sum(TIPOBITO == "Não fetal" & IDADE_DIAS >= 0 & IDADE_DIAS <= 27 & RACACOR == "Indígena", na.rm = TRUE),
    
    # --- 4. Informações Maternas (TPMORTEOCO, ESC e IDADE) ---
    TO_MT = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após", "De 43 dias a 1 ano"), na.rm = TRUE),
    TO_MT_DG = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
    TO_MT_PT = sum(TPMORTEOCO == "No parto", na.rm = TRUE),
    TO_MT_AB = sum(TPMORTEOCO == "No abortamento", na.rm = TRUE),
    TO_MT_42 = sum(TPMORTEOCO == "Até 42 dias após", na.rm = TRUE),
    TO_MT_43 = sum(TPMORTEOCO == "De 43 dias a 1 ano", na.rm = TRUE),
    TO_MT_P = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após"), na.rm = TRUE),
    TO_MT_P_I = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após") & IDADE_ANOS >= 15 & IDADE_ANOS <= 49, na.rm = TRUE),
    TO_MT_P_ES = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após") & ESC2010 == "Sem escolaridade", na.rm = TRUE),
    TO_MT_P_EFI = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após") & ESC2010 == "Fundamental I", na.rm = TRUE),
    TO_MT_P_EFII = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após") & ESC2010 == "Fundamental II", na.rm = TRUE),
    TO_MT_P_EM = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após") & ESC2010 == "Médio", na.rm = TRUE),
    TO_MT_P_ESI = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após") & ESC2010 == "Superior incompleto", na.rm = TRUE),
    TO_MT_P_ESC = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após") & ESC2010 == "Superior completo", na.rm = TRUE)
  )
}

# Realizar o Agrupamento Nível Município
resumo_sim_municipios <- dados_sim_2 %>%
  mutate(CODMUNRES = as.character(CODMUNRES)) %>% 
  group_by(CODMUNRES) %>%
  resumo_sim_vars() %>%
  mutate(ANO = 2015, NIVEL = "MUNICIPIO") %>%
  ungroup() %>%
  select(ANO, NIVEL, CODMUNRES, everything())

# Realizar o Agrupamento Nível Estado Total (UF)
resumo_sim_uf <- dados_sim_2 %>%
  mutate(CODMUNRES = "53") %>%  # Adaptar para a sua UF
  group_by(CODMUNRES) %>%
  resumo_sim_vars() %>%
  mutate(ANO = 2015, NIVEL = "UF") %>%
  ungroup() %>%
  select(ANO, NIVEL, CODMUNRES, everything())

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv
# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o
# comentário "Dados da UF e Script Etapa 2"# Faça um merge de script de SIM
# para main
SIM_DF <- bind_rows(resumo_sim_uf, resumo_sim_municipios)
write.csv(SIM_DF, "SIM_DF.csv", row.names = FALSE)


#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Ao terminar a ETAPA 2 faça um merge de SIM para main
# Altere as orientações do script e commit (em main) "Script com orientações ETAPA 3 - SIDRA"
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos da Tarefa 1 abaixo

# Carregando pacote necessário para manipulação dos dados
library(dplyr)

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
pop_est_2015   <- read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv", sep=";", header=TRUE)
pop_censo_tot  <- read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv", sep=";", header=TRUE)
pop_idade_uf   <- read.csv("população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv", sep=";", header=TRUE)
pop_idade_mun  <- read.csv("população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv", sep=";", header=TRUE)

f_15    <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")
f_15_49 <- c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")
f_50    <- c("50 a 54 anos", "55 a 59 anos", "60 a 64 anos", "65 a 69 anos", "70 a 74 anos", "75 a 79 anos", "80 a 89 anos", "90 a 99 anos", "100 anos ou mais")


# Filtrando os bancos de dados
df_est_uf   <- pop_est_2015 %>% filter(CODMUNRES == 53)
df_tot_uf   <- pop_censo_tot %>% filter(CODMUNRES == 53)
df_idade_uf <- pop_idade_uf %>% filter(CODMUNRES == 53)

# Calculando as faixas etárias requeridas (total e por sexo feminino)
calc_uf <- df_idade_uf %>%
  summarise(
    POPRC_15      = sum(POP[F_IDADE %in% f_15], na.rm = TRUE),
    POPRC_15_49   = sum(POP[F_IDADE %in% f_15_49], na.rm = TRUE),
    POPRC_50      = sum(POP[F_IDADE %in% f_50], na.rm = TRUE),
    POPRC_F_15    = sum(POPF[F_IDADE %in% f_15], na.rm = TRUE),
    POPRC_F_15_49 = sum(POPF[F_IDADE %in% f_15_49], na.rm = TRUE),
    POPRC_F_50    = sum(POPF[F_IDADE %in% f_50], na.rm = TRUE)
  )

# Criando a linha de informações do Estado
linha_uf <- data.frame(
  ANO           = 2015,
  NIVEL         = "UF",
  CODMUNRES     = 53,
  POPRE_T       = df_est_uf$POPRE_T,
  POPRC_T       = df_tot_uf$POPRC_T,
  POPRC_M       = df_tot_uf$POPRC_M,
  POPRC_F       = df_tot_uf$POPRC_F,
  POPRC_15      = calc_uf$POPRC_15,
  POPRC_15_49   = calc_uf$POPRC_15_49,
  POPRC_50      = calc_uf$POPRC_50,
  POPRC_F_15    = calc_uf$POPRC_F_15,
  POPRC_F_15_49 = calc_uf$POPRC_F_15_49,
  POPRC_F_50    = calc_uf$POPRC_F_50
)

# Filtrando os bancos de dados
df_est_mun   <- pop_est_2015 %>% filter(CODMUNRES == 5300108)
df_tot_mun   <- pop_censo_tot %>% filter(CODMUNRES == 5300108)
df_idade_mun <- pop_idade_mun %>% filter(CODMUNRES == 5300108)

# Calculando as faixas etárias
calc_mun <- df_idade_mun %>%
  summarise(
    POPRC_15      = sum(POP[F_IDADE %in% f_15], na.rm = TRUE),
    POPRC_15_49   = sum(POP[F_IDADE %in% f_15_49], na.rm = TRUE),
    POPRC_50      = sum(POP[F_IDADE %in% f_50], na.rm = TRUE),
    POPRC_F_15    = sum(POPF[F_IDADE %in% f_15], na.rm = TRUE),
    POPRC_F_15_49 = sum(POPF[F_IDADE %in% f_15_49], na.rm = TRUE),
    POPRC_F_50    = sum(POPF[F_IDADE %in% f_50], na.rm = TRUE)
  )

# Criando a linha de informações do Município
linha_mun <- data.frame(
  ANO           = 2015,
  NIVEL         = "MUNICIPIO",
  CODMUNRES     = 5300108,
  POPRE_T       = df_est_mun$POPRE_T,
  POPRC_T       = df_tot_mun$POPRC_T,
  POPRC_M       = df_tot_mun$POPRC_M,
  POPRC_F       = df_tot_mun$POPRC_F,
  POPRC_15      = calc_mun$POPRC_15,
  POPRC_15_49   = calc_mun$POPRC_15_49,
  POPRC_50      = calc_mun$POPRC_50,
  POPRC_F_15    = calc_mun$POPRC_F_15,
  POPRC_F_15_49 = calc_mun$POPRC_F_15_49,
  POPRC_F_50    = calc_mun$POPRC_F_50
)

# Empilhando as duas linhas em um único banco de dados de nome SIDRA_UF
SIDRA_UF <- bind_rows(linha_uf, linha_mun)

# Exporte o arquivo em formato CSV
write.csv(SIDRA_UF, "SIDRA_DF.csv", row.names = FALSE)

#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

