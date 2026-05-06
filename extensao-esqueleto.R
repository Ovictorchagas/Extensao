# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


#########################################################################
# SCRIPT COMPLETO - ETAPA 1: BANCO DE DADOS DO SINASC (Tarefas 1 a 11)
#########################################################################
library(dplyr)

# =====================================================================
# Tarefa 1. Leitura do banco original do SINASC 2015
# =====================================================================
dados_sinasc <- read.csv("SINASC_2015.csv", header = TRUE, sep = ";", dec = ",") 

# Filtrar o banco original inteiro para o DF (código 53) antes de excluir colunas
dados_sinasc_df <- subset(dados_sinasc, substr(as.character(CODMUNRES), 1, 2) == "53")

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

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas,
# nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC,
# IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, OBITOGRAV,
# OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar
# (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de
# dados como dados_sim_2

# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes
#variáveis: TIPOBITO, SEXO, RACACOR, TPMORTEOCO, OBITOGRAV, OBITOPUERP,
# CAUSABAS, TPOBITOCOR, MORTEPARTO

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria
# de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada
# variável. Em variáveis quantitativas como IDADE verificar se existem valores como
# 99 para NA

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas
#investigadas na tarefa 4. Exemplo: dados_sim_2$TIPOBITO =
#factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")
         
         # Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv),
         # contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da
         # Etapa 2.pdf”
         # Atenção:
         # 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
         # 2. Para informações fetais utilize TIPOBITO
         # 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias
         # e RACACOR
         # 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE
         
         # Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv
         # Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o
         # comentário "Dados da UF e Script Etapa 2"# Faça um merge de script de SIM
         # para main
#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



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

