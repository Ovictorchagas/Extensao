# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# TAREFA 1: Leitura do banco de dados do SINASC 2015
# Importante: O separador padrão do arquivo do OpenDATASUS costuma ser ";"
dados_sinasc <- read.csv("SINASC_2015.csv", header = TRUE, sep = ";")

# TAREFA 2: Redução do banco para as 22 colunas especificadas
colunas_selecionadas <- c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)
dados_sinasc_1 <- dados_sinasc[, colunas_selecionadas]

# TAREFA 3: Filtrar apenas nascimentos do Distrito Federal (UF 53)
# Extraímos os dois primeiros dígitos do código do município de residência
dados_sinasc_2 <- subset(dados_sinasc_1, substr(CODMUNRES, 1, 2) == "53")

# TAREFA 4: Verificar frequências (opcional para conferência, mas bom manter)
# table(dados_sinasc_2$SEXO)

# TAREFA 5: Atribuição de NA para valores "Ignorados" ou "Não Informados"
# Segundo o dicionário, o código 9 (e às vezes o 0 no sexo) representam dados ausentes
vars_9 <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", 
            "RACACOR", "IDANOMAL", "ESCMAE2010", "RACACORMAE", "TPAPRESENT", "KOTELCHUCK")

for(v in vars_9) {
  dados_sinasc_2[[v]][dados_sinasc_2[[v]] == 9] <- NA
}
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] <- NA

# TAREFA 6: Atribuição de Legendas (Factors)
# Transformando códigos numéricos em palavras para facilitar a análise
dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO, levels = c(1, 2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ, levels = c(1, 2, 3), labels = c("Única", "Dupla", "Três ou mais"))
dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO, levels = c(1, 2), labels = c("Vaginal", "Cesáreo"))
dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL, levels = c(1, 2), labels = c("Sim", "Não"))

# TAREFA 7: Criação de Variáveis Categorizadas (Faixas)
# 7.1. F_PESO (Critério de Baixo Peso e Macrossomia)
dados_sinasc_2$F_PESO <- cut(dados_sinasc_2$PESO, 
                             breaks = c(-Inf, 2499, 3999, Inf), 
                             labels = c("Baixo peso", "Peso normal", "Macrossomia"))

# 7.2. F_IDADE (Faixas etárias da mãe)
dados_sinasc_2$F_IDADE <- cut(dados_sinasc_2$IDADEMAE, 
                              breaks = c(-Inf, 14, 19, 24, 29, 34, 39, 44, 49, Inf),
                              labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"))

# 7.3. F_APGAR5 (Apgar de 5º minuto: <7 é considerado asfixia/baixo)
dados_sinasc_2$F_APGAR5 <- ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 <- as.factor(dados_sinasc_2$F_APGAR5)

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



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

