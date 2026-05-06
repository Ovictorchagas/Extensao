# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# Tarefa 1. Leitura do banco de dados do SINASC 2015
dados_sinasc <- read.csv("SINASC_2015.csv", header = TRUE, sep = ";", dec = ",") 

# Tarefa 2. Reduzir para as 22 colunas selecionadas
colunas_selecionadas <- c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)
dados_sinasc_1 <- dados_sinasc[, colunas_selecionadas]

names(dados_sinasc_1) <- c("CONTADOR", "CODMUNNASC", "LOCNASC", "IDADEMAE", "ESTCIVMAE", 
                           "CODMUNRES", "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", 
                           "APGAR5", "RACACOR", "PESO", "IDANOMAL", "ESCMAE2010", 
                           "RACACORMAE", "SEMAGESTAC", "CONSPRENAT", "TPAPRESENT", 
                           "TPROBSON", "PARIDADE", "KOTELCHUCK")

# Tarefa 3. Reduzir apenas para o DF e exportar
dados_sinasc_2 <- subset(dados_sinasc_1, substr(as.character(CODMUNRES), 1, 2) == "53")
write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

# Tarefa 4. (Frequências verificadas via table())

# Tarefa 5. Atribuir NA para "Ignorado" (Atendendo ao Item 4 do Feedback)
vars_ignorado_9 <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", 
                     "RACACOR", "RACACORMAE", "IDANOMAL", "ESCMAE2010", "TPAPRESENT", "KOTELCHUCK")

for(var in vars_ignorado_9) {
  dados_sinasc_2[[var]][dados_sinasc_2[[var]] == 9] <- NA
}

# Tratamento específico para evitar erro na categorização de idade e pré-natal
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] <- NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] <- NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] <- NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 9] <- NA # Correção Item 4
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] <- NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == "I" | dados_sinasc_2$SEXO == 9] <- NA

# Tarefa 6. Atribuir legendas (Labels)
dados_sinasc_2$LOCNASC <- factor(dados_sinasc_2$LOCNASC, levels = 1:4, labels = c("Hospital", "Outro estabelecimento", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE, levels = 1:5, labels = c("Solteira", "Casada", "Viúva", "Separada/Divorciada", "União consensual"))
dados_sinasc_2$GESTACAO <- factor(dados_sinasc_2$GESTACAO, levels = 1:6, labels = c("Menos de 22 sem", "22-27 sem", "28-31 sem", "32-36 sem", "37-41 sem", "42+ sem"))
dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ, levels = 1:3, labels = c("Única", "Dupla", "Tríplice+"))
dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO, levels = 1:2, labels = c("Vaginal", "Cesáreo"))
dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO, levels = c("M", "F"), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR, levels = 1:5, labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL, levels = 1:2, labels = c("Sim", "Não"))

# Tarefa 7. Criar as 5 novas variáveis (Totalizando 27 no banco)
dados_sinasc_2$F_PESO <- factor(ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso", ifelse(dados_sinasc_2$PESO < 4000, "Peso normal", "Macrossomia")))
dados_sinasc_2$F_IDADE <- cut(dados_sinasc_2$IDADEMAE, breaks = c(0, 14, 19, 24, 29, 34, 39, 44, 49, 100), labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"))
dados_sinasc_2$F_APGAR5 <- factor(ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal"))
dados_sinasc_2$PERIG <- factor(ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$ESTCIV <- factor(ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União consensual"), "Com companheiro", "Sem companheiro"))


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.


# Tarefa 9. Obter as frequências das categorias das variáveis qualitativas e medidas descritivas de variáveis quantitativas e salvar os resultados em novas variáveis.
# Exemplo: freq_SEXO = table(dados_sinasc_2$SEXO)   media_peso = mean(dados_sinasc_2$PESO)
# Medidas descritivas a serem calculadas para variáveis QUANTITATIVAS: P25, P50, P75, média e desvio-padrão. Atenção: usar na.rm = TRUE, quando necessário.


# Tarefa 10. Criar as colunas do novo banco de dados (de nome SINASC_UF.csv Exemplo: SINASC_RJ.csv) com base nas análises prévias, devendo as variáveis estar na ordem indicada abaixo
# ATENÇÃO aos nomes das variáveis e ordem das colunas
# 1. ANO: 2015  2. UFR (Estado de residência)   3. TN (total de nascimentos)   4. TNRC (total de nascimentos com registros completos, ou seja, sem NA em todas as variáveis do banco de dados)
# 5. TGI_15 (total de gestantes com idade inferior a 15 anos - F_IDADE)   6. TGI_15_19 (total de gestantes com idade >=15 e <=19 anos)
# 7: TGI_20_24 (total de gestantes com idade >=20 e <=24 anos)   8. TGI_25_29 (total de gestantes com idade >=25 e <=29 anos)
# 9: TGI_30_34 (total de gestantes com idade >=30 e <=34 anos)   10. TGI_35_39 (total de gestantes com idade >=35 e <=39 anos)
# 11: TGI_40_44 (total de gestantes com idade >=40 e <=44 anos)  12. TGI_45_49 (total de gestantes com idade >=45 e <=49 anos)
# 13: TGI_50 (total de gestantes com idade >=50)   14: TGIF (total de gestantes em idade fértil, idade >=15 e <=49 anos)
# 15: IM_P25 (percentil 25 da idade materna - IDADEMAE) 16: IM_P50 (percentil 50 da idade materna)   17: IM_P75 (percentil 75 da idade materna)
# 18. IM_MD (idade média materna)   19: IM_DP (desvio-padrão da idade materna)
# 20. EM_S (total de gestantes sem escolaridade, ESCMAE2010=0)   21: EM_FI (total de gestantes com escolaridade Fundamental I)
# 22. EM_FII (total de gestantes com escolaridade Fundamental II)   23. EM_M (total de gestantes com escolaridade Médio)   
# 24. EM_SI (total de gestantes com escolaridade Superior Incompleto)   25. EM_SC (total de gestantes com escolaridade Superior Completo) 
# 26. TGRC_B (total de gestantes da raça/cor branca - RACACORMAE)   27. TGRC_PT (total de gestantes da raça/cor preta)
# 28. TGRC_A (total de gestantes da raça/cor amarela)   29. TGRC_PD (total de gestantes da raça/cor parda)
# 30. TGRC_I (total de gestantes da raça/cor indígena)
# 31. TGSC (total de gestantes sem companheiro - ESTCIV)   32. TGCC (total de gestantes com companheiro)
# 33. TGPRI (total de gestantes primíparas - PARIDADE)     34. TGNPRI (total de gestantes não primíparas)
# 35. TGU (total de gestações única)   36. TGG (total de gestações gemelares)   37. TGD_22 (total de gestações com duração inferior a 22 semanas - GESTACAO)
# 38. TGD_22_27 (total de gestações com duração da gestação >=22 e <=27)   39. TGD_28_31 (total de gestações com duração da gestação >=28 e <=31)
# 40. TGD_32_36 (total de gestações com duração da gestação >=32 e <=36)   41. TGD_37_41 (total de gestações com duração da gestação >=37 e <=41)
# 42. TGD_42 (total de gestações com duração da gestação >=42)   43. TGD_PRT (total de gestações pre-termo, duração < 37 semanas)
# 44. TGD_AT (total de gestações a termo, duração >=37 e <=41)   45. TGD_PST  (total de gestações pós termo, duração >=42) 
# 46. DG_P25 (percentil 25 da duração da gestação - SEMAGESTAC)  47. DG_P50 (percentil 50 da duração da gestação)   
# 48. DG_P75 (percentil 75 da duração da gestação)   49. DG_MD (idade média da duração da gestação)   50. DG_DP (desvio-padrão da duração da gestação)
# 51. TKC_NR (total de consultas de pre-natal não realizado - KOTELCHUCK)   52. TKC_ID (total de consultas de pre-natal inadequado)
# 53. TKC_IT (total de consultas de pre-natal intermediário)   54. TKC_AD (total de consultas de pre-natal adequado)  
# 55. TKC_MAD (total de consultas de pre-natal mais que adequado)   56. TGPRG_S (total de gestantes que peregrinaram)  
# 57. TGPRG_N (total de gestantes que não peregrinaram)    58. TPV (total de partos vaginais)   59. TPC (total de partos cesáreos) 
# 60. TRAP_C (total de recém-nascidos na posição cefálica - TPAPRESENT)   61. TRAP_P (total de recém-nascidos na posição pélvica ou podálica)
# 62. TRAP_T (total de recém-nascidos na posição transversa)  63. TGROB_1 (total de gestantes do grupo de Robson 1 - TPROBSON)
# 64. TGROB_2 (total de gestantes do grupo de Robson 2)   65. TGROB_3 (total de gestantes do grupo de Robson 3)
# 66. TGROB_4 (total de gestantes do grupo de Robson 4)   67. TGROB_5 (total de gestantes do grupo de Robson 5)
# 68. TGROB_6 (total de gestantes do grupo de Robson 6)   69. TGROB_7 (total de gestantes do grupo de Robson 7)
# 70. TGROB_8 (total de gestantes do grupo de Robson 8)   71. TGROB_9 (total de gestantes do grupo de Robson 9)
# 72. TGROB_10 (total de gestantes do grupo de Robson 10)   
# 73. TNLOC_H (total de nascimentos em hospital)   74. TNLOC_ES (total de nascimentos em outros estabelecimentos de saúde)
# 75. TNLOC_D (total de nascimentos em domicílio)  76. TNLOC_O (total de nascimentos em outros locais) 
# 77. TNLOC_AI (total de nascimentos em aldeias indígenas)   
# 78. TRRC_B (total de recém-nascidos da raça/cor branca - RACACOR)   79. TRRC_PT (total de recém-nascidos da raça/cor preta)
# 80. TRRC_A (total de recém-nascidos da raça/cor amarela)   81. TRRC_PD (total de recém-nascidos da raça/cor parda)
# 82. TRRC_I (total de recém-nascidos da raça/cor indígena)  83. TRP_BP (total de recém nascidos com baixo peso - FPESO)
# 84. TRP_N (total de recém nascidos com peso normal)   85. TRP_M (total de recém nascidos com macrossomia)
# 86. PESO_P25 (percentil 25 do peso dos recém-nascidos - PESO)  87. PESO_P50 (percentil 50 do peso dos recém-nascidos)   
# 88. PESO_P75 (percentil 75 do peso dos recém-nascidos)  89. PESO_MD (peso médio dos recém-nascidos)   
# 90. PESO_DP (desvio-padrão dos pesos dos recém-nascidos)    91. TRPIG_P (total de recém-nascidos de GESTAÇÕES ÚNICAS com PIG) 
# 92. TRPIG_A (total de recém-nascidos de GESTAÇÕES ÚNICAS com AIG)   93. TRPIG_G (total de recém-nascidos de GESTAÇÕES ÚNICAS com GIG)
# 94: TRAPG5_B (total de recém-nascidos com Apgar5 baixo, ou seja, < 7)
# 95: TRAPG5_N (total de recém-nascidos com Apgar5 normal, ou seja, >= 7)   96. APG5_MD (Apgar5 médio dos recém-nascidos)   
# 97. APG5_DP (desvio-padrão dos Apgar5 dos recém-nascidos)   98. TRAC (total de recém-nascidos com anomalia congênita - IDANOMAL)
# 99. TRSAC (total de recém-nascidos sem anomalia congênita)


# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv



# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"




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

# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes: variáveis: TIPOBITO, SEXO, RACACOR, TPMORTEOCO, OBITOGRAV, OBITOPUERP,
# CAUSABAS, TPOBITOCOR, MORTEPARTO

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria
# de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada
# variável. Em variáveis quantitativas como IDADE verificar se existem valores como
# 99 para NA

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4. Exemplo: dados_sim_2$TIPOBITO =
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

