# ==============================================================================
# DISCIPLINA: Tópicos Especiais em Inteligência Artificial (Data Science in R)
# ATIVIDADE 03: Análise Preliminar com PCA em Dados Próprios
# PROJETO: Otimização e Previsão de Treinamentos Técnicos em Telecomunicações
# ==============================================================================

# 1. Carregar pacotes necessários
# (Se ainda não instalou, rode no console uma única vez: install.packages(c("factoextra", "readxl")))
library(tidyverse)
library(readxl)
library(factoextra)

# 2. Importar a base de dados Excel (.xlsx)
# Verifique se o nome do arquivo na pasta não tem espaço antes do .xlsx
raw_data <- read_excel("Database 23_10_Last.xlsx")

# 3. Limpeza dos nomes das colunas (garantindo nomes 100% únicos)
 names(raw_data) <- names(raw_data) %>% str_extract("\\(([^()]+)\\)$") %>% str_remove_all("[\\(\\)]") %>% str_trim() %>% replace_na("Coluna_Extra") %>% make.unique(sep = "_")

# 4. Tratamento, deduplicação por turma e conversão de variáveis
dados_limpos <- raw_data %>%
  filter(`Class Status` != "Class Status", `Disable Flag` == "no") %>%
  distinct(`Clss Code`, .keep_all = TRUE) %>%
  mutate(
    # Variáveis numéricas contínuas
    duracao_dias  = as.numeric(`Class Duration`),
    tamanho_turma = as.numeric(`Class Size`),
    satisfacao    = as.numeric(`Class Satisfaction`),
    
    # Conversão de Categóricas Binárias -> Numéricas (0 e 1)
    modo_lvc_num    = ifelse(`Delivery Mode` == "LVC", 1, 0),
    gestao_rtc_num  = ifelse(`Class Management` == "RTC", 1, 0),
    bg_operator_num = ifelse(BG == "Operator", 1, 0),
    
    # Conversão de Categóricas Ordinais -> Numéricas (Escala)
    nivel_curso_ord = case_when(
      `SPart Level` == "L1" ~ 1,
      `SPart Level` == "L2" ~ 2,
      `SPart Level` == "L3" ~ 3,
      `SPart Level` == "L4" ~ 4,
      TRUE ~ NA_real_
    ),
    risco_operacional_ord = case_when(
      `High Risk Class` == "Low-risk"    ~ 1,
      `High Risk Class` == "Medium-risk" ~ 2,
      `High Risk Class` == "High-risk"   ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(
    duracao_dias, tamanho_turma, satisfacao,
    modo_lvc_num, gestao_rtc_num, bg_operator_num,
    nivel_curso_ord, risco_operacional_ord
  )

# Verificar quantas turmas válidas entraram na análise
cat("Total de turmas únicas analisadas no PCA:", nrow(dados_limpos), "\n")

# 5. Selecionar APENAS a matriz numérica para o PCA
matriz_pca <- dados_limpos %>%
  select(
    duracao_dias,
    tamanho_turma,
    satisfacao,
    modo_lvc_num,
    gestao_rtc_num,
    bg_operator_num,
    nivel_curso_ord,
    risco_operacional_ord
  )

# 6. Executar o PCA com padronização (center = TRUE, scale. = TRUE)
pca_telecom <- prcomp(matriz_pca, center = TRUE, scale. = TRUE)

# 7. Resultados Numéricos (Variância Explicada e Pesos/Loadings)
summary(pca_telecom)
print(round(pca_telecom$rotation, 3))

# 8. Visualizações Gráficas
# Gráfico 1: Scree Plot (Porcentagem de variância explicada por componente)
fviz_eig(pca_telecom, addlabels = TRUE, ylim = c(0, 45),
         main = "Scree Plot - Variância Explicada (Treinamentos Telecom)")

# Gráfico 2: Círculo de Correlações (Contribuição e direção das variáveis)
fviz_pca_var(pca_telecom,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Círculo de Correlações - Variáveis Operacionais")

# Gráfico 3: Biplot agrupado por Modo de Entrega (LVC Online vs ILT Presencial)
fviz_pca_biplot(pca_telecom,
                geom.ind = "point",
                habillage = dados_limpos$`Delivery Mode`,
                addEllipses = TRUE,
                repel = TRUE,
                title = "PCA Biplot - Turmas por Modo de Entrega (LVC vs ILT)")

# Gráfico 4: Indivíduos agrupados por Segmento (Operator vs Enterprise vs Others)
fviz_pca_ind(pca_telecom,
             geom.ind = "point",
             habillage = dados_limpos$BG,
             addEllipses = TRUE,
             title = "PCA - Agrupamento por Business Group (BG)")

