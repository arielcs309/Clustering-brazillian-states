# =========================================================
# Clustering dos Estados Brasileiros
# MBA Data Science & Analytics – USP ESALQ
# Autor: Ariel Sousa
# =========================================================

# -----------------------------
# 1. Configurações iniciais
# -----------------------------
set.seed(42)

library(readxl)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggpubr)
library(corrplot)

# -----------------------------
# 2. Leitura dos dados
# -----------------------------
base <- read_excel("data/IBGE_estados2.xlsx")

# Visualização inicial
glimpse(base)
summary(base)

# -----------------------------
# 3. Limpeza e seleção de variáveis
# -----------------------------
base <- base %>%
  select(
    UF,
    IDH_2020,
    Votos_lula,
    Votos_bolsonaro,
    PIB_2021,
    IDEB_2021,
    Taxa_homicidios,
    Indice_gini,
    Indice_desocupacao,
    Rendimento_mensal_domiciliar_per_capita
  )

summary(base)

# -----------------------------
# 4. Padronização (Z-score)
# -----------------------------
dados_padronizados <- base %>%
  select(-UF) %>%
  scale() %>%
  as.data.frame()

# -----------------------------
# 5. Matriz de dissimilaridade
# -----------------------------
matriz_dist <- dist(dados_padronizados, method = "euclidean")

# -----------------------------
# 6. Clusterização hierárquica
# Método escolhido: Average Linkage
# -----------------------------
cluster_hier <- agnes(matriz_dist, method = "average")

# -----------------------------
# 7. Dendrograma
# -----------------------------
fviz_dend(
  cluster_hier,
  k = 4,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "black",
  show_labels = FALSE,
  ggtheme = theme_bw()
)

# -----------------------------
# 8. Definição dos clusters
# -----------------------------
base$cluster_H <- factor(cutree(cluster_hier, k = 4))
dados_padronizados$cluster_H <- base$cluster_H

# -----------------------------
# 9. Validação estatística (ANOVA)
# -----------------------------
variaveis_anova <- c(
  "Votos_lula",
  "Votos_bolsonaro",
  "IDH_2020",
  "Rendimento_mensal_domiciliar_per_capita",
  "PIB_2021",
  "IDEB_2021",
  "Taxa_homicidios",
  "Indice_gini",
  "Indice_desocupacao"
)

anova_resultados <- lapply(variaveis_anova, function(var) {
  modelo <- aov(as.formula(paste(var, "~ cluster_H")), data = base)
  summary(modelo)
})

names(anova_resultados) <- variaveis_anova
anova_resultados

# -----------------------------
# 10. Análise descritiva dos clusters
# -----------------------------
analise_clusters <- base %>%
  group_by(cluster_H) %>%
  summarise(
    IDH = mean(IDH_2020, na.rm = TRUE),
    Votos_Lula = mean(Votos_lula, na.rm = TRUE),
    Votos_Bolsonaro = mean(Votos_bolsonaro, na.rm = TRUE),
    PIB = mean(PIB_2021, na.rm = TRUE),
    IDEB = mean(IDEB_2021, na.rm = TRUE),
    Homicidios = mean(Taxa_homicidios, na.rm = TRUE),
    Gini = mean(Indice_gini, na.rm = TRUE),
    Desocupacao = mean(Indice_desocupacao, na.rm = TRUE),
    Rendimento = mean(Rendimento_mensal_domiciliar_per_capita, na.rm = TRUE)
  )

analise_clusters

# -----------------------------
# 11. Correlação de Pearson
# -----------------------------
matriz_cor <- cor(base %>% select(-UF, -cluster_H))

corrplot(
  matriz_cor,
  method = "color",
  type = "upper",
  tl.cex = 0.8,
  addCoef.col = "black",
  number.cex = 0.7
)

# -----------------------------
# 12. Exportação dos resultados
# -----------------------------
write.csv(analise_clusters, "outputs/cluster_summary.csv", row.names = FALSE)
write.csv(base, "outputs/base_com_clusters.csv", row.names = FALSE)
