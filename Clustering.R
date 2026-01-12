# =========================================================
# Clustering dos Estados Brasileiros
# MBA Data Science & Analytics – USP ESALQ
# Autor: Ariel Sousa
# =========================================================

# -----------------------------
# 1. Configurações iniciais
# -----------------------------
pacotes <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "geobr",
  "sf",
  "cluster",
  "factoextra",
  "ggpubr",
  "corrplot"
)

pacotes_nao_instalados <- pacotes[!pacotes %in% installed.packages()[, "Package"]]

if (length(pacotes_nao_instalados) > 0) {
  install.packages(pacotes_nao_instalados)
}

lapply(pacotes, library, character.only = TRUE)

set.seed(42)

# -----------------------------
# 2. Leitura dos dados
# -----------------------------
# UF já está em sigla (AC, AM, SP, ...)
base <- read_excel("C:/Users/mcs30/R/IBGE_estados2.xlsx")

glimpse(base)
summary(base)

# -----------------------------
# 3. Seleção das variáveis
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
# Método: Average Linkage
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
write.csv(
  analise_clusters,
  "C:/Users/mcs30/R/outputs/cluster_summary.csv",
  row.names = FALSE
)

write.csv(
  base,
  "C:/Users/mcs30/R/outputs/base_com_clusters.csv",
  row.names = FALSE
)

# -----------------------------
# 13. Mapa do Brasil por clusters
# -----------------------------

# Leitura do mapa dos estados brasileiros
mapa_estados <- read_state(year = 2020)

# Join do mapa com a base de dados (UF em sigla)
mapa_cluster <- mapa_estados %>%
  left_join(base, by = c("abbrev_state" = "UF"))

# Plot do mapa com paleta personalizada
mapa_clusters_plot <- ggplot(mapa_cluster) +
  geom_sf(aes(fill = cluster_H), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "#5f6f3a",  # mais escuro
      "#f2d680",   #base
      "#eb9c4d",  # mais claro
      "#f3ffcf"   # bem claro (outlier)
    ),
    name = "Clusters",
    labels = c(
      "Low income, high homicide, Candidate 1 majority",
      "Higher income and GDP",
      "Lower inequality and unemployment, Candidate 2 majority",
      "Economic outlier (São Paulo)"
    )
  ) +
  labs(
    title = "Brazilian States Grouped by Socioeconomic and Political Similarity",
    subtitle = "Hierarchical clustering based on socioeconomic indicators and 2022 presidential votes",
    caption = "Sources: IBGE, TSE, Ministry of Education"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold")
  )

# Exibir o mapa
print(mapa_clusters_plot)


# -----------------------------
# 14. Exportação do mapa
# -----------------------------

ggsave(
  filename = "C:/Users/mcs30/R/outputs/mapa_clusters_brasil.png",
  plot = mapa_clusters_plot,
  width = 10,
  height = 8,
  dpi = 300
)
