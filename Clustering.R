
#Importando a base#
library(readxl)
base <- read_excel("C:/Users/mateus/Documents/Pessoal/USP Esalq/TCC/IBGE_estados2.xlsx")
View(base)
summary(base)

library(ggpubr)

#Limpando a base#
base$Codigo <- NULL
base$Gentílico<-NULL
base$Governador <- NULL
base$Capital <- NULL
base$Area_territorial <- NULL
base$Receitas_realizadas_2017 <- NULL
base$IDH_2010 <- NULL
base$Densidade_demografica <- NULL
base$Matriculas_ens_fun_2021 <- NULL
base$Despesas_empenhadas_2017 <- NULL
base$Total_veiculos_2022 <- NULL
base$Populacao_estimada <- NULL
base$Densidade_demografica <- NULL

summary(base)

  #---- Depois de limpar a base vamos para os códigos de Aprendizado não supervisionado Cluster----#

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias
options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# É necessário padronizar as variáveis pois elas têm medidas distintas
# É feito um Zscore com o comando Scale

IBGE_padronizado <- as.data.frame(scale(base[,2:10]))
view(IBGE_padronizado)

#matriz de dissimilaridade
matriz_D <- IBGE_padronizado %>% 
  dist(method = "euclidean")
#single linkage

cluster_hier_single <- agnes(x = matriz_D, method = "single")
# Construção do dendrograma "single linkage"

dev.off()
fviz_dend(x = cluster_hier_single, show_labels = F)

# Construção do dendrograma "complete linkage"
fviz_dend(x = cluster_hier_complete, show_labels = F)

# 2º Teste: Elaboração da clusterização hierárquica como "complete linkage"
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")


# 3º Teste: Elaboração da clusterização hierárquica como "average linkage"
cluster_hier_average <- agnes(x = matriz_D, method = "average")

# Construção do dendrograma "average linkage"
fviz_dend(x = cluster_hier_average, show_labels = F)
#Vamos aplicar a correlação de pearson para entender como as variáveis se comportam entre si

## Vamos optar pelo Average linkage 
# Dendrograma com visualização dos clusters (selecionando por "altura")
fviz_dend(x = cluster_hier_average,
          h = 5.0,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())


# Vamos detalhar esse esquema hierárquico
coeficientes <- sort(cluster_hier_average$height, decreasing = FALSE) 
esquema <- as.data.frame(cbind(cluster_hier_average$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

## Portanto, vamos gerar uma variável indicando 4 clusters

base$cluster_H <- factor(cutree(tree = cluster_hier_average, k = 4))
IBGE_padronizado$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 4))

# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos

summary(anova_votos_lula <- aov(formula = Votos_lula ~ cluster_H,
                                data = IBGE_padronizado))

summary(anova_votos_bolsonaro <- aov(formula = Votos_bolsonaro ~ cluster_H,
                             data = IBGE_padronizado))

summary(anova_IDH <- aov(formula = IDH_2020 ~ cluster_H,
                            data = IBGE_padronizado))

summary(anova_rendimento <- aov(formula = Rendimento_mensal_domiciliar_per_capita ~ cluster_H,
                         data = IBGE_padronizado))

summary(anova_PIB <- aov(formula = PIB_2021 ~ cluster_H,
                         data = IBGE_padronizado))


summary(anova_IDEB <- aov(formula = IDEB_2021 ~ cluster_H,
                         data = IBGE_padronizado))


summary(anova_homicidios <- aov(formula = Taxa_homicidios ~ cluster_H,
                         data = IBGE_padronizado))

summary(anova_gini <- aov(formula = Indice_gini ~ cluster_H,
                         data = IBGE_padronizado))

summary(anova_desocupacao <- aov(formula = Indice_desocupacao ~ cluster_H,
                         data = IBGE_padronizado))


analise <- group_by(base, cluster_H)%>%
  summarise(IDH_2020 = mean(IDH_2020, na.rm = TRUE),
            Votos_lula = mean(Votos_lula, na.rm = TRUE),
            Votos_bolsonaro = mean(Votos_bolsonaro, na.rm = TRUE),
            PIB = mean(PIB_2021, na.rm = TRUE),
            IDEB = mean(IDEB_2021, na.rm = TRUE),
            Homicidios = mean(Taxa_homicidios, na.rm = TRUE),
            Gini = mean(Indice_gini, na.rm = TRUE),
            Desocupacao = mean(Indice_desocupacao, na.rm = TRUE),
            Rendimento = mean(Rendimento_mensal_domiciliar_per_capita, na.rm = TRUE))


chart.Correlation((base[2:10]), histogram = TRUE)

