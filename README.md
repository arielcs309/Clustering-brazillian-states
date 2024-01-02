# Clustering brazillian states 
[![NPM](https://img.shields.io/npm/l/react)]((https://github.com/arielcs309/Clustering-brazillian-states/blob/main/LICENSE))

# About the project

The project aims to analyse the data from brazilian states. I gathered the Social economic e economic indicators from each state of Brazil on the database . 
It was also added the votes of each state for the president on the 2022 election.
The data was taken from IBGE, TSE and Ministério da Educação, which are public organs the make researches and gather data from Brazil.

![Indicators](https://github.com/arielcs309/assets/blob/main/Indicators.png)


## States divided after clustering
![cluster](https://github.com/arielcs309/assets/blob/main/Brazil%20map.png)

## clusters table
![table 1](https://github.com/arielcs309/assets/blob/main/Table%20Clusters.png)

## Dendrogram map
![Dendrogram](https://github.com/arielcs309/assets/blob/main/dendrogram.png)

# Language
![R](https://github.com/arielcs309/assets/blob/main/R%20language.jpg)

# How to execute the project
## Summarizing the project
```bash
# install packages
packages <- c("plotly", #graph platform
             "tidyverse", #load other R packages
             "ggrepel", 
             "knitr", "kableExtra", #format tables
             "reshape2", #'melt' function
             "misc3d", #3D graphs
             "plot3D", #3D graphs
             "cluster", #'agnes' function to elaborate hierarchy cluster
             "factoextra", #função 'fviz_dend' to build dendrograms
             "ade4")

# Use Scale function to make the indicators standart. Average becomes 0 and standart deviation becomes 1.
IBGE_padronizado <- as.data.frame(scale(base[,2:10]))
view(IBGE_padronizado)

# create dissimilarity matrix with the euclidian distance
matriz_D <- IBGE_padronizado %>% 
  dist(method = "euclidean")

# Choosing the linkage with agnes function that allows you to use single, average and complete linkage
cluster_hier_average <- agnes(x = matriz_D, method = "average")

#building the dendrogram
dev.off()
fviz_dend(x = cluster_hier_average, show_labels = F)

#checking if variables help to create groups with ANOVA.
summary(anova_IDH <- aov(formula = IDH_2020 ~ cluster_H,
                            data = IBGE_padronizado))

#analyse the final clusters created with group by according
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

# Also added a Pearson correlation to check how the variables behave with each other
chart.Correlation((base[2:10]), histogram = TRUE)

```

# Author

Ariel Sousa

https://www.linkedin.com/in/ariel-candido-22684578/
