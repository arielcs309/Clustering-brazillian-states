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
It was used the group by to gather all the states from each cluster and then I chose the mean in each variable.
The first cluster for example had the highest vote for the candidate 1, the lowest GDP, the highest homicide rate and the per capita income was the lowest compared to the other clusters.
The second cluster had the best per capita income 
The third cluster had the highest percentage of votes for the candidate 2, the gini index was the lowest (that means the social inequality is not so big) and the unemployment rate is the lowest.
the forth cluster is composed by one state, which is an outlier. The GDP contrants from the other states. It is São Paulo the city where the economy is the best. 
![table 1](https://github.com/arielcs309/assets/blob/main/Table%20Clusters.png)

## Dendrogram map
Dendrograms have a tree-like structure, where each "node" represents a group or cluster of entities, and the branches represent the relationships between these clusters. It is a graph to represent the hierarchy of how the clusters are organized.
![Dendrogram](https://github.com/arielcs309/assets/blob/main/dendrogram.png)

## Analysis of Variance (ANOVA)
All variables had a p-value less than 0.05, leading to the rejection of the null hypothesis. This indicates that they demonstrated significance in differentiating the clusters.
Null Hypothesis (H0): The means of the groups are equal.
Alternative Hypothesis (H1): At least one group mean is different from the others.
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
https://www.kaggle.com/arielsousa
