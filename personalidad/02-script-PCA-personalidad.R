# Librerías
library(pacman)
p_load(
  tidyverse,
  plotly,
  FactoMineR,
  ggcorrplot
)

frases2 <- df6[,8:31] |> colnames()

# Matriz de correlaciones
r <- cor(
  df6 |> select(all_of(frases2))
)

# Gráfica
ggplotly(
  ggcorrplot(
    corr = r,
    show.legend = F,
    tl.cex = 6,
    colors = c("red", "white", "blue")
  ) +
    theme(
      axis.text.x = element_blank(),
      panel.grid = element_blank()
    )
)

# PCA: Principal Components Analysis

## Consumismo
consumismo <- frases2[c(5,10,16)]

pca_consumismo <- FactoMineR::PCA(
  df6 |> select(all_of(consumismo)),
  ncp = 1
)

pca_consumismo$eig
pca_consumismo$var$cor
pca_consumismo$ind$coord

# Comparando componente con vars originales
tibble(
  df6 |> select(all_of(consumismo)),
  consumismo = pca_consumismo$ind$coord *-1
) |> View()


## Disciplina
disciplina <- frases2[c(4,15,19)]

pca_disciplina <- FactoMineR::PCA(
  df6 |> select(all_of(disciplina)),
  ncp = 1
)

pca_disciplina$eig
pca_disciplina$var$cor

## Humildad
humildad <- frases2[c(1,6,18)]

pca_humildad <- FactoMineR::PCA(
  df6 |> select(all_of(humildad)),
  ncp = 1
)

pca_humildad$eig
pca_humildad$var$cor

## Respeto
respeto <- frases2[c(14,12,9)]

pca_respeto <- FactoMineR::PCA(
  df6 |> select(all_of(respeto)),
  ncp = 1
)

pca_respeto$eig
pca_respeto$var$cor

## Extroversión
extroversion <- frases2[c(20,23)]

pca_extroversion <- FactoMineR::PCA(
  df6 |> select(all_of(extroversion)),
  ncp = 1
)

pca_extroversion$eig
pca_extroversion$var$cor

## introversion
introversion <- frases2[c(11,21,22)]

pca_introversion <- FactoMineR::PCA(
  df6 |> select(all_of(introversion)),
  ncp = 1
)

pca_introversion$eig
pca_introversion$var$cor

# Data frame con las dimensiones
df10 <- 
  df6 |> 
  mutate(
    consumismo   = pca_consumismo$ind$coord *-1,
    disciplina   = pca_disciplina$ind$coord *-1,
    humildad     = pca_humildad$ind$coord,
    respeto      = pca_respeto$ind$coord *-1,
    extroversion = pca_extroversion$ind$coord *-1,
    introversion = pca_introversion$ind$coord
  )



















