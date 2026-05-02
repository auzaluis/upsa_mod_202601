library(FactoMineR)
library(NbClust)
library(tidyverse)

dimensiones <- c(
  "consumismo",  
  "disciplina",
  "humildad",
  "respeto",
  "extroversion",
  "introversion"
  )

# Clustering
clustering <- NbClust(
  data = df10 |> select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn",
  min.nc = 6,
  max.nc = 6
)

clustering$Best.partition |> table()
clustering$Best.partition |> table() |> prop.table()

df11 <- df10 |> 
  mutate(segmento = clustering$Best.partition)

# Análisis de los segmentos
df11 |> 
  group_by(segmento) |> 
  summarise(
    consumismo   = mean(consumismo),
    disciplina   = mean(disciplina),
    humildad     = mean(humildad),
    respeto      = mean(respeto),
    extroversion = mean(extroversion),
    introversion = mean(introversion)
  )

# Mapa perceptual
df11 |> 
  group_by(segmento) |> 
  summarise(
    consumismo   = mean(rescale(consumismo)),
    disciplina   = mean(rescale(disciplina)),
    humildad     = mean(rescale(humildad)),
    respeto      = mean(rescale(respeto)),
    extroversion = mean(rescale(extroversion)),
    introversion = mean(rescale(introversion))
  ) |> 
  column_to_rownames("segmento") |> 
  CA()









