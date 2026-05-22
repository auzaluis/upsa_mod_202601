# Librerías
library(gsheet)
library(tidyverse)
library(viridis)
library(arules)
library(arulesViz)

# Extraer los datos
url <- "https://docs.google.com/spreadsheets/d/1NTjA8nrmcWltvZn4oq5KJK7-R4Mb-_is_5vVsoBDCv0/edit?usp=sharing"
df <- read.csv(
  text = gsheet2text(url, format = "csv"),
  stringsAsFactors = F
)

# basket_id
## id_canasta
df2 <- 
  df |>
  mutate(basket_id = paste(Member_number, Date, sep = "_")) |> 
  select(basket_id, itemDescription)

## convertir basket_id a lista
list <- split(
  x = df2$itemDescription,
  f = df2$basket_id
)

## convertir list a formato transactions
basket <- as(
  object = list,
  Class = "transactions"
)

class(basket)

# Productos más vendidos
itemFrequencyPlot(
  x = basket,
  topN = 20,
  horiz = T
)

# Reglas

rules <-
  apriori(
    data = basket,
    parameter = list(
      supp = 0.0005,
      conf = 0.10,
      minlen = 2,
      maxlen = 2
    )
  )

inspect(rules)

plot(
  rules,
  method = "graph",
  engine = "htmlwidget"
)

# Fijar la mano derecha
# La salchicha se va a vencer! :(
rules_rhs <- apriori(
  data = basket,
  parameter = list(
    supp = 0.001,
    conf = 0.07,
    minlen = 2,
    maxlen = 3
  ),
  appearance = list(
    rhs = "sausage",
    default = "lhs"
  )
)

plot(
  rules_rhs,
  method = "graph",
  engine = "htmlwidget"
)

# Fijar la mano izquierda
rules_lhs <- apriori(
  data = basket,
  parameter = list(
    supp = 0.001,
    conf = 0.05,
    minlen = 2,
    maxlen = 3
  ),
  appearance = list(
    lhs = "whole milk",
    default = "rhs"
  )
)

plot(
  rules_lhs,
  method = "graph",
  engine = "htmlwidget"
)










