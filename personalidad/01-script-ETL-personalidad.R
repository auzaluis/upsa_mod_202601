# Librerías
# install.packages("pacman")
library(pacman)
p_load(
  gsheet,
  tidyverse
)

# Tema 01: Carga de datos ----
## Carga local
df <- read.csv(
  file = "personalidad/Personalidad y uso de apps (respuestas).csv",
  check.names = F
)

colnames(df) # Mostrar nombres de columnas

## Carga en línea (API)
url <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

df <- read.csv(
  text = gsheet2text(url),
  check.names = F
)

colnames(df)    # Ver los nombre de las columnas
class(df)       # Ver el tipo de variable/objeto
class(df$Sexo)  # Ver el tipo de dato
nrow(df)        # Cantidad de filas
ncol(df)        # Cantidad de columnas
glimpse(df)     # Resumen general del df



# Tema 02: Transformación de datos ----
## Valores perdidos (NA) ----

df$`Escribe tu edad exacta`
is.na(df$`Escribe tu edad exacta`)
summary(is.na(df$`Escribe tu edad exacta`))

df$`Escribe tu edad exacta` |> 
  is.na() |> 
  summary()


## Imputación de datos (reemplazo por la media)
round(mean(df$`Escribe tu edad exacta`, na.rm = TRUE))

# Cálculo de la edad promedio
edad_promedio <- df$`Escribe tu edad exacta` |>
  mean(na.rm = TRUE) |> 
  round()

edad_promedio

ifelse(
  test = is.na(df$`Escribe tu edad exacta`),
  yes = edad_promedio,
  no = df$`Escribe tu edad exacta`
)

df2 <- df |>
  mutate(
    edad2 = ifelse(
      test = is.na(df$`Escribe tu edad exacta`),
      yes = edad_promedio,
      no = df$`Escribe tu edad exacta`
    )
  ) |> 
  relocate(edad2, .after = `Escribe tu edad exacta`)

## Eliminar toda la fila
df2 <- na.omit(df)





