# Librerías
# install.packages("pacman")
library(pacman)
p_load(
  gsheet,
  tidyverse,
  scales
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


## Estandarización de variables
### Normalización
df2$`Escribe tu edad exacta` |> scale()

data.frame(
  original = df2$`Escribe tu edad exacta`,
  normalizada = df2$`Escribe tu edad exacta` |> scale()
)

df3 <- df2 |> 
  mutate(edadZ = `Escribe tu edad exacta` |> scale()) |> 
  relocate(edadZ, .after = `Escribe tu edad exacta`)

### Rango (0-1)
df3 <- df3 |> 
  mutate(edadR = `Escribe tu edad exacta` |> rescale()) |> 
  relocate(edadR, .after = `Escribe tu edad exacta`)

## Agrupaciones ----
### Rangos numéricos
cut(
  df3$`Escribe tu edad exacta`,
  breaks = c(-Inf, 18, 21, Inf),
  labels = c("18 o menos", "19 a 21", "Más de 21")
)

df4 <- df3 |>
  mutate(
    edadGR = cut(
      df3$`Escribe tu edad exacta`,
      breaks = c(-Inf, 18, 21, Inf),
      labels = c("18 o menos", "19 a 21", "Más de 21")
    )
  ) |> 
  relocate(edadGR, .after = `Escribe tu edad exacta`)

df4$edadGR |> table() # Conteo
df4$edadGR |> table() |> prop.table() # Porcentajes


### Agrupaciones categóricas
unique(df4$`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [No discrimino y trato a todos por igual]`)
df4 |> colnames()
df4[,8] |> unique()

# ifelse (SI|NO)
ifelse(
  test = df4[,8] == "Totalmente verdadero" | df4[,8] == "Un poco verdadero",
  yes = "SI",
  no = "NO"
)

# Bucles (loops)

## Paso1: crear una variable que contenga los nombres
## de las columas a iterar
frases <-
  df4 |>
  select(starts_with("Según tu")) |>
  colnames()

## Paso2: Ejecutar el bucle
df5 <- df4

for (col in frases) {
  df5[,col] <- ifelse(
    test = df5[,col] == "Un poco verdadero" | df5[,col] == "Totalmente verdadero",
    yes = 1,
    no = 0
  )
}


# Tema 03: Manipulación de datos ----

df5 <- df5 |> as_tibble() # Conversión a tibble

## Selección de columnas
df5 |> select(Sexo)
df5 |> select(Sexo, `Escribe tu edad exacta`)
df5 |> select(`Escribe tu edad exacta`:Sexo)
df5 |> select(-`Marca temporal`)
df5 |> select(-(`Marca temporal`:Sexo))
df5 |> select(starts_with("edad"))
df5 |> select(contains("edad"))
df5 |> select(ends_with("00"))


## Filtrado de filas
df5 |> select(Sexo) |> filter(Sexo == "Mujer")
df5 |> select(Sexo) |> filter(Sexo != "Mujer")

df5 |>
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` > 21)

df5 |>
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` >= 18 & `Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` >= 18,
         `Escribe tu edad exacta` <= 21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(between(`Escribe tu edad exacta`, 18, 21))

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` %in% 18:21)

df5 |> 
  select(Sexo, `Escribe tu edad exacta`) |> 
  filter(`Escribe tu edad exacta` %in% 18:21,
         Sexo == "Hombre")







