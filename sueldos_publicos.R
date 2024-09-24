# descargar tabla ----

download.file(url = "https://static.theclinic.cl/media/2024/09/17-220517_9n70_100-Funcionarios-publicos-con-suelos-superiores-al-del-Presidente.pdf",
              destfile = "datos/theclinic_sueldos_mayores_presidente.pdf")

# install.packages("tabulapdf", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
# install.packages("rJava")
# brew install java

library(tabulapdf)
library(purrr)
library(janitor)
library(stringr)
library(tidyr)

source("app/funciones.R")

dolar <- obtener_dolar(scrapear = T)

# extraer ----
# extraer datos desde tabla en PDF 
tabla_0 <- extract_tables("datos/theclinic_sueldos_mayores_presidente.pdf", col_names = F)

tabla_1 <- tabla_0 |> 
  list_rbind() |> 
  row_to_names(1) |> 
  clean_names()


# extraer monto ----
tabla_2 <- tabla_1 |> 
  mutate(moneda = ifelse(str_detect(monto_bruto, "US"), "USD", "CLP")) |> 
  rename(monto_original = monto_bruto) |> 
  mutate(monto = str_remove_all(monto_original, "\\."),
         monto = str_extract_all(monto, "\\d+")) |> 
  unnest(monto) |>
  mutate(monto = as.numeric(monto),
         monto = ifelse(moneda == "USD", monto * dolar, monto))


# ordenar ----
tabla_3 <- tabla_2 |> 
  group_by(nombre) |> 
  slice_max(monto) |> 
  ungroup() |> 
  relocate(moneda, monto, .after = monto_original) |> 
  arrange(desc(monto))
         

# redondear ---- 
tabla_4 <- tabla_3 |> 
  mutate(monto = signif(monto, digits = 4))


# agregar presidente ----
tabla_5 <- tabla_4 |> 
  add_row(nombre = "Gabriel Boric", cargo = "Presidente de la República",
          monto_original = "$7.634.386", moneda = "CLP", monto = 7634386,
          .before = 1)

tabla_5
