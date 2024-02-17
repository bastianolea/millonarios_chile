library(dplyr)
library(stringr)
library(glue)
library(readr)
library(spatstat) #weighted.median y weighted.quartile
library(ggforce) #geom_circle

options(scipen = 9999)

source("funciones.R")

casen <- arrow::read_parquet("datos/casen2022/casen2022.parquet")

#creando los deciles ----
casen_ingresos <- casen |> 
  select(expr, ytrabajocor) |> 
  filter(!is.na(ytrabajocor))

# casen_ingresos_exp <- casen_ingresos %>% purrr::map_df(., rep, .$expr)
# 
# casen_ingresos_exp |> 
#   filter(ytrabajocor > 500000)

# casen_deciles_2b <- quantile(casen_ingresos$ytrabajocor, seq(.1, .9, by = .1)) #sin expandir
# casen_deciles_2a <- quantile(casen_ingresos_exp$ytrabajocor, seq(.1, .9, by = .1)) #con datos expandidos

#calculo con pesos
casen_porcentajes_ingreso <- weighted.quantile(casen_ingresos$ytrabajocor, w = casen_ingresos$expr, probs = seq(.1, .9, by = .1)) |> 
  as.integer() |> 
  signif(5)

#guardar 
readr::write_rds(casen_porcentajes_ingreso, "datos/casen_cortes_deciles.rds")

## calcular ingresos sumados de todos los chilenos ----

casen_deciles <- casen_ingresos |> 
  rowwise() |> 
  mutate(decil = encontrar_decil(casen_porcentajes_ingreso, ytrabajocor)) |> 
  mutate(ytrabajocor_exp = ytrabajocor * expr) |>
  group_by(decil) |> 
  summarize(poblacion = sum(expr), #personas en casa decil
            suma_ingresos = sum(ytrabajocor_exp), #ingresos sumados de todas las personas del decil
            #estadisticos de cada decil
            promedio_ingresos = weighted.mean(ytrabajocor, expr) |> as.integer(),
            mediana_ingresos = weighted.median(ytrabajocor, expr),
            min_ingresos = min(ytrabajocor),
            max_ingresos = max(ytrabajocor))

#guardar
readr::write_csv2(casen_deciles, "datos/casen_deciles.csv")
