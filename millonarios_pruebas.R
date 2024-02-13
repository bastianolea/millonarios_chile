source("funciones.R")

# individuo -----
# años que debiese trabajar una persona para obtener la fortuna
fortuna

sueldo = 1300000

fortuna/sueldo #sueldos

fortuna/(sueldo*12) #año

sueldo_dia = sueldo/20

fortuna/sueldo_dia #días laborales para obtener su fortuna
as.integer(fortuna/(sueldo_dia*365)) #años trabajando para obtener su fortuna



# equipo ----
# grupo de personas que debería trabajar x tiempo para obtener la fortuna
fortuna

sueldo = 1300000

equipo = 1

sueldo = sueldo*equipo

fortuna/sueldo #sueldos

fortuna/(sueldo*12) #año

sueldo_dia = sueldo/20

fortuna/sueldo_dia #días laborales para obtener su fortuna
as.integer(fortuna/(sueldo_dia*365)) #años trabajando para obtener su fortuna


# sueldos repartibles ----
fortuna/sueldo #personas que podrían ganar x sueldo con la fortuna


# vidas trabajadas ----
# https://cooperativa.cl/noticias/pais/poblacion/chile-alcanzo-un-nuevo-record-en-la-esperanza-de-vida-supero-los-81/2024-02-05/065352.html
# https://www.df.cl/economia-y-politica/actualidad/efecto-millennials-edad-promedio-de-los-ocupados-sube-a-44-anos
genero = "mujer"

esperanza_vida = ifelse(genero ==  "mujer", 83.5, 78.5)
edad_jubilacion = ifelse(genero ==  "mujer", 60, 65)
años_jubilacion = esperanza_vida - edad_jubilacion
edad_laboral_promedio = 24

años_laborales = esperanza_vida - edad_laboral_promedio - años_jubilacion

sueldo_anual = sueldo * 12

#sueldo exponencial: considerando que los primeros 10 años de la vida uno ganó menos
sueldo_primeros_años = sueldo_anual * seq(0.4, 1, by = 0.065)
sueldo_vida <- sum(sueldo_primeros_años) + (sueldo_anual * (años_laborales-10))
as.integer(fortuna/sueldo_vida)

#sueldo lineal: mismo sueldo toda la vida
sueldo_vida <- sueldo_anual * años_laborales
as.integer(fortuna/sueldo_vida)


exp(1:10)
seq(500, 1500, by = 4)




# graficar ----
forbes <- readr::read_csv2("forbes_2023_chile.csv")
millonarios <- readRDS("millonarios_chile.rds")

library(ggplot2)

millonarios |> 
  mutate(nombre = factor(nombre, forbes$nombre)) |> 
  ggplot(aes(nombre, fortuna_b_usd)) +
  geom_point() +
  geom_segment(aes(xend = nombre, yend = 0)) +
  geom_text(aes(label = miles(fortuna_b_usd)), 
            angle = 30, hjust = 0, size = 3, nudge_x = 0.1, vjust = 0) +
  scale_y_continuous(expand = expansion(c(0, 0.15)), labels = ~paste("USD", miles(.x))) +
  theme_minimal() +
  labs(y = "Fortuna (en millones de dólares)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x = element_blank()
        ) 
  

#equivalente en ingresos de la población ----

casen <- arrow::read_parquet("datos/casen2022/casen2022.parquet")

casen_deciles <- casen |> 
  select(expr, ytrabajocor, dautr) |> 
  filter(!is.na(ytrabajocor),
         !is.na(dautr)) |> 
  mutate(ytrabajocor_exp = ytrabajocor * expr) |> 
  group_by(dautr) |> 
  summarize(suma_ingresos = sum(ytrabajocor_exp),
            promedio_ingresos = weighted.mean(ytrabajocor, expr) |> as.integer())

casen_deciles

# ingreso_mediano = 502604
# poblacion = 20086377
# ingresos_mitad_chile = ingreso_mediano * (poblacion/2)

ingresos_mitad_baja = casen_deciles |> filter(dautr %in% c("I", "II", "III", "IV", "V")) |> summarize(sum(suma_ingresos)) |> pull()
ingresos_mitad_alta = casen_deciles |> filter(dautr %in% c("V", "VI", "VII", "VIII", "IX", "X")) |> summarize(sum(suma_ingresos)) |> pull()

sum(millonarios$fortuna_pesos)
