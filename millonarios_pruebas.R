library(dplyr)
library(stringr)
library(glue)
library(readr)
library(spatstat) #weighted.median y weighted.quartile
library(ggforce) #geom_circle

options(scipen = 9999)

source("funciones.R")

millonarios <- read_rds("millonarios_chile.rds")
casen <- arrow::read_parquet("datos/casen2022/casen2022.parquet")
casen_porcentajes_deciles <- readr::read_rds("datos/casen_cortes_deciles.rds")
casen_deciles <- readr::read_csv2("datos/casen_deciles.csv")

dolar <- obtener_dolar(scrapear = F)

fortuna <- millonarios |> slice(3) |> pull(fortuna_pesos)

farkas <- millonarios |> filter(nombre == "Leonardo Farkas")  |> pull(fortuna_pesos)


# individuo -----
# años que debiese trabajar una persona para obtener la fortuna
fortuna

sueldo = 3500000

fortuna/sueldo #sueldos

fortuna/(sueldo*12) #año

sueldo_dia = sueldo/20

fortuna/sueldo_dia #días laborales para obtener su fortuna
as.integer(fortuna/(sueldo_dia*365)) #años trabajando para obtener su fortuna



# equipo ----
# grupo de personas que debería trabajar x tiempo para obtener la fortuna
fortuna

sueldo = 1800000

equipo = 1

sueldo = sueldo*equipo

fortuna/sueldo #sueldos

fortuna/(sueldo*12) #año

sueldo_dia = sueldo/20

fortuna/sueldo_dia #días laborales para obtener su fortuna
as.integer(fortuna/(sueldo_dia*365)) #años trabajando para obtener su fortuna


#  sueldos repartibles ----
fortuna/sueldo #personas que podrían ganar x sueldo con la fortuna

poblacion_chile_2024 = 20086377
fortuna_en_sueldos_minimos = fortuna/500000

scales::percent(fortuna_en_sueldos_minimos/poblacion_chile_2024, accuracy = 1.11)
# Se pordría pagar un sueldo mínimo al x% de la población chilena

# sueldo en toda la vida ----
# https://cooperativa.cl/noticias/pais/poblacion/chile-alcanzo-un-nuevo-record-en-la-esperanza-de-vida-supero-los-81/2024-02-05/065352.html
# https://www.df.cl/economia-y-politica/actualidad/efecto-millennials-edad-promedio-de-los-ocupados-sube-a-44-anos

sueldo = 3500000
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



#sueldo en la vida vs fortuna ----

proporcion_fortuna = fortuna/fortuna
proporcion_farkas = farkas/fortuna
proporcion_sueldo_fortuna = sueldo_vida/fortuna

##cuadrado con proporción ----
ggplot() +
  geom_rect(aes(xmin = 1, xmax = 1+proporcion_fortuna/100, ymin = 1, ymax = 1+proporcion_fortuna/100),
            fill = "grey") +
  geom_rect(aes(xmin = 1, xmax = 1-proporcion_sueldo_fortuna,
                ymin = 1, ymax = 1-proporcion_sueldo_fortuna),
            fill = "red") +
  coord_fixed(clip = "off") +
  theme_void()

##  circulos concéntricos ----
ggplot(data = NULL, aes(x0 = 1, y0 = 1)) +
  geom_circle(aes(r = proporcion_fortuna * 0.01), fill = "gray40", color = "gray40") +
  geom_circle(aes(r = proporcion_farkas * 0.10), fill = "blue", color = "blue") +  
  geom_circle(aes(r = proporcion_sueldo_fortuna), fill = "red", color = "red") +
  #textos
  # annotate("segment", x = 0.990, y = 1.010, xend = 1.010, yend = 0.990) + #
  annotate("text", label = "1% de fortuna", x = 0.9935, y = 1.0066,  hjust = 0) +
  annotate("text", label = "1% de Farkas", x = 0.9993 + (proporcion_farkas * 0.10), 
           y = 1.0002 - (proporcion_farkas * 0.10), hjust = 0) +
  annotate("text", label = "Tu", x = 1.00004 + proporcion_sueldo_fortuna, y = 0.9996 - proporcion_sueldo_fortuna, hjust = 0) +
  coord_fixed(clip = "off") +
  theme_void()


# graficar ----
# forbes <- readr::read_csv2("forbes_2023_chile.csv")
# millonarios <- readRDS("millonarios_chile.rds")

library(ggplot2)

millonarios |> 
  mutate(nombre = factor(nombre, millonarios$nombre)) |> 
  ggplot(aes(nombre, fortuna)) +
  geom_point() +
  geom_segment(aes(xend = nombre, yend = 0)) +
  geom_text(aes(label = miles(fortuna)), 
            angle = 30, hjust = 0, size = 3, nudge_x = 0.1, vjust = 0) +
  scale_y_continuous(expand = expansion(c(0, 0.15)), labels = ~paste("USD", miles(.x))) +
  theme_minimal() +
  labs(y = "Fortuna (en millones de dólares)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x = element_blank()
  ) 





# deciles ----
#usando variable de deciles


casen |> 
  select(expr, ytrabajocor, dau) |> #el decil es por ingreso de hogares
  filter(!is.na(ytrabajocor),
         !is.na(dau)) |> 
  mutate(ytrabajocor_exp = ytrabajocor * expr) |> 
  group_by(dau) |> 
  summarize(#suma_ingresos = sum(ytrabajocor_exp),
    promedio_ingresos = weighted.mean(ytrabajocor, expr) |> as.integer(),
    mediana_ingresos = weighted.median(ytrabajocor, expr),
    min_ingresos = min(ytrabajocor),
    max_ingresos = max(ytrabajocor))


##  encontrar en qué porcentaje de ingresos se ubica un ingreso ----
ingreso = 290000
which(abs(casen_porcentajes_deciles - ingreso) == min(abs(casen_porcentajes_deciles - ingreso)))

encontrar_decil <- function(casen_porcentajes_deciles, ingreso) {
  which(abs(casen_porcentajes_deciles - ingreso) == min(abs(casen_porcentajes_deciles - ingreso)))
}

decil <- encontrar_decil(casen_porcentajes_deciles, ingreso = 290000)

decil = 6

ggplot(data = NULL, aes(y = 1)) +
  geom_point(aes(x = 1:10), size = 3, alpha = 0.35) +
  geom_point(aes(x = decil), size = 6) +
  # geom_text(aes(x = c(1, 3, 5, 7, 10), y = 1.04,
  #               label = c("10%", "30%", "50%", "70%", "100%"))) +
  coord_fixed(ratio = 6, clip = "off") +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 10),
                     labels = c("10%", "30%", "50%", "70%", "100%")) +
  theme_void() +
  theme(axis.text.x = element_text())


                  
### OK calcular ingresos de la mitad de chile ----
ingresos_mitad_baja = casen_deciles |> filter(decil <= 5) |> summarize(sum(suma_ingresos)) |> pull()
ingresos_mitad_alta = casen_deciles |> filter(decil > 5) |> summarize(sum(suma_ingresos)) |> pull()


fortuna > ingresos_mitad_baja

scales::percent(ingresos_mitad_baja/fortuna)

#graficar esto! ----
tibble("etiqueta" = c("La mitad de los ingresos\nde todos los chilenos", "Culiao"),
       "valor" = c(ingresos_mitad_baja, fortuna)) |> 
ggplot(aes(valor, etiqueta, fill = etiqueta)) +
geom_col() +
  scale_x_continuous(expand = expansion(0)) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        axis.text.x = element_blank(), 
        axis.title = element_blank())
