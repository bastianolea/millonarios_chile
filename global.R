library(shiny)
library(dplyr)
library(glue)
library(shinyWidgets)
library(readr)
library(stringr)
library(thematic)
library(ggplot2)
library(ggforce)
library(shinycssloaders)


color_fondo = "#0D3C67"
# color_texto = "#19C461"
# color_fondo = "#243665"
color_texto = "#8BD8BD"
# color_fondo = "#2E3C7E"
# color_texto = "#FBEAEB"
# color_detalle = "#1a4c70"
color_detalle = "#387285"
color_claro = "#1a4c70"
color_destacado = "#a9cf76"

degradado <- colorRampPalette(c("#a9cf76", "#387285", "#387285"))

options(spinner.type = 4, spinner.color = color_detalle)

#tema automático
thematic_shiny(font = "auto", bg = color_fondo, fg = color_texto, accent = color_texto)
# thematic::thematic_on(bg = color_fondo, fg = color_texto, accent = color_texto)

options(scipen = 9999)

source("funciones.R")

#datos ----
# millonarios <- readr::read_csv("millonarios_2023_billionaires.csv")
# millonarios <- read_csv2("millonarios_2023_chile.csv")
millonarios <- read_rds("millonarios_chile.rds")
casen_porcentajes_deciles <- readr::read_rds("datos/casen_cortes_deciles.rds")
casen_deciles <- readr::read_csv2("datos/casen_deciles.csv")

poblacion_chile_2024 = 20086377

monto_teleton_2023 = 44000000000
monto_chile_se_levanta_2024 = 5544577916

dolar <- obtener_dolar(scrapear = F)

css <- function(text) {
  tags$style(glue(text, .open = "{{", .close = "}}"))
}

cifra <- function(x) {
  tags$span(x, style = glue("color: {color_destacado}; 
                            font-size: 130%; 
                            line-heignt: 0.2; display:inline-block; 
                            margin-top: -80px;
                            font-weight: bold;"))
}
