library(shiny)
library(dplyr)
library(shinyWidgets)
library(readr)
library(stringr)
library(thematic)

color_fondo = "#0D3C67"
color_texto = "#19C461"
# color_fondo = "#243665"
color_texto = "#8BD8BD"
# color_fondo = "#2E3C7E"
# color_texto = "#FBEAEB"

#tema automático
thematic_shiny(font = "auto", bg = color_fondo, fg = color_texto, accent = color_texto)

options(scipen = 9999)

# forbes <- readr::read_csv("forbes_2023_billionaires.csv")
forbes <- read_csv2("forbes_2023_chile.csv")

source("precio_dolar.R")
dolar <- obtener_dolar(scrapear = F)

pesos <- function(x) {
  paste0("$", format(x, big.mark = ".", decimal.mark = ","))
}

miles <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",")
}


ui <- fluidPage(
  theme = bslib::bs_theme(
    bg = color_fondo, fg = color_texto, primary = color_texto,
    # base_font = bslib::font_google("Pacifico")
  ),
  
  fluidRow(
    column(12,
           h2("Platita")
    )
  ),
  
  fluidRow(
    column(12,
           pickerInput("millonario", 
                       "Seleccione un ctm", selected = "Sebastián Piñera",
                       choices = forbes$nombre),
           
           p("La plata del loco es", textOutput("fortuna", inline = T))
    )
  ),
  
  fluidRow(
    column(12,
           numericInput("sueldo", "Ingrese su sueldo aprox", 
                        value = 500000, min = 200000, max = 20000000, step = 100000),
           
           # p("Tu sueldo es", textOutput("texto_sueldo", inline = T)),
           
           p("Al año ganas", textOutput("sueldo_anual", inline = T)),
           
           
           
           p("Con su sueldo, tendrías que trabajar", textOutput("dias_trabajando", inline = T), "días hábiles"),
           
           p("Su fortuna es equivalente a", textOutput("equivalencia_en_sueldos", inline = T), "sueldos tuyos"),
           
           p("Con su sueldo, tendrías que trabajar", textOutput("años_trabajando", inline = T), "años"),
           
           p("Con su sueldo, tendrías que trabajar", textOutput("siglos_trabajando", inline = T), "siglos"),
           
           p("Tendrías que trabajar sin parar, todos los días, hasta el año ", textOutput("año_trabajado_final", inline = T))
    )
  ),
  
  fluidRow(
    column(12, 
           radioGroupButtons("genero", "Género", 
                             choices = c("Femenino", "Masculino")
           ),
           
           numericInput("edad_laboral", "A qué edad empezaste a trabajar",
                        value = 24, min = 10, max = 99, step = 1
           ),
           
           #output
           p("En toda tu vida, vas a ganar", textOutput("sueldo_vital", inline = T)),
           p("Necesitarías trabajar", 
             textOutput("vidas_trabajadas", inline = T),
             "vidas enteras para alcanzar la fortuna de x"),
           p("considerando que vas a vivir hasta los", textOutput("esperanza_vida", inline = T)),
           p("Y que tu edad de jubilación será a los", textOutput("edad_jubilacion", inline = T))
    )
  ),
  
  fluidRow(
    column(12, 
           hr(),
           p("Diseñado y programado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           p(
             "Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/millonarios_chile")
           ),
           
           div(style = "height: 40px")
           
    )
  )
  
)

server <- function(input, output) {
  
  sueldo <- reactive({
    message("sueldo")
    
    ifelse(input$sueldo > 9999999, 9999999, input$sueldo)
  })
  
  output$texto_sueldo <- renderText(sueldo())
  
  
  millonario <- reactive(
    forbes |> 
      filter(nombre == input$millonario)
  )
  
  fortuna <- reactive(
    millonario()$fortuna_pesos
  )
  
  output$fortuna <- renderText(pesos(fortuna()))
  
  output$fortuna_millones <- renderText(pesos(fortuna()/1000000))
  
  
  
  # individuo -----
  # años que debiese trabajar una persona para obtener la fortuna
  
  #fortuna equivalente en sueldos
  sueldos_individuo <- reactive({
    fortuna()/sueldo()
  })
  
  #días laborales para obtener su fortuna
  dias_trabajo_individuo <- reactive({
    dias_habiles_mes = 20
    sueldo_diario = sueldo()/dias_habiles_mes
    as.integer(fortuna()/sueldo_diario)
  })
  
  output$dias_trabajando <- renderText(miles(dias_trabajo_individuo()))
  
  
  # #años trabajando para obtener su fortuna
  sueldo_anual = reactive(sueldo() * 12)
  
  años_trabajo_individuo <- reactive({
    as.integer(fortuna()/sueldo_anual())
  })
  
  output$sueldo_anual <- renderText(pesos(sueldo_anual()))
  output$equivalencia_en_sueldos <- renderText(miles(sueldos_individuo()))
  
  output$siglos_trabajando <- renderText(miles(round(años_trabajo_individuo()/100, 0)))
  output$años_trabajando <- renderText(miles(años_trabajo_individuo()))
  
  # output$milenios_trabajando <- renderText(as.integer(años_trabajo_individuo()/1000))
  
  output$año_trabajado_final <- renderText({
    año = format(Sys.Date(), "%Y")
    año_final <- as.numeric(año) + años_trabajo_individuo()
    miles(as.integer(año_final))
    })
  
  
  # vidas trabajadas ----
  # https://cooperativa.cl/noticias/pais/poblacion/chile-alcanzo-un-nuevo-record-en-la-esperanza-de-vida-supero-los-81/2024-02-05/065352.html
  # https://www.df.cl/economia-y-politica/actualidad/efecto-millennials-edad-promedio-de-los-ocupados-sube-a-44-anos
  
  esperanza_vida = reactive(ifelse(input$genero == "Femenino", 83.5, 78.5))
  edad_jubilacion = reactive(ifelse(input$genero == "Masculino", 60, 65))
  
  sueldo_vital <- reactive({
    años_jubilacion = esperanza_vida() - edad_jubilacion()
    
    años_laborales = esperanza_vida() - input$edad_laboral - años_jubilacion
    
    # #sueldo lineal: mismo sueldo toda la vida
    # sueldo_vida <- sueldo_anual() * años_laborales
    # as.integer(fortuna/sueldo_vida)
    
    #sueldo exponencial: considerando que los primeros 10 años de la vida uno ganó menos
    sueldo_primeros_años = sueldo_anual() * seq(0.5, 1, by = 0.1)
    sueldo_vida <- sum(sueldo_primeros_años) + (sueldo_anual() * (años_laborales-6))
    sueldo_vida
  })
  
  vidas_trabajo <- reactive({
    as.integer(fortuna()/sueldo_vital())
  })
  
  output$sueldo_vital <- renderText(pesos(sueldo_vital()))
  output$vidas_trabajadas <- renderText(format(vidas_trabajo(), big.mark = "."))
  output$esperanza_vida <- renderText(esperanza_vida())
  output$edad_jubilacion <- renderText(edad_jubilacion())
  
}

shinyApp(ui = ui, server = server)
