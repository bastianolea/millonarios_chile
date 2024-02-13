library(shiny)
library(dplyr)
library(shinyWidgets)
library(readr)
library(stringr)
library(thematic)
library(ggplot2)

color_fondo = "#0D3C67"
color_texto = "#19C461"
# color_fondo = "#243665"
color_texto = "#8BD8BD"
# color_fondo = "#2E3C7E"
# color_texto = "#FBEAEB"
color_detalle = "#1a4c70"
color_detalle = "#387285"

#tema automático
thematic_shiny(font = "auto", bg = color_fondo, fg = color_texto, accent = color_texto)
# thematic::thematic_on(bg = color_fondo, fg = color_texto, accent = color_texto)

options(scipen = 9999)

source("funciones.R")

# millonarios <- readr::read_csv("millonarios_2023_billionaires.csv")
# millonarios <- read_csv2("millonarios_2023_chile.csv")
millonarios <- read_rds("millonarios_chile.rds")



dolar <- obtener_dolar(scrapear = F)



ui <- fluidPage(
  title = "Millonarios de Chile",
  lang = "es",
  
  theme = bslib::bs_theme(
    bg = color_fondo, fg = color_texto, primary = color_texto,
    # base_font = bslib::font_google("Pacifico")
  ),
  
  js_ancho_ventana(),
  
  css("
      #edad_laboral {
      height: 33px; 
      margin-top: 0;
      }
  "),
  
  fluidRow(
    ##título ----
    div(style = "padding-top: 12px; padding-bottom: 20px;",
        
        titlePanel(h1("Millonarios de Chile"), 
                   windowTitle = "Millonarios de Chile"),
        p("Aplicación interactiva sobre las fortunas de los empresarios más ricos de Chile", 
          style = "margin-bottom: 8px; font-size: 90%;"),
        em(tags$a("Bastián Olea Herrera", 
                  href = "http://bastian.olea.biz",
                  target = "_blank")),
        hr()
    )
  ),
  
  fluidRow(
    column(12,
           plotOutput("grafico_millonarios")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           pickerInput("millonario", 
                       "Seleccione un ctm", selected = "Sebastián Piñera Echeñique",
                       choices = millonarios$nombre),
           
           p("La plata del loco es de ",
             textOutput("fortuna_usd", inline = T), "millones de dólares, lo que equivale a",
             textOutput("fortuna", inline = T), "pesos chilenos")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           h2("Compara tu sueldo con la fortuna de un multimillonario chileno"),
           
           numericInput("sueldo", "Ingrese su sueldo aprox", 
                        value = 500000, min = 200000, max = 20000000, step = 100000),
           
           # p("Tu sueldo es", textOutput("texto_sueldo", inline = T)),
           
           p("Al año ganas", textOutput("sueldo_anual", inline = T)),
           
           p("Con su sueldo, tendrías que trabajar", textOutput("dias_trabajando", inline = T), "días hábiles"),
           
           p("Su fortuna es equivalente a", textOutput("equivalencia_en_sueldos", inline = T), "sueldos tuyos"),
           
           p("Con su sueldo, tendrías que trabajar", textOutput("años_trabajando", inline = T), "años"),
           
           p("Con su sueldo, tendrías que trabajar", textOutput("siglos_trabajando", inline = T), "siglos"),
           
           p("Tendrías que trabajar sin parar, todos los días, hasta el año ", textOutput("año_trabajado_final", inline = T)),
           
           hr()
    )
    
  ),
  
  fluidRow(
    column(12, 
           h2("¿Cuánto tendrías que trabajar para ser multimillonario?"),
           
           div(style = "display: inline-block;",
               
               div(style = "display: inline-block; padding: 6px;",
               radioGroupButtons("genero", "Género", 
                                 choices = c("Femenino", "Masculino"), size = "sm",
               )),
               
               div(style = "display: inline-block; padding: 6px;",
               numericInput("edad_laboral", "A qué edad empezaste a trabajar",
                            value = 24, min = 10, max = 99, step = 1
               ))
           ),
           
           #output
           p("En toda tu vida, vas a ganar", textOutput("sueldo_vital", inline = T)),
           
           p("considerando que tu esperanza de vida actual es de", textOutput("esperanza_vida", inline = T), "años,
           y que tu edad de jubilación será a los", textOutput("edad_jubilacion", inline = T), "años, 
           necesitarías trabajar", 
           textOutput("vidas_trabajadas", inline = T),
           "vidas enteras para alcanzar la fortuna de",
           textOutput("nombre_millonario", inline = T),
           ),
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
    millonarios |> 
      filter(nombre == input$millonario)
  )
  output$nombre_millonario <- renderText(input$millonario)
  
  fortuna <- reactive(
    millonario()$fortuna_pesos
  )
  
  output$fortuna <- renderText(pesos(fortuna()))
  output$fortuna_usd <- renderText(pesos(millonario()$fortuna))
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
  
  #ancho de ventana ----
  ancho <- reactive({
    req(length(input$dimension[1]) != 0)
    req(input$dimension[1] > 0)
    
    input$dimension[1]
  }) |> bindEvent(input$dimension)
  
  ancho_ventana <- ancho |> debounce(500)
  
  #graficos ----
  output$grafico_millonarios <- renderPlot({
    cantidad_x <- ancho_ventana()/50
    # browser()
    
    millonarios |> 
      slice(1:cantidad_x) |> 
      mutate(nombre = factor(nombre, millonarios$nombre)) |> 
      ggplot(aes(nombre, fortuna)) +
      geom_point(size = 3) +
      geom_point(size = 6, alpha = 0.35) +
      geom_segment(aes(xend = nombre, yend = 0), linewidth = 1.5) +
      geom_text(aes(label = miles(fortuna)), 
                angle = 30, hjust = 0, size = 3, nudge_x = 0.3, nudge_y = 100, vjust = 0) +
      scale_y_continuous(expand = expansion(c(0, 0.15)), labels = miles) +
      theme_minimal() +
      coord_cartesian(clip = "off") +
      labs(y = "Fortuna (en millones de dólares)") +
      theme(axis.text.x = element_text(size = 10, angle = 30, hjust = 1, color = color_texto),
            axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5, color = color_detalle),
            axis.title.x = element_blank(),
            panel.grid = element_line(color = color_detalle),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            text = element_text(color = color_texto)
      ) 
  })
  
}

shinyApp(ui = ui, server = server)
