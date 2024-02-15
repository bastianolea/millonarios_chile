library(shiny)
library(dplyr)
library(glue)
library(shinyWidgets)
library(readr)
library(stringr)
library(thematic)
library(ggplot2)
library(ggforce)


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

ui <- fluidPage(
  title = "Millonarios de Chile",
  lang = "es",
  
  theme = bslib::bs_theme(
    bg = color_fondo, fg = color_texto, primary = color_texto,
    # base_font = bslib::font_google("Pacifico")
  ),
  
  js_ancho_ventana(),
  
  css("#edad_laboral {
      height: 33px; 
      margin-top: 0;
      }"),
  
  css(".shiny-input-number {
      background-color: {{color_claro}};
      border: none;
  }"),
  
  css(".dropdown-toggle { font-size: 90%; font-weight: normal; padding-left: 12px;}"),
  css(".explicacion {
      font-size: 80%; opacity: 0.65; line-height: 1.4;
      }"),
  
  fluidRow(
    #título ----
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
  
  # gráfico millonarios ----
  fluidRow(
    column(12,
           plotOutput("grafico_millonarios")
    )
  ),
  
  hr(),
  
  ## input millonario ----
  fluidRow(
    column(4, align = "right",
           h5("Seleccione un multimillonario"),
           pickerInput("millonario", 
                       NULL, selected = "Sebastián Piñera Echeñique",
                       choices = millonarios$nombre, 
           )
    ),
    column(8,
           
           #fortuna ----
           p("La plata del loco es de ",
             textOutput("fortuna_usd", inline = T), "millones de dólares, lo que equivale a",
             cifra(textOutput("fortuna_millones", inline = T)), cifra("millones"), "de pesos chilenos"),
           
           p("Con todo ese dinero, se podría pagar un sueldo al", textOutput("sueldos_repartibles", inline = T) |> cifra(),
             "de la población del país"),
           
           ## 50% de chile ----
           p("Si", strong("sumamos"), "todos los ingresos de", strong("la mitad de los chilenos"), "(50% inferior), 
             y comparamos todo ese dinero a la fortuna de", textOutput("nombre_millonario_coma", inline = T),
             "entonces su fortuna sería un", textOutput("fortuna_porcentaje_mitad_chile", inline = T) |> cifra(),
             "más alta"),
           
           plotOutput("grafico_barras_50", height = 140),
           
           hr(),
    )
  ),
  
  
  # sueldo ----
  fluidRow(
    column(4, align = "right",
           ## input sueldo ----
           h5("Ingresa tu sueldo"),
           numericInput("sueldo", NULL, 
                        value = 500000, min = 200000, max = 20000000, step = 100000,
                        width = "170px"
           ),
           p(class = "explicacion",
             "El dato se usará para comparar las fortunas con tus propios ingresos. Puedes ingresar un estimado. Este dato no se almacena, solo es con fines ilustrativos.", 
           )
    ),
    column(8,
           h2("Compara tu sueldo con la fortuna de un multimillonario chileno"),
           
           
           ## decil ----
           p("Tu sueldo de aproximadamente", textOutput("texto_sueldo", inline = T),
             "te ubica dentro del", strong(textOutput("decil_ingresos", inline = T), "de mayores ingresos del país."), 
             "Esto significa que recibes más ingresos mensuales que el", textOutput("decil_ingresos2", inline = T), "de los chilenos."
           ),
           
           div(plotOutput("grafico_decil", height = 40),
               style = "margin-bottom: 24px;"),
           
           p("Sin embargo, la fortuna de", textOutput("nombre_millonario3", inline = T), "es equivalente a", cifra(textOutput("equivalencia_en_sueldos", inline = T)), cifra("sueldos tuyos")),
           
           hr()
    )
  ),
  
  # cuantos tendrías que trabajar ----
  fluidRow(
    column(4, align = "right",
           ## inputs genero y edad ----
           radioGroupButtons("genero", "Género", 
                             choices = c("Femenino", "Masculino"), 
                             size = "sm",
           ),
           
           numericInput("edad_laboral", "A qué edad empezaste a trabajar",
                        value = 24, min = 10, max = 99, step = 1, width = "170px"
           ),
           
           p(class = "explicacion",
             "Estos datos se usan para calcular la expectativa de vida y la edad de jubilación, y no son almacenados, sólo se utilizan para mejorar las estimaciones."
           )
    ),
    
    column(8, 
           h2("¿Cuánto tendrías que trabajar para ser multimillonario?"),
           
           p("Considerando que en un año ganas", textOutput("sueldo_anual", inline = T),
             "entonces tendrías que trabajar", textOutput("dias_trabajando", inline = T) |> cifra(), 
             cifra("días hábiles"), "para igualar la fortuna de", textOutput("nombre_millonario_punto", inline = T)),
           
           p("Esto equivale a", cifra(textOutput("años_trabajando", inline = T)), cifra("años!")),
           
           p("Tendrías que trabajar sin parar, todos los días, hasta el año ", textOutput("año_trabajado_final", inline = T)),
           
           ## cuantas vidas ---- 
           p("Considerando que tu esperanza de vida actual es de", textOutput("esperanza_vida", inline = T), "años,
           y que tu edad de jubilación será a los", textOutput("edad_jubilacion", inline = T), "años,", 
           cifra("necesitarías trabajar"), cifra(textOutput("vidas_trabajadas", inline = T)),
           cifra("vidas enteras"), "para alcanzar la fortuna de", textOutput("nombre_millonario_punto2", inline = T),
           ),
           hr(),
           
           # podrías ser multimillonario ----
           fluidRow(
             column(12,
                    h2("¿Podrías ser multimillonario?"),
                    p("En toda tu vida, vas a ganar", cifra(textOutput("sueldo_vital", inline = T))),
                    
                    p("En este gráfico se visualizan tres círuclos cuyo tamaño es proporcional a tres cifras. 
                      Al centro se ubica un círculo que representa una estimación de", strong("todo el dinero que ganarás en tu vida."), 
                      "Al rededor, se ubica un primer círculo que representa el 10% de la fortuna de Leonardo Farkas, famoso millonario Chileno,
                      y al rededor, el 1% de la fortuna de", textOutput("nombre_millonario4", inline = T)
                    ),
                    
                    plotOutput("grafico_circulo_fortuna"),
                    hr(),
             )
           )
    )
  ),
  
  
  
  
  ## firma ----
  fluidRow(
    column(12, 
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
  
  # millonario ----
  millonario <- reactive(
    millonarios |> 
      filter(nombre == input$millonario)
  )
  
  ## nombre millonario ----
  output$nombre_millonario5 <- output$nombre_millonario4 <- output$nombre_millonario3 <- output$nombre_millonario2 <- output$nombre_millonario <- renderText(input$millonario)
  output$nombre_millonario_coma <- renderText(paste0(input$millonario, ","))
  output$nombre_millonario_punto2 <- output$nombre_millonario_punto <- renderText(paste0(input$millonario, "."))
  
  
  ## fortuna ----
  fortuna <- reactive(
    millonario()$fortuna_pesos
  )
  
  output$fortuna <- renderText(pesos(fortuna()))
  output$fortuna_usd <- renderText(pesos(millonario()$fortuna))
  output$fortuna_millones <- renderText(pesos(fortuna()/1000000))
  
  
  
  # individuo -----
  sueldo <- reactive({
    message("sueldo")
    
    ifelse(input$sueldo > 9999999, 9999999, input$sueldo)
  })
  output$texto_sueldo <- renderText(pesos(sueldo()))
  
  
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
  
  output$sueldo_anual <- renderText(paste0(pesos(sueldo_anual()), ","))
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
  
  output$sueldo_vital <- renderText(paste0(pesos(sueldo_vital()), "."))
  output$vidas_trabajadas <- renderText(format(vidas_trabajo(), big.mark = "."))
  output$esperanza_vida <- renderText(esperanza_vida())
  output$edad_jubilacion <- renderText(edad_jubilacion())
  
  
  #sueldos repartibles ----
  sueldos_repartibles <- reactive({
    fortuna()/sueldo() #personas que podrían ganar x sueldo con la fortuna
    
    poblacion_chile_2024 = 20086377
    fortuna_en_sueldos_minimos = fortuna()/500000
    
    scales::percent(fortuna_en_sueldos_minimos/poblacion_chile_2024, accuracy = 1.11)
    # Se pordría pagar un sueldo mínimo al x% de la población chilena
  })
  
  output$sueldos_repartibles <- renderText(sueldos_repartibles())
  
  #decil del sueldo ----
  decil_ingresos <- reactive({
    decil <- encontrar_decil(casen_porcentajes_deciles, sueldo())
  })
  output$decil_ingresos2 <- output$decil_ingresos <- renderText(paste0(decil_ingresos(), "0%"))
  
  
  ### ingresos de la mitad de chile ----
  
  ingresos_mitad_baja <-  reactive({
    casen_deciles |> filter(decil <= 5) |> summarize(sum(suma_ingresos)) |> pull()
  })
  # ingresos_mitad_alta = casen_deciles |> filter(decil > 5) |> summarize(sum(suma_ingresos)) |> pull()
  
  fortuna_porcentaje_mitad_chile <- reactive({
    # browser()
    division <- fortuna()/ingresos_mitad_baja()
    scales::percent(division)
  })
  
  output$fortuna_porcentaje_mitad_chile <- renderText(fortuna_porcentaje_mitad_chile())
  
  
  
  
  #ancho de ventana ----
  ancho <- reactive({
    req(length(input$dimension[1]) != 0)
    req(input$dimension[1] > 0)
    
    input$dimension[1]
  }) |> bindEvent(input$dimension)
  
  ancho_ventana <- ancho |> debounce(500)
  
  
  
  #graficos ----
  
  ## decil ----
  
  output$grafico_decil <- renderPlot({
    ggplot(data = NULL) +
      geom_point(aes(x = 0:10, y = 1), size = 3, alpha = 0.35) +
      geom_point(aes(x = decil_ingresos(), y = 1), size = 7, color = color_destacado) +
      # geom_text(aes(x = c(1, 3, 5, 7, 10), y = 1.05,
      #               label = c("10%", "30%", "50%", "70%", "100%")),
      #           size = 4) +
      scale_x_continuous(breaks = c(0, 3, 5, 7, 10),
                         labels = c("0%", "30%", "50%", "70%", "100%")) +
      theme_void() +
      theme(axis.text.x = element_text())
  })
  
  ## millonarios ----
  output$grafico_millonarios <- renderPlot({
    cantidad_x <- ancho_ventana()/50
    # browser()
    
    millonarios |> 
      slice(1:cantidad_x) |> 
      mutate(nombre = factor(nombre, millonarios$nombre)) |> 
      ggplot(aes(nombre, fortuna)) +
      geom_point(size = 3) +
      geom_point(size = 7, alpha = 0.35) +
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
  
  ## circulo ----
  farkas <- reactive(millonarios |> filter(nombre == "Leonardo Farkas")  |> pull(fortuna_pesos))
  
  output$grafico_circulo_fortuna <- renderPlot({
    
    proporcion_fortuna = 1 #fortuna/fortuna
    proporcion_farkas = farkas()/fortuna()
    proporcion_sueldo_fortuna = sueldo_vital()/fortuna()
    
    ggplot(data = NULL, aes(x0 = 1, y0 = 1)) +
      geom_circle(aes(r = proporcion_fortuna * 0.01), fill = color_claro, color = color_claro) +
      geom_circle(aes(r = proporcion_farkas * 0.10), fill = color_detalle, color = color_detalle) +  
      geom_circle(aes(r = proporcion_sueldo_fortuna), fill = color_destacado, color = color_destacado) +
      #textos
      # annotate("segment", x = 0.990, y = 1.010, xend = 1.010, yend = 0.990) + #
      annotate("text", label = paste("1% de", input$millonario), x = 0.9935, y = 1.0066,  hjust = 0) +
      annotate("text", label = "10% de Farkas", x = 0.9993 + (proporcion_farkas * 0.10), 
               y = 1.0002 - (proporcion_farkas * 0.10), hjust = 0) +
      annotate("text", label = "Tu", x = 1.00004 + proporcion_sueldo_fortuna, y = 0.9996 - proporcion_sueldo_fortuna, hjust = 0) +
      coord_fixed(clip = "off") +
      theme_void()
  })
  
  ## barras chile vs millonario ----
  output$grafico_barras_50 <- renderPlot({
    # division <- fortuna()/ingresos_mitad_baja()

    tabla <- tibble("etiqueta" = c("La mitad de los ingresos\nde todos los chilenos", input$millonario),
           "valor" = c(ingresos_mitad_baja(), fortuna()))
    
    tabla |> 
      mutate(etiqueta = factor(etiqueta, tabla$etiqueta)) |> 
      ggplot(aes(valor, etiqueta, fill = etiqueta)) +
      geom_col(width = 0.5) +
      scale_x_continuous(expand = expansion(0)) +
      scale_fill_manual(values = c(color_detalle, color_destacado)) +
      theme_minimal() +
      theme(legend.position = "none", 
            axis.text.y = element_text(color = color_texto),
            panel.grid = element_blank(),
            axis.text.x = element_blank(), 
            axis.title = element_blank())
  }, res = 120)
}

shinyApp(ui = ui, server = server)
