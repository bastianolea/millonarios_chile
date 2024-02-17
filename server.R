library(shiny)

server <- function(input, output) {
  
  # millonario ----
  millonario <- reactive(
    millonarios |> 
      filter(nombre == input$millonario)
  )
  
  ## nombre millonario ----
  output$nombre_millonario8 <- output$nombre_millonario7 <- output$nombre_millonario6 <- output$nombre_millonario5 <- output$nombre_millonario4 <- output$nombre_millonario3 <- output$nombre_millonario2 <- output$nombre_millonario <- renderText(input$millonario)
  output$nombre_millonario_coma <- renderText(paste0(input$millonario, ","))
  output$nombre_millonario_punto2 <- output$nombre_millonario_punto <- renderText(paste0(input$millonario, "."))
  
  ## datos millonario ----

  output$millonario_genero <- renderText(ifelse(millonario()$género == "m", "un empresario", "una empresaria"))
  
  output$millonario_categorias <- renderText({
    categorias <- millonario()$categoria |> tolower() |> stringr::str_replace("(\\,)(?!.*\\,)", " y")
    paste0(categorias, ".")
    })
  
  output$millonario_rank_forbes <- renderUI({
    if (!is.na(millonario()$rank)) {
      p("Ocupó el", cifra("puesto N°"), cifra(millonario()$rank), 
        "en el listado", strong("Forbes", millonario()$año), "de las mayores fortunas a nivel mundial."
        )
    }
  })
  
  # millonario$familia == "Sí"
  
  
  ## fortuna ----
  fortuna <- reactive(
    millonario()$fortuna_pesos
  )
  
  output$fortuna <- renderText(pesos(fortuna()))
  output$fortuna_usd <- renderText(pesos(millonario()$fortuna))
  output$fortuna_millones <- renderText(pesos(fortuna()/1000000))
  
  
  ### comparar fortuna ----
  output$fortuna_comparar_teleton <- renderText({
    cantidad <- (fortuna()*0.1)/monto_teleton_2023
    round(cantidad, 1)
  })
  
  # https://www.adnradio.cl/2024/02/17/juntos-chile-se-levanta-logra-millonaria-recaudacion-este-fue-el-monto-recaudado-en-el-evento-solidario-por-los-damnificados-en-los-incendios/
  output$fortuna_comparar_chileselevanta <- renderText({
    cantidad <- (fortuna()*0.1)/monto_chile_se_levanta_2024
    round(cantidad, 1)
  })
  
  
  ### bono ----
  bono_millonario <- reactive({
    bono <- fortuna()/poblacion_chile_2024
    # round(bono) |> floor(4)
    signif(bono, digits = 3)
  })
  output$bono_millonario <- renderText(bono_millonario() |> pesos())
  
  
  
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
  
  
  output$genero <- renderText(tolower(input$genero))
  
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
  output$vidas_trabajadas <- renderText(format(vidas_trabajo(), big.mark = ".", decimal.mark = ","))
  output$esperanza_vida <- renderText(esperanza_vida())
  output$edad_jubilacion <- renderText(edad_jubilacion())
  
  
  #sueldos repartibles ----
  sueldos_repartibles <- reactive({
    fortuna()/sueldo() #personas que podrían ganar x sueldo con la fortuna
    
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
  
  ingresos_mitad_baja <- reactive({
    casen_deciles |> filter(decil <= 5) |> summarize(sum(suma_ingresos)) |> pull()
  })
  
  ingresos_mitad_alta <- reactive({
    casen_deciles |> filter(decil > 5) |> summarize(sum(suma_ingresos)) |> pull()
  })
  
  fortuna_porcentaje_mitad_chile <- reactive({
    # browser()
    division <- fortuna()/ingresos_mitad_baja()
    scales::percent(division)
  })
  
  output$fortuna_porcentaje_mitad_chile <- renderText(fortuna_porcentaje_mitad_chile())
  
  
  
  #top 10 ----
  top_10_millonarios <- reactive({
    millonarios |> 
      arrange(desc(fortuna)) |> 
      slice_max(fortuna, n = 10, with_ties = F) |> 
      mutate(nombre = factor(nombre, rev(nombre)))
  })
  
  ingresos_chile <- reactive({
    casen_deciles |> summarize(sum(suma_ingresos)) |> pull()
  })
  
  suma_top_10_fortunas <- reactive(sum(top_10_millonarios()$fortuna_pesos))
  
  output$top_fortunas_vs_chile <- renderText({
    proporcion_fortunas_vs_chile = suma_top_10_fortunas()/ingresos_chile()
    format(proporcion_fortunas_vs_chile, digits = 2, decimal.mark =",")
  })
  
  output$top_fortunas_vs_chile_porcentaje <- renderText({
    porcentaje_fortunas_vs_chile = ingresos_chile()/suma_top_10_fortunas()
    scales::percent(porcentaje_fortunas_vs_chile, accuracy = 1)
  })
  
  
  #persona mas rica ----
  mayor_millonario <- reactive(millonarios |> slice_max(fortuna))
  
  precio_cesfam <- mean(c(7557000000, 6300000000, 7819818000, 4786766000))
  precio_hospital <- 100000000 * dolar #http://www.supersalud.gob.cl/prensa/672/w3-printer-3445.html #https://www.eldinamo.cl/pais/2019/08/07/us10-000-millones-y-75-hospitales-este-es-el-plan-nacional-de-inversiones-2108-2022/
  precio_casa <- 47200000
  
  output$mayor_millonario_nombre <- renderText(paste0(mayor_millonario()$nombre, ","))
  output$mayor_millonario_fortuna <- renderText(mayor_millonario()$fortuna |> pesos())
  
  output$mayor_millonario_cesfam <- renderText(round(mayor_millonario()$fortuna_pesos/precio_cesfam) |> miles())
  output$mayor_millonario_hospitales <- renderText(round(mayor_millonario()$fortuna_pesos/precio_hospital) |> miles())
  output$mayor_millonario_casas <- renderText(round(mayor_millonario()$fortuna_pesos/precio_casa) |> miles())
  
  bono_iris_fontbona <- reactive({
    # mayor_millonario()$fortuna_pesos/poblacion_chile_2024 #plata que podría darle a cada chileno
    beneficiados_bono <- mayor_millonario()$fortuna_pesos/1000000 #personas a las que les podría entregar 1 palo
    beneficiados_extra <- beneficiados_bono - poblacion_chile_2024 #personas que sobran
    millones_de_dolares_sobrantes <- ((beneficiados_extra * 1000000) / dolar) / 1000000 #millones de dolares sobrantes
    
    return(millones_de_dolares_sobrantes)
  })
  
  output$bono_iris_sobrantes <- renderText(round(bono_iris_fontbona()) |> pesos())
  
  
  
  #ancho de ventana ----
  ancho <- reactive({
    req(length(input$dimension[1]) != 0)
    req(input$dimension[1] > 0)
    # message(input$dimension[1])
    input$dimension[1]
  }) #|> bindEvent(input$dimension)
  
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
      scale_x_continuous(breaks = c(0, 5, 10),
                         labels = c("0%", "50%", "100%")) +
      theme_void() +
      theme(axis.text.x = element_text())
  }, res = 120)
  
  ## millonarios ----
  output$grafico_millonarios <- renderPlot({
    cantidad_x <- ancho_ventana()/50
    # browser()
    
    millonarios |> 
      slice(1:cantidad_x) |> 
      mutate(nombre = factor(nombre, millonarios$nombre)) |> 
      ggplot(aes(nombre, fortuna)) +
      geom_point(size = 3) +
      geom_point(size = 7, alpha = 0.3) +
      geom_segment(aes(xend = nombre, yend = 0), linewidth = 1.2) +
      geom_text(aes(label = miles(fortuna)), 
                angle = 40, hjust = 0, size = 3, nudge_x = 0.3, nudge_y = 600, vjust = 0) +
      scale_y_continuous(expand = expansion(c(0, 0.15)), labels = miles) +
      scale_x_discrete(expand = expansion(c(0.04, 0.05))) +
      theme_minimal() +
      coord_cartesian(clip = "off") +
      labs(y = "Fortuna (en millones de dólares)") +
      theme(axis.text.x = element_text(size = 10, angle = 40, hjust = 1, color = color_texto),
            axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5, color = color_detalle),
            axis.title.x = element_blank(),
            panel.grid = element_line(color = color_detalle),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            text = element_text(color = color_texto)
      ) 
  }, res = 95)
  
  
  ## dona millonarios ----
  
  
  output$grafico_dona_millonarios <- renderPlot({
    top_10_millonarios() |> 
      ggplot(aes(x = 1, y = fortuna, fill = nombre))+ 
      geom_bar(width = 0.2, stat = "identity",
               color = color_fondo, linewidth = 1)+
      coord_polar(theta = "y", direction = 1, start = 3, clip = "off") +
      xlim(c(0, 1.5)) +
      geom_text(aes(x = 1.15, y = fortuna, 
                    label = ifelse(nombre == "Iris Fontbona", as.character(nombre), "")),
                size = 2, hjust = 1, color = color_texto,
                position = position_stack(vjust = 0.5)) +
      ggrepel::geom_text_repel(aes(x = 1.15, y = fortuna, 
                                   label = ifelse(nombre != "Iris Fontbona", as.character(nombre), "")), 
                               size = 2, hjust = 0, color = color_texto,
                               box.padding = -30, seed = 2014, direction = "y", max.iter = 20000,
                               position = position_stack(vjust = 0.5)) +
      # scale_fill_brewer(palette = "GnBu")+
      scale_fill_manual(values = rev(degradado(length(top_10_millonarios()$nombre))), 
                        aesthetics = c("fill", "color")) +
      theme_void() +
      theme(legend.position = "none", 
            plot.margin = unit(c(-10, 0, -10, 0), "mm"))
  }, res = 125)
  
  ## circulo fortunas 1% ----
  farkas <- reactive(millonarios |> filter(nombre == "Leonardo Farkas")  |> pull(fortuna_pesos))
  
  output$grafico_circulo_fortuna <- renderPlot({
    
    proporcion_fortuna = 1 #fortuna/fortuna
    proporcion_farkas = farkas()/fortuna()
    proporcion_sueldo_fortuna = sueldo_vital()/fortuna()
    
    # browser()
    
    mostrar_sueldo = ifelse(
      proporcion_sueldo_fortuna > 0.00006, 
      TRUE, FALSE)
    
    mostrar_farkas = ifelse(
      (proporcion_farkas * 0.10) / (proporcion_fortuna * 0.01) > 0.9, FALSE, TRUE)
    # browser()
    
    p <- ggplot(data = NULL, aes(x0 = 1, y0 = 1)) +
      geom_circle(aes(r = proporcion_fortuna * 0.01), fill = color_claro, color = color_claro)
    
    if (mostrar_farkas) {
      p <- p + geom_circle(aes(r = proporcion_farkas * 0.10), fill = color_detalle, color = color_detalle)
    }
    p <- p +
      geom_circle(aes(r = proporcion_sueldo_fortuna), fill = color_destacado, color = color_destacado) +
      #textos
      # annotate("segment", x = 0.990, y = 1.010, xend = 1.010, yend = 0.990) + #linea cruzada
      annotate("text", label = paste("1% de", input$millonario), x = 0.9935, y = 1.0066,  hjust = 0)
    
    if (mostrar_farkas) {
      if (mostrar_sueldo) {
      p <- p + annotate("text", label = "10% de Farkas", 
                        x = 0.9993 + (proporcion_farkas * 0.10), 
                        y = 1.0002 - (proporcion_farkas * 0.10), hjust = 0)
      } else {
        p <- p + annotate("text", label = "10% de Farkas", 
                          x = 0.9993 + (proporcion_farkas * 0.30), 
                          y = 1.0002 - (proporcion_farkas * 0.30), hjust = 0)
      }
    }
    
    if (mostrar_sueldo) {
    p <- p + 
      annotate("text", label = "Tu", x = 1.00004 + proporcion_sueldo_fortuna, y = 0.9996 - proporcion_sueldo_fortuna, hjust = 0)
    }
    p <- p +
      coord_fixed(clip = "off") +
      theme_void()
    p
  }, res = 80)
  
  
  
  ## barras chile vs millonario ----
  output$grafico_barras_50 <- renderPlot({
    # division <- fortuna()/ingresos_mitad_baja()
    
    tabla <- tibble("etiqueta" = c("La mitad de los ingresos\nde todos los chilenos (50% inferior)", "Mitad superior de ingresos\nde todos los chilenos (50%superior)", input$millonario),
                    "valor" = c(ingresos_mitad_baja(), ingresos_mitad_alta(), fortuna()))
    
    tabla |> 
      mutate(etiqueta = factor(etiqueta, tabla$etiqueta)) |> 
      ggplot(aes(valor, etiqueta, fill = etiqueta)) +
      geom_col(width = 0.5) +
      scale_x_continuous(expand = expansion(0)) +
      scale_fill_manual(values = c(color_detalle, color_detalle, color_destacado)) +
      theme_minimal() +
      theme(legend.position = "none", 
            axis.text.y = element_text(color = color_texto),
            panel.grid = element_blank(),
            axis.text.x = element_blank(), 
            axis.title = element_blank())
  }, res = 180)
}