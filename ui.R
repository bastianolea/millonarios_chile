library(shiny)

fluidPage(
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
      font-size: 80%; opacity: 0.65; line-height: 1.3;
      }"),
  
  css("h2, h3 {
      font-style: italic;
  }"),
  
  css("h5 {
      font-weight: bold;
  }"),
  
  fluidRow(
    #título ----
    div(style = "padding-top: 12px; padding-bottom: 20px;",
        
        titlePanel(
          h1("Millonarios de Chile", 
             style = glue("font-weight: bold; color: {color_destacado}")),
          windowTitle = "Millonarios de Chile"),
        
        div(style = "opacity: 0.6;",
        p("Aplicación interactiva sobre las fortunas de los empresarios más ricos de Chile", 
          style = "margin-bottom: 8px; font-size: 80%;"),
        em(tags$a("Bastián Olea Herrera", 
                  href = "http://bastian.olea.biz",
                  target = "_blank"))
        ),
        
        div(style = "padding-top: 18px;",
            p("En toda economía de mercado existen personajes que acaparan vastas riquezas, ya sea por el éxito de sus negocios, por poseer recursos clave, haber recibido herencias o ser sucesores de otros magnates, o bien, por haber ejercido estrategias", em("cuestionables"), "para el enriquecimiento propio."),
            p("Con este visualizador puedes poner en perspectiva sus fortunas para así dimensionar un aspecto clave de la desigualdad en Chile y el mundo."),
        ),
        
        # hr()
    )
  ),
  
  # gráfico millonarios ----
  fluidRow(
    column(12,
           h2("Ranking de los empresarios más ricos de Chile"),
           p("¿Quiénes son las personas más ricas de Chile? En este gráfico podemos ver 
             a los principales millonarios del país, ordenados por su patrimonio estimado."),
           
           plotOutput("grafico_millonarios") |> withSpinner(),
           hr(),
    )
  ),
  
  ## top fortunas ----
  fluidRow(
    column(12,
           h2("Las riquezas de los 10 empresarios más grandes de Chile"),
           p("Si sumamos las fortunas de los 10 mayores empresarios del país, obtendríamos una suma de dinero que es aproximadamente",
             textOutput("top_fortunas_vs_chile", inline = T) |> cifra(), 
             cifra("veces mayor"), "que todos los ingresos percibidos por toda la población chilena en un mes."),
           p("En otras palabras, si el próximo mes todos los trabajadores de Chile depositaran sus sueldos a una misma cuenta,
             apenas alcanzaríamos el",
             textOutput("top_fortunas_vs_chile_porcentaje", inline = T) |> cifra(),
             "de la suma de las fortunas de las 10 personas más ricas del país.")
    ),
    column(12, align = "center", 
           #este gráfico tiene scroll horizontal si la pantalla es muy chica, pero sin agrandar el ancho del sitio
           div(style = "min-width: 300px; overflow-x: scroll;",
               plotOutput("grafico_dona_millonarios", width="500px") |> withSpinner()
           )
    )
  ),
  
  #la persona más rica ----
  fluidRow(
    column(12,
           p("La persona más rica de Chile es, a la fecha, es",
             textOutput("mayor_millonario_nombre", inline = T),
             "de quien se estima un patrimonio avaluado en aproximadamente",
             textOutput("mayor_millonario_fortuna", inline = T), "millones de dólares."),
           p("Esta cifra estratosférica equivale, aproximadamente, a la construcción de",
             cifra(textOutput("mayor_millonario_hospitales", inline = T)), cifra("hospitales públicos,"),
             textOutput("mayor_millonario_cesfam", inline = T), "Centros de Salud Familiar (Cesfam),",
             "o", textOutput("mayor_millonario_casas", inline = T), "viviendas promedio."),
           p("Si esta persona enloqueciera, podría liquidar su fortuna y entregar a todos y cada uno de los chilenos un bono de 1 millón de pesos, 
             y aún así, le quedarían", textOutput("bono_iris_sobrantes", inline = T), "millones de dólares a su favor."),
           
           hr()
    )
  ),
  
  
  
  # millonario ----
  ## input millonario ----
  fluidRow(
    column(4, align = "right",
           h5("Selecciona un millonario"),
           pickerInput("millonario", 
                       NULL, selected = "Sebastián Piñera Echeñique",
                       choices = millonarios$nombre, 
           )
    ),
    
    column(8,
           ### datos millonario ----
           h2(textOutput("nombre_millonario7")),
           
           p(textOutput("nombre_millonario8", inline = T),
             "es", textOutput("millonario_genero", inline = T), 
             "que ha participado en empresas de los sectores", textOutput("millonario_categorias", inline = T)
           ),
           
           htmlOutput("millonario_rank_forbes"),
           
           
           
           
           ### fortuna millonario ----
           
           h3("Contextualizando su fortuna"),
           
           p("La fortuna de", textOutput("nombre_millonario6", inline = T), "corresponde a",
             cifra(textOutput("fortuna_usd", inline = T)), cifra("millones de dólares,"), "lo que es equivalente a",
             cifra(textOutput("fortuna", inline = T)), "en pesos chilenos."),
           
           ## comparar ----
           p("Un 10% de esta fortuna equivaldría a",
             cifra(textOutput("fortuna_comparar_teleton", inline = T)),
             "veces el monto recaudado en la", cifra("Teletón 2023,"),
             "o bien,",
             cifra(textOutput("fortuna_comparar_chileselevanta", inline = T)),
             "veces el dinero recaudado en", cifra("“Juntos, Chile se Levanta”"),
             "para", 
             tags$a("ayudar a los damnificados", href = "https://www.adnradio.cl/2024/02/17/juntos-chile-se-levanta-logra-millonaria-recaudacion-este-fue-el-monto-recaudado-en-el-evento-solidario-por-los-damnificados-en-los-incendios/", target = "_blank"), 
             "por los incendios en la región de Valparaíso."
           ),
           
           ## repartir chile ----
           p("Si esta persona decidiera repartir su riqueza entre las",
             tags$a("20 millones de personas que habitan Chile al 2024,",
                    href = "https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion", target = "_blank"),
             cifra("podría regalar aprox."), cifra(textOutput("bono_millonario", inline = T)), cifra("pesos a cada chileno/a."),
             "O también podría pagar un sueldo de quinientos mil pesos al", textOutput("sueldos_repartibles", inline = T) |> cifra(),
             "de la población del país."),
           
           
           
           ## 50% de chile ----
           p("Si", strong("sumamos"), "todos los ingresos de", strong("la primera mitad de todos los chilenos;"), "es decir,
             todo el dinero que ganan los aprox. 10 millones de trabajadoras y trabajadores chilenos que ganan sueldos",
             tags$a("iguales o menores a 500 mil pesos (la mediana de ingresos),", href = "https://fundacionsol.cl/blog/actualidad-13/post/los-verdaderos-sueldos-de-chile-2023-7416", target = "_blank"),
             "y comparamos todo ese dinero a la fortuna de", textOutput("nombre_millonario_coma", inline = T),
             "entonces su fortuna sería un", textOutput("fortuna_porcentaje_mitad_chile", inline = T) |> cifra(),
             "más alta que lo que ganaría la mitad de los trabajadores del país en un mes."),
           
           plotOutput("grafico_barras_50", height = 180),
           
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
           
           div(plotOutput("grafico_decil", height = 70),
               style = "margin-bottom: 24px;"),
           
           p("Sin embargo, la fortuna de", textOutput("nombre_millonario3", inline = T), "es equivalente a", 
             cifra(textOutput("equivalencia_en_sueldos", inline = T)), cifra("de sueldos tuyos")),
           
           hr()
    )
  ),
  
  # cuantos tendrías que trabajar ----
  fluidRow(
    column(4, align = "right",
           ## inputs genero y edad ----
           div(style = "margin-bottom: 18px; line-height: 1.3;",
               h5("Ingresa tus datos",
                  style = "margin-bottom: 0px;"),
               em("de género y edad en que empezaste a trabajar para ajustar las estimaciones")
           ),
           radioGroupButtons("genero", 
                             NULL, 
                             choices = c("Femenino", "Masculino"), 
                             size = "sm",
           ),
           
           numericInput("edad_laboral", 
                        NULL,
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
           p("Considerando que la",
             tags$a("esperanza de vida", 
                    href = "https://cooperativa.cl/noticias/pais/poblacion/chile-alcanzo-un-nuevo-record-en-la-esperanza-de-vida-supero-los-81/2024-02-05/065352.html", target = "_blank"),
             "de una persona de género", 
             textOutput("genero", inline = T),
             "es actualmente de", textOutput("esperanza_vida", inline = T), "años,
           y que su edad de jubilación está fijada en los", textOutput("edad_jubilacion", inline = T), "años, entonces", 
           cifra("necesitarías trabajar"), cifra(textOutput("vidas_trabajadas", inline = T)),
           cifra("vidas enteras"), "sin gastar un solo peso para alcanzar la fortuna de", textOutput("nombre_millonario_punto2", inline = T),
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
                    )
             ),
             column(12, align = "center", 
                    #este gráfico tiene scroll horizontal si la pantalla es muy chica, pero sin agrandar el ancho del sitio
                    div(style = "min-width: 300px; overflow-x: scroll;",
                        plotOutput("grafico_circulo_fortuna", width = "400px") |> withSpinner()
                    )
             )
           )
    )
  ),
  
  # conclusión ----
  
  fluidRow(
    column(12,
           hr(),
           h2("Conclusiones"),
           p("A trabajar, para que algún día seas millonari@ 🥲"),
           hr(),
    )
  ),
  
  
  
  
  
  ## firma ----
  fluidRow(
    column(12, style = "opacity: 0.5; font-size: 80%;",
           p("Diseñado y programado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           p("Puedes explorar mis otras",
             tags$a("aplicaciones interactivas sobre datos sociales aquí.",
                    href = "https://bastianolea.github.io/shiny_apps/", target = "_blank")
           ),
           p("Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/millonarios_chile")
           ),
           
           div(style = "height: 40px")
           
    )
  )
  
)