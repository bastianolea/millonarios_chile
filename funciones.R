
pesos <- function(x) {
  paste0("$", format(x, big.mark = ".", decimal.mark = ",", trim = T))
}

miles <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = T)
}



#para obtener el precio del dólar, realizamos un web scraping del sitio del Banco Central de Chile


obtener_dolar <- function(scrapear = FALSE) {
  
  if (scrapear == TRUE) {
    library(rvest)
    library(polite)
    
    banco <- session("https://si3.bcentral.cl/indicadoressiete/secure/IndicadoresDiarios.aspx")
    
    respuesta <- banco$response$status_code
    
    # si está disponible el sitio, scrapear
    if (respuesta == 200) {
      message("scrapeando...")
      Sys.sleep(0.2)
      
      dolar <- banco |> 
        read_html() |> 
        html_element("#lblValor1_3") |> 
        html_text() |> 
        stringr::str_replace(",", "\\.") |> 
        as.numeric()
      
    } else {
      dolar = 900
    }
  } else {
    dolar = 900
  }
  return(dolar)
}

# obtener_dolar()

css <- function(text) {
  tags$style(glue(text, .open = "{{", .close = "}}"))
}

#detectar ancho de ventana ----
js_ancho_ventana <- function() {
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '))
}

#deciles ----
encontrar_decil <- function(casen_porcentajes_ingreso = NULL, ingreso = 500000) {
  which(abs(casen_porcentajes_ingreso - ingreso) == min(abs(casen_porcentajes_ingreso - ingreso)))
}