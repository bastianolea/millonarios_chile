#para obtener el precio del dólar, realizamos un web scraping del sitio del Banco Central de Chile

library(rvest)
library(polite)

obtener_dolar <- function(scrapear = FALSE) {
  
  if (scrapear == TRUE) {
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
