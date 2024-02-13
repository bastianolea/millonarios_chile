#descargar datos de población de Chile desde el INE para obtener la población del 2024

#descargar proyecciones de población
download.file("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/ine_estimaciones-y-proyecciones-de-población-1992-2050_base-2017_base-de-datos.csv?sfvrsn=4022da86_11&download=true", 
              destfile = "datos/ine_estimaciones-y-proyecciones-de-población-1992-2050_base-2017_base-de-datos.csv")

#cargar proyecciones de población
poblacion <- read_csv2("datos/ine_estimaciones-y-proyecciones-de-población-1992-2050_base-2017_base-de-datos.csv", 
                      locale = locale(encoding = "latin1"))

#limpiar datos
poblacion_1 <- poblacion |> 
  janitor::row_to_names(1) |> 
  janitor::clean_names()
  
#seleccionar primer total, que corresponde a ambos géneros
poblacion_2 <- poblacion_1 |> 
filter(edad == "TOTAL") |> 
  slice(1)

poblacion_3 <- poblacion_2 |> 
  tidyr::pivot_longer(cols = starts_with("x"), names_to = "año", values_to = "poblacion") |> 
  select(año, poblacion)
  
poblacion_2024 = poblacion_3 |> filter(año == "x2024") |> pull(poblacion)

poblacion_2024
