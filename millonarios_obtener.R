library(dplyr)
options(scipen = 9999)

# https://www.kaggle.com/datasets/prasertk/forbes-worlds-billionaires-list-2023?resource=download
# https://www.forbes.com/billionaires/

forbes <- readr::read_csv("forbes_2023_billionaires.csv")

dolar <- obtener_dolar(scrapear = TRUE)

forbes_chile <- forbes |> 
  filter(country == "Chile")

forbes_chile_2 <- forbes_chile |> 
  select(1:4, 7:9, 14, 5) |> 
  rename(nombre = personName) |>
  mutate(nombre = str_remove(nombre, " & family"),
         nombre = str_replace(nombre, "Sebastian", "Sebastián")) |> 
  mutate(fortuna_b_usd = finalWorth,
         fortuna_usd = finalWorth * 1000000,
         fortuna_pesos = fortuna_usd * dolar)

readr::write_csv2(forbes_chile_2, "forbes_2023_chile.csv")

fortuna <- forbes_chile_2 |> 
  filter(personName == "Sebastian Piñera & family") |> 
  pull(fortuna_pesos)


#3 más acá
# https://www.rankia.cl/blog/mejores-opiniones-chile/2190823-hombres-mas-ricos-chile


#farkas
# Pese a que es muy difícil de determinar, diferentes medios de comunicación estiman que la fortuna de Leonardo Farkas es de alrededor de 100 millones de dólares, es decir, unos 87 mil millones de pesos chilenos ($87.292.460.000).
# https://redgol.cl/tendencias/por-que-leonardo-farkas-es-millonario-y-cuanto-dinero-tiene-20231119-RDG-200890.html
# https://www.ciperchile.cl/2021/10/12/pandora-papers-las-fundaciones-privadas-de-leonardo-farkas-con-las-que-planeo-la-sucesion-de-su-patrimonio-en-panama/#:~:text=La%20cuantía%20real%20del%20patrimonio,millones)–%20es%20una%20incógnita.

# Falabella
# https://www.elmostrador.cl/mercados/sin-editar-mercado/2013/01/17/los-siete-multimillonarios-de-falabella-que-casi-nadie-conoce-fuera-de-chile/


# Kast
# https://www.biobiochile.cl/noticias/economia/negocios-y-empresas/2022/09/08/familia-kast-declara-us86-millones-en-el-extranjero-tras-reestructuracion-de-sus-negocios.shtml
# https://www.latercera.com/politica/noticia/las-operaciones-jose-antonio-kast-retirarse-los-negocios-familiares/813972/#:~:text=En%20ese%20documento%2C%20Kast%20declaró,controla%20el%2097%2C5%25.

#Luksic
# Grupo Luksic, el mayor conglomerado económico de Chile. Con una fortuna familiar de US$13 mil 500 millones 
# https://www.ciperchile.cl/2015/04/23/la-lista-completa-la-verdad-sobre-las-1-123-empresas-que-financian-la-politica-en-chile/

#Familias
# https://www.elconfidencial.com/alma-corazon-vida/2018-06-29/25-familias-mas-ricas-1-1-billones-de-dolares_1585353/#:~:text=%2DLa%20familia%20Hoffmann.&text=25%2C1%20miles%20de%20millones%20de%20dólares.