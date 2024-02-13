
# individuo -----
# años que debiese trabajar una persona para obtener la fortuna
fortuna

sueldo = 1300000

fortuna/sueldo #sueldos

fortuna/(sueldo*12) #año

sueldo_dia = sueldo/20

fortuna/sueldo_dia #días laborales para obtener su fortuna
as.integer(fortuna/(sueldo_dia*365)) #años trabajando para obtener su fortuna



# equipo ----
# grupo de personas que debería trabajar x tiempo para obtener la fortuna
fortuna

sueldo = 1300000

equipo = 1

sueldo = sueldo*equipo

fortuna/sueldo #sueldos

fortuna/(sueldo*12) #año

sueldo_dia = sueldo/20

fortuna/sueldo_dia #días laborales para obtener su fortuna
as.integer(fortuna/(sueldo_dia*365)) #años trabajando para obtener su fortuna


# sueldos repartibles ----
fortuna/sueldo #personas que podrían ganar x sueldo con la fortuna


# vidas trabajadas ----
# https://cooperativa.cl/noticias/pais/poblacion/chile-alcanzo-un-nuevo-record-en-la-esperanza-de-vida-supero-los-81/2024-02-05/065352.html
# https://www.df.cl/economia-y-politica/actualidad/efecto-millennials-edad-promedio-de-los-ocupados-sube-a-44-anos
genero = "mujer"

esperanza_vida = ifelse(genero ==  "mujer", 83.5, 78.5)
edad_jubilacion = ifelse(genero ==  "mujer", 60, 65)
años_jubilacion = esperanza_vida - edad_jubilacion
edad_laboral_promedio = 24

años_laborales = esperanza_vida - edad_laboral_promedio - años_jubilacion

sueldo_anual = sueldo * 12

#sueldo exponencial: considerando que los primeros 10 años de la vida uno ganó menos
sueldo_primeros_años = sueldo_anual * seq(0.4, 1, by = 0.065)
sueldo_vida <- sum(sueldo_primeros_años) + (sueldo_anual * (años_laborales-10))
as.integer(fortuna/sueldo_vida)

#sueldo lineal: mismo sueldo toda la vida
sueldo_vida <- sueldo_anual * años_laborales
as.integer(fortuna/sueldo_vida)


exp(1:10)
seq(500, 1500, by = 4)
