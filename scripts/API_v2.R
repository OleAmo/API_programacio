# install.packages("httr")
# install.packages("jsonlite")
library(sf)
library(httr)
library(jsonlite)
library(tidyverse)


#  --- OBTENCIÓ DADES -----
#  ------------------------

#    -) Venen de la API OPEN METEO
#    -) Les usaré per practicar


res_2 <- GET(
  "https://archive-api.open-meteo.com/v1/archive",
  query = list(
    latitude = 41.38,
    longitude = 2.17,
    start_date = "2024-03-01",
    end_date = "2024-03-03",
    hourly = "temperature_2m,relative_humidity_2m,windspeed_10m"
  )
)  

text_2 <- content(res_2, "text")
dades_2 <- fromJSON(text_2)

str(dades_2)


# --- FUNCIONS BÀSIQUES ----
# --------------------------

#   CALCUL ESTADÍSTICS

#   -) Max, Min = Temp, Humitat, Vent
#   -) Ho calcula PER DIA


calcul_dades <- function(dades){
  max1 <- max(dades[1:24,])
  max2 <- max(dades[25:48,])
  max3 <- max(dades[49:72,])
  
  min1 <- min(dades[1:24,])
  min2 <- min(dades[25:48,])
  min3 <- min(dades[49:72,])
  
  text_max <- paste0(max1,' - ',max2,' - ',max3)
  text_min <- paste0(min1,' - ',min2,' - ',min3)
  
  return(list(
    max1 = max1,
    max2 = max2,
    max3 = max3,
    min1 = min1,
    min2 = min2,
    min3 = min3,
    text_max = text_max,
    text_min = text_min
  ))
}

#   CALCUL DIES

#   -) Calcula el DIA màxim
#   -) Calcula el DIA mínim


calcul_dies <- function(df){
  

  dia_v1 <- data.frame(df[1])
  
  dia1 <- dia_v1[1,]
  dia2 <- dia_v1[25,]
  dia3 <- dia_v1[49,]
  
  dia1_f <- str_split_1(dia1, "T")[1]
  dia2_f <- str_split_1(dia2, "T")[1]
  dia3_f <- str_split_1(dia3, "T")[1]
  

  return(list(
    dia_1 = dia1_f,
    dia_2 = dia2_f,
    dia_3 = dia3_f
    
  ))
  
}




#  FUNCIO 01 = DADES a DATAFRAME
#  ----------------------------

#   -) Puc saver la Màxima i Minimna x dia
#   -) Dades a sabeR (Temp, Humitat, Vent, Dia, Latitud)

#   -) Creo un DATA FRAME amb la info
#   -) La info la associo a PUNTS (x,y)
#   -) La podré representar després a QGIS



create_DF <- function(dades){
  
  df <- data.frame(dades)
  
  t <- df[2]
  hum <- df[3]
  w <- df[4]
  
  dia <- calcul_dies(df)
  t_dades <- calcul_dades(t)
  hum_dades <- calcul_dades(hum)
  w_dades <- calcul_dades(w)
  
 
  resultat <- data.frame(
    Dies = c(dia$dia_1,dia$dia_2,dia$dia_3),
    T_max = c(t_dades$max1,t_dades$max2,t_dades$max3),
    T_min = c(t_dades$min1,t_dades$min2,t_dades$min3),
    Hum_max = c(hum_dades$max1,hum_dades$max2,hum_dades$max3),
    Hum_min = c(hum_dades$min1,hum_dades$min2,hum_dades$min3),
    Win_max = c(w_dades$max1,w_dades$max2,w_dades$max3),
    Win_min = c(w_dades$min1,w_dades$min2,w_dades$min3)
    
  )
  
  
  return(resultat)
  
  
}


create_DF(dades_2$hourly)


# --- GEOMETRIA -----------
# -------------------------



punt <- st_sfc(st_point(c(2.17, 41.38)), crs = 4326)
punt_sf <- st_sf(nom = "Barcelona", geometry = punt)
st_write(punt_sf, "data/processed/punt.shp")




#  EXERCICI 99999 = DADES
#  -----------------

#   -) Buscar INFO de DIVERSES LOCALITZACIONS
#   -) Calcular de cada una si en una SETMANA la TºC puja o no
#   -) Ho assoccio a un poligon de MUNICIPI
#   -) Li donc escala de color
#   -) Ho represento


