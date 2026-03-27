# install.packages("httr")
# install.packages("jsonlite")
library(sf)
library(httr)
library(jsonlite)
library(tidyverse)


#  ------- FUNCIÓ 01 = DADES API ---------
#  ---------------------------------------

#    -) OBTENCIÓ DADES DIFERENTS PUNTS

#    -) Venen de la API OPEN METEO
#    -) Les usaré per practicar

#    -) Creo FUNCIÓ que INTRODUINT LAT, LONG, DATA 1 i DATA 2
#    -) Ens dongui diferents DATA SETS


dades_API <- function(lat,long,date_1,date_2){
  
  res_2 <- GET(
    "https://archive-api.open-meteo.com/v1/archive",
    query = list(
      latitude = lat,
      longitude = long,
      start_date = date_1,
      end_date = date_2,
      hourly = "temperature_2m,relative_humidity_2m,windspeed_10m"
    )
  )  
  
  text_2 <- content(res_2, "text")
  dades_2 <- fromJSON(text_2)
  
  return(dades_2)

}


dades_2 <- dades_API(41.38,2.17,"2024-03-01","2024-03-03")

str(dades_2)


#  ------- FUNCIÓ 02 = CALCUL ESTADÍSTICS ---------
#  --------------  NOVA VERSIÓ  -------------------

#   -) Max, Min = Temp, Humitat, Vent
#   -) Ho calcula PER DIA


calcul_dades <- function(dades){
  max1 <- max(dades[1:24,])
  max2 <- max(dades[25:48,])
  max3 <- max(dades[49:72,])
  
  min1 <- min(dades[1:24,])
  min2 <- min(dades[25:48,])
  min3 <- min(dades[49:72,])
  
  
  return(list(
    max1 = max1,
    max2 = max2,
    max3 = max3,
    min1 = min1,
    min2 = min2,
    min3 = min3
  ))
}

#  ------- FUNCIÓ 03 = CALCUL DIES ---------
#  -----------------------------------------

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


#  ------- FUNCIÓ 04 = CADADES a DATAFRAME ---------
#  -------------------------------------------------


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


#  ------- FUNCIÓ 05 = ASSIGNAR GEOMETRIA ---------
#  -------------------------------------------------


#  CREO FUNCIÓ PER ASSIGNAR GEOMETRIAO
#  CREO Columna GEOMETRIA
#  Transformo el CRS a 25831

#  AFEGEIXO la GEOMETRIA 
#  Com que CADA FILA és el MATEIX PUNT
#  Cada fila TINDRÀ LA MATEIXA GEOMETRIA (el punt)

#  REPETIR GEOMETRIA:
#  FAIG funcio REP() = Repetir
#  nrow() = PER CADA FILA

#  I ho guardo a la carpeta PROCESSED com a SHAPE


assign_Geom <- function(dades,long,lat){
  
  geom <- st_sfc(st_point(c(long, lat)),crs = 4326) %>%
    st_transform(25831)
  
  df_meteo_geom <- st_sf(
    dades ,
    geometry = rep(geom, nrow(dades))
  )
  
  return(df_meteo_geom)
  
}


#  ------- FUNCIÓ 06 = AUTOMATIZACIÓ FINAL ---------
#  -------------------------------------------------


#  NOMÉS amb LAT, LONG, DIA 1 i DIA 2
#  TINC EL DF FINAL amb GOEMETRIA
#  PUC FER UNA FUNCIÓ QUE HO ENGLOBI



create_DF_GEOM <- function(lat,long,data_1,data_2){
  
  dades_api <- dades_API(lat,long,data_1,data_2)
  
  dades_api_processed <- create_DF(dades_api$hourly)
  
  dades_api_processed_geom <- assign_Geom(dades_api_processed,long,lat)
  
  return(dades_api_processed_geom)
  
}


#  ------- OBTENCIÓ DADES DIFERENTS PUNTS ---------
#  -------------------------------------------------


#  NOMÉS amb LAT, LONG, DIA 1 i DIA 2
#  PUC TENIR DF amb GEOMETRIA

DF_FINAL <- create_DF_GEOM(41.38,2.17,"2024-03-01","2024-03-03")
DF_FINAL


# BCN = 41.3927674  2.0577875
# GIRONA = 41.9803704  2.7774675
# LLEIDA = 41.6183991 0.5787351
# TARRAGONA =  41.1258621 1.1973837


DF_BCN <- create_DF_GEOM(41.3927674,2.0577875,"2024-03-01","2024-03-03")
DF_GIRONA <- create_DF_GEOM(41.9803704,2.7774675,"2024-03-01","2024-03-03")
DF_LLEIDA <- create_DF_GEOM(41.6183991, 0.5787351,"2024-03-01","2024-03-03")
DF_TARRAGONA <- create_DF_GEOM(41.1258621, 1.1973837,"2024-03-01","2024-03-03")


# Ho GUARDO com a SHAPE


st_write(DF_BCN, "data/processed/BCN_v1.shp", delete_layer = TRUE)
st_write(DF_GIRONA, "data/processed/GIRONA_v1.shp", delete_layer = TRUE)
st_write(DF_LLEIDA, "data/processed/LLEIDA_v1.shp", delete_layer = TRUE)
st_write(DF_TARRAGONA, "data/processed/TARRAGONA_v1.shp", delete_layer = TRUE)




#  ------- NO SE COM FER HO ---------
#  ----------------------------------

#   -) Max, Min = Temp, Humitat, Vent
#   -) Ho calcula PER DIA

#   -) Vull implementar que en funció del numero de dies
#   -) Calcularà més o menys valors

#   -) Ex: si les dades de la API son de 5 dies = ha d'haver 5 max, min,...
#   -) Ex: si les dades de la API son de 47 dies = ha d'haver 47 max, min,...

#    la funcio CALCUL DE DADE (DADES, DIES) HA DE SABER QUANS DIES TTINC
#    EN FUNCIÓ D'AIXO FAREM DIFERNTS CALCULS


dades <- dades_2$hourly$temperature_2m

dia_1 <- as.Date("2024-03-01")
dia_2 <- as.Date("2024-03-03")
num <- as.integer(dia_2-dia_1)+1
num

long <- length(dades)

long/num









#  EXERCICI 99999 = DADES
#  -----------------

#   -) Buscar INFO de DIVERSES LOCALITZACIONS
#   -) Calcular de cada una si en una SETMANA la TºC puja o no
#   -) Ho assoccio a un poligon de MUNICIPI
#   -) Li donc escala de color
#   -) Ho represento


