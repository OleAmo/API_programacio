# install.packages("httr")
# install.packages("jsonlite")
library(sf)
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)


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


# --------- CREACIÓ DADES en f(x) DIES ----------
# ------------------------------------------------

#   -) Creo una funció que depengui del numero de dies
#   -) En funció del Nº Dies Calcularà:
#   -) més o menys valors de Max, Min = Temp, Humitat, Vent

#   -) Ex: si les dades de la API son de 5 dies = ha d'haver 5 max, min,...
#   -) Ex: si les dades de la API son de 47 dies = ha d'haver 47 max, min,...

#    la funcio CALCUL DE DADE (DADES, DIES) HA DE SABER QUANS DIES TTINC
#    EN FUNCIÓ D'AIXO FAREM DIFERNTS CALCULS


dades_create <- function(df,dia_Inici,dia_Final){
  
  df <- data.frame(df)
  
  dia_1 <- as.Date(dia_Inici)
  dia_2 <- as.Date(dia_Final)
  num <- as.integer(dia_2-dia_1)+1
  num
  
  max <- c()
  min <- c()
  a = 1
  b = a + 23
  
  max <- c(max,max(df[a:b,]))
  min <- c(min,min(df[a:b,]))
  
  
  if (dia_Inici==dia_Final){    # evito el error de processar Data Inicial = Data final
    max <- max
    min <- min
    
  } else {
    
      for (i in 1:(num-1)){
        a <- b + 1
        b <- b + 24
        max <- c(max,max(df[a:b,]))
        min <- c(min,min(df[a:b,]))
      }
    
  }
  
  return(list(
    max = max,
    min = min
  ))
  
}


# ----------- CREACIÓ DIES en f(x) DIES ----------
# ------------------------------------------------

#   -) Creo una funció que depengui del numero de dies
#   -) En funció de dies em crei un VECTOR del 1r a l'ultim dia



dies_create <- function(df,dia_Inici,dia_Final){
  
  if(dia_Inici == dia_Final){    # evito el error de processar DIA Inici = DIA Final
    dies <- c()
    dies <- c(dia_Inici)
    
  } else {
  
      df <- data.frame(df[1])
      
      
      dia_1 <- as.Date(dia_Inici)
      dia_2 <- as.Date(dia_Final)
      num <- as.integer(dia_2-dia_1)+1
      num
      
      dies <- c()
      a = 1
      b = a + 23
      
      dia_max <- max(df[a:b,]) 
      dia_max <- str_split_1(dia_max, "T")[1]
      
      dies <- c(dies,dia_max)
      
      
      for (i in 1:(num-1)){
        a <- b + 1
        b <- b + 24
        
        dia_max <- max(df[a:b,])
        dia_max <- str_split_1(dia_max, "T")[1]
        
        dies <- c(dies,dia_max)
      }

  }
  
  return(dies)
  
}


# ----------- CREACIÓ DATA FRAME en f(x) DIES ----------
# -------------------------------------------------------

#   -) Creo una funció que depengui del numero de dies
#   -) En funció de dies crearà un DATA FRAME
#   -) El data FRAME tindrà 7 files
#   -) DIES, Temp_max, Temp_min,...
#   -) I les columnes seran depenent del Num de dies

#   -) Dins seu té 2 FUNCIONS que ja gestionen les dades en f(x) dels dies:

#        -) dies_create
#        -) dades_create


DF_create <- function(dades,dia_Inici,dia_Final){
  
  df <- data.frame(dades)
  
  t <- df[2]
  hum <- df[3]
  w <- df[4]
  
  dies <- dies_create(df,dia_Inici,dia_Final)
  
  t_dades <- dades_create(t,dia_Inici,dia_Final)
  hum_dades <- dades_create(hum,dia_Inici,dia_Final)
  w_dades <- dades_create(w,dia_Inici,dia_Final)
  
  
  resultat <- data.frame(
    Dies = dies,
    T_max = t_dades$max,
    T_min = t_dades$min,
    Hum_max = hum_dades$max,
    Hum_min = hum_dades$min,
    Win_max = w_dades$max,
    Win_min = w_dades$min
    
  )
  
  
  return(resultat)
  
  
}



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
  
  dades_api_processed <- DF_create(dades_api$hourly,data_1,data_2)
  
  dades_api_processed_geom <- assign_Geom(dades_api_processed,long,lat)
  
  return(dades_api_processed_geom)
  
}


#  ------- OBTENCIÓ DADES DIFERENTS PUNTS ---------
#  -------------------------------------------------


#  NOMÉS amb LAT, LONG, DIA 1 i DIA 2
#  PUC TENIR DF amb GEOMETRIA

DF_FINAL_8_dies <- create_DF_GEOM(41.38,2.17,"2024-03-01","2024-03-08")
DF_FINAL_8_dies

DF_FINAL_1_dia <- create_DF_GEOM(41.38,2.17,"2024-03-01","2024-03-01")
DF_FINAL_1_dia


DF_BCN <- create_DF_GEOM(41.3927674,2.0577875,"2024-03-01","2024-03-28")
DF_GIRONA <- create_DF_GEOM(41.9803704,2.7774675,"2024-03-01","2024-03-28")
DF_LLEIDA <- create_DF_GEOM(41.6183991, 0.5787351,"2024-03-01","2024-03-28")
DF_TARRAGONA <- create_DF_GEOM(41.1258621, 1.1973837,"2024-03-01","2024-03-28")


# Ho GUARDO com a SHAPE


st_write(DF_BCN, "data/processed/BCN_v1.shp", delete_layer = TRUE)
st_write(DF_GIRONA, "data/processed/GIRONA_v1.shp", delete_layer = TRUE)
st_write(DF_LLEIDA, "data/processed/LLEIDA_v1.shp", delete_layer = TRUE)
st_write(DF_TARRAGONA, "data/processed/TARRAGONA_v1.shp", delete_layer = TRUE)


# ------------------------------------------
# ----- COMARQUES CATALUNYA PROJECTE -------
# ------------------------------------------

# OBJECIU:

# Calcular el centroide de cada comarca
# Calculaar per UN DIA les dades de TEMP, HUMITAT,...
# Visaulitzar-ho al QGIS

# DADES = SHAPE Comarques

comarques <- st_read("data/raw/comarques.shp")



# --- OBTENCIÓ DE LATITUTD LONGITUD ----
# --------------------------------------


#     -) NECESSITO OBTENCIÓ Lat - Long
#     -) En 4326 (Google Maps)
#     -) Ja que la API de OPEN METEO obté dades amb 4326


# VERSIÓ 01

Alt_Camp <- comarques %>%
  filter(NOMCOMAR=='Alt Camp') %>%
  mutate(
    centoride = st_centroid(geometry),
    CRS_WGS84 =  st_transform(centoride, 4326),
    Coords = st_coordinates(CRS_WGS84)
  )

Alt_Camp 

# VERSIÓ 02

Alt_Camp_2 <- comarques %>%
  filter(NOMCOMAR=='Alt Camp') %>%
  mutate(
    coords = st_centroid(geometry) %>%
      st_transform(4326) %>% 
      st_coordinates() ,
    lat = coords[,1],
    long = coords[,2]
  )

Alt_Camp_2


# VERSIÓ 03 = TOT CATALUNYA


comarques_xy <- comarques %>%
  mutate(
    coords = st_centroid(geometry) %>%
      st_transform(4326) %>% 
      st_coordinates() ,
    lat = coords[,1],
    long = coords[,2]
  )

comarques_xy



# ------------ CREAR SHAPE CAPITAL COMARQUES -----------
# -----------------------------------------------------

#     -) Crear de les 4 COMARQUES = 4 CENTROIDS
#     -) De cada CENTROID = Buscar al info de TEMP, HUMITAT,...
#     -) Tindre 4 SHAPES POINTS 
#     -) Els UNEIXO en un SOL SHAPE


create_coords <- function(data,nom){
  
  data_processed <- data %>%
    filter(NOMCOMAR==nom) %>%
    mutate(
      coords = st_centroid(geometry) %>%
        st_transform(4326) %>% 
        st_coordinates() ,
      lat = coords[,1],
      long = coords[,2]
    )
  
  long <- data_processed$long
  lat <- data_processed$lat
  
  return(list(
    long = long,
    lat =lat
  ))
  
}

BCNES <- create_coords(comarques,'Barcelonès')
GIRONES <- create_coords(comarques,'Gironès')
TARRAGONES <- create_coords(comarques,'Tarragonès')
SEGRIA <- create_coords(comarques,'Segrià')


DF_BCNES <- create_DF_GEOM(BCNES$long,BCNES$lat,"2024-03-01","2024-03-01")
DF_GIRONES <- create_DF_GEOM(GIRONES$long,GIRONES$lat,"2024-03-01","2024-03-01")
DF_SEGRIA <- create_DF_GEOM(SEGRIA$long, SEGRIA$lat,"2024-03-01","2024-03-01")
DF_TARRAGONA <- create_DF_GEOM(TARRAGONES$long, TARRAGONES$lat,"2024-03-01","2024-03-01")

# UNIR SHAPES: 

sf_total <- rbind(DF_BCNES,DF_GIRONES,DF_SEGRIA,DF_TARRAGONA)
st_write(sf_total, "data/processed/COMARQUES_CAPITALS.shp", delete_layer = TRUE)


# ------------ CREAR TOTES COMARQUES -----------
# ----------------------------------------------

#     -) Crear TOTS CENTROIDES COMERQUES
#     -) De cada CENTROID = Buscar al info de TEMP, HUMITAT,...
#     -) Tindre 4 SHAPES POINTS 
#     -) Els UNEIXO en un SOL SHAPE


BCNES <- create_coords(comarques,'Barcelonès')
DF_BCNES <- create_DF_GEOM(BCNES$long,BCNES$lat,"2024-03-01","2024-03-01")


create_shape <- function(df,data_inici,data_final){
  
  
}

create_shape(comarques,"2024-03-01","2024-03-01")

long <- length(comarques$CODICOMAR) 

llista <- c()

for(i in 1:long){
  
  nom_comarca <- comarques$NOMCOMAR[i]
  coord_comarca <- create_coords(comarques,nom_comarca)
  comarca_DF_GEOM <- create_DF_GEOM(BCNES$long,BCNES$lat,"2024-03-01","2024-03-01")
  comarca_DF_GEOM %>% mutate( nom_comarca = nom_comarca )
  sf_total <- rbind(sf_total, comarca_DF_GEOM)
}

class(sf_total)




#   He de trobar un forma de automatizar-ho
#   Ara puc calcular totes les LAT i LONG dels centroides de comarques
#   Podria caclular per un dia o més les temp i humitats de cada punt
















#  EXERCICI 99999 = DADES
#  -----------------

#   -) Buscar INFO de DIVERSES LOCALITZACIONS
#   -) Calcular de cada una si en una SETMANA la TºC puja o no
#   -) Ho assoccio a un poligon de MUNICIPI
#   -) Li donc escala de color
#   -) Ho represento


