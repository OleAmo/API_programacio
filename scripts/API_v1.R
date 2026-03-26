# install.packages("httr")
# install.packages("jsonlite")

library(httr)
library(jsonlite)
library(tidyverse)


#  ---- CONCEPTES BASICS ------
#  ----------------------------

#   GET() → fa la petició
#   content() → treu el text
#   fromJSON() → converteix a R
#   str() → veure estructura

#   GET =
#     -) R envia una petició HTTP a l’API
#     -) L’API respon amb dades (normalment JSON)

url <- "https://api.open-meteo.com/v1/forecast"

res <- GET(url, query = list(
  latitude = 41.38,
  longitude = 2.17,
  current_weather = TRUE
))


#   CONTENT =
#     -) agafa el cos de la resposta
#     -) el converteix a text (string)

text <- content(res, "text")

#   fromJSON =
#     -) transforma JSON → objectes de R
#     -) llistes (list)
#     -) dataframes (si és possible)

dades <- fromJSON(text)

#   ESTRUCTURA DADES
#   Em diu com estan estructurades:

str(dades)

#   Puc TROBAR una dada concreta:
#   Exemple = LA TEMPERATURA

temp <- dades$current_weather$temperature
wind <- dades$current_weather$windspeed
dia_v1 <- dades$current_weather$time
dia_v2 <- str_split_1(dia_v1, "T")[1]
hora <- str_split_1(dia_v1, "T")[2]
  
temp_units <- dades$current_weather_units$temperature
wind_units <- dades$current_weather_units$windspeed


#  SALT DE LINIES en uns text
#  s'usa:
#     -)  paste0()  = la funció
#     -)  \n        = per indicar els SALT de LÍNIA
#     -)  cat()     = és com la funcio print


frase <- paste0('DIA = ',dia_v2,'\n',
               'Hora = (', hora,') \n',
               'Temperatura = ',temp,temp_units,'\n',
               'Velocitat del Vent = ',wind,wind_units,'\n')
  
cat(frase)

#  ---- OBTENIR UN SEGUIT DE DADES ------
#  --------------------------------------

#   L'API OPEN METO té una VERSIÓ amb DADES HISTÒRIQUES
#   La URL és diferent que l'exercici anterior

#     Ara puc demanar un INTERVAL de DIES
#     Del 1 de Març al 3 de Març
#     Li demano Temperatura, Humitat i Vent



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

#  EXPLORO les DADES
#  -----------------

#  Començo amb les TEMPERATURES
#  Veig que hi ha 72 Temp, 72 humitats I 72 vents
#  3 dies = 72 hores
#  Pertant tinc per cada hora les 3 variables



dia <- dades_2$hourly$time
temp <- dades_2$hourly$temperature_2m
humitat <- dades_2$hourly$relative_humidity_2m
wind <- dades_2$hourly$windspeed_10m

length(temp)



#  FUNCIÓ 01
#  ----------

#  Aquesta funció em retornarà dades
#  Les retornarè en format frase


analisis <- function(dia,temp,humitat,wind,num){
  
  t <- temp[as.numeric(num)]
  hum <- humitat[as.numeric(num)]
  w <- wind[as.numeric(num)]
  dia_v1 <- dia[as.numeric(num)]
  dia_v2 <- str_split_1(dia_v1, "T")[1]
  hora <- str_split_1(dia_v1, "T")[2]
  
  
  frase <- paste0('DIA = ',dia_v2,'\n',
                  'Hora = (', hora,') \n',
                  'Temperatura = ',t,'\n',
                  'Humitat Relativa = ',hum,'\n',
                  'Velocitat del Vent = ',w,'\n')
  

  
  return(cat(frase))
  
}

analisis(dia,temp,humitat,wind,5)
analisis(dia,temp,humitat,wind,10)


#  FUNCIÓ 02
#  ----------

#  Aquesta funció em retornarà dades
#  Les retornarè en format frase



analisis_v2 <- function(dades,num){
  
  #  Primer transormo les dades amb DATA FRAME
  #  Així despres puc accedir a les dades individuals
  
  #  eXERMPLE= df[2][23,]
  
  #      -) El [2] = son TEMPERATURES
  #      -) El [23,] = és el dia 23
  
  df <- data.frame(dades)
  
  t <- df[2][as.numeric(num),]
  hum <- df[3][as.numeric(num),]
  w <- df[4][as.numeric(num),]
  dia_v1 <- df[1][as.numeric(num),]
  dia_v2 <- str_split_1(dia_v1, "T")[1]
  hora <- str_split_1(dia_v1, "T")[2]
  
  
  frase <- paste0('DIA = ',dia_v2,'\n',
                  'Hora = (', hora,') \n',
                  'Temperatura = ',t,'\n',
                  'Humitat Relativa = ',hum,'\n',
                  'Velocitat del Vent = ',w,'\n')
  
  
  
  return(cat(frase))
  
}


analisis_v2(dades_2$hourly,5)
analisis_v2(dades_2$hourly,10)




#  FUNCIO 03 = DADES MAX i MIN
#  ----------------------------

#   -) Puc saver la MITJA ALS 3 DIES
#   -) Màxima i Minimna ALS 3 DIES


analisis_AVERAGES <- function(dades){
  
  #  Creo una funció 
  #  Per cada punt en retorna un vector
  #  Amb la Mitja Temp, Mitja Humitat, Mitja Vent
  #  També Max Temp, Min Temp,...Max Humitat,....
  
  # Aixi després em serà fàcil de usar per extreure dades de molts punts
  # Tindré molts vectors i em serà facil de treballar amb territoris
  
  df <- data.frame(dades)
  
  t <- df[2]
  hum <- df[3]
  w <- df[4]
  
  dia_v1 <- df[1]
  dia_max <- dia_v1[length(dia_v1[,1]),]
  dia_min <- dia_v1[1,]
  
  dia_max_f <- str_split_1(dia_max, "T")[1]
  dia_min_f <- str_split_1(dia_min, "T")[1]
  
  max_t <- max(t)
  min_t <- min(t)
  
  max_hum <- max(hum)
  min_hum <- min(hum)
  
  max_w <- max(w)
  min_w <- min(w)
  
  
  frase <- paste0('PERÍODE  = (',dia_min_f,' / ',dia_max_f,') \n',
                  'Max Temp = ', max_t,'\n',
                  'Min Temp = ', min_t,'\n',
                  '\n',
                  'Max Humitat = ', max_hum,'\n',
                  'Min Humitat = ', min_hum,'\n',
                  '\n',
                  'Max Vent = ', max_w,'\n',
                  'Min Vent = ', min_w,'\n'
                  )
  
  
  
  return(cat(frase))
  
}

analisis_AVERAGES(dades_2$hourly)


#  FUNCIO 03 = DADES MAX i MIN - EN 3 DIES
#  ---------------------------------------

#   -) Puc saver la Màxima i Minimna x dia
#   -) Dades a saber = ( Temp, Humitat i Vent)


analisis_AVERAGES_DAY <- function(dades){
  
  df <- data.frame(dades)
  
  t <- df[2]
  hum <- df[3]
  w <- df[4]
  
  dia_v1 <- df[1]
  dia_max <- dia_v1[length(dia_v1[,1]),]
  dia_min <- dia_v1[1,]
  
  dia_max_f <- str_split_1(dia_max, "T")[1]
  dia_min_f <- str_split_1(dia_min, "T")[1]
  
  max_t1 <- max(t[1:24,])
  max_t2 <- max(t[25:48,])
  max_t3 <- max(t[49:72,])
  
  min_t1 <- min(t[1:24,])
  min_t2 <- min(t[25:48,])
  min_t3 <- min(t[49:72,])
  
  max_hum1 <- max(hum[1:24,])
  max_hum2 <- max(hum[25:48,])
  max_hum3 <- max(hum[49:72,])
  
  min_hum1 <- min(hum[1:24,])
  min_hum2 <- min(hum[25:48,])
  min_hum3 <- min(hum[49:72,])
  
  max_hum <- max(hum)
  min_hum <- min(hum)
  
  max_w <- max(w)
  min_w <- min(w)
  
  
  frase <- paste0('PERÍODE  = (',dia_min_f,' / ',dia_max_f,') \n',
                  '\n',
                  'Max Temp = ', max_t1,' - ',max_t2,' - ',max_t3,'\n',
                  'Min Temp = ', min_t1,' - ',min_t2,' - ',min_t3,'\n',
                  '\n',
                  'Max Humitat = ', max_hum1,' - ',max_hum2,' - ',max_hum3,'\n',
                  'Min Humitat = ', min_hum1,' - ',min_hum2,' - ',min_hum3,'\n',
                  '\n',
                  'Max Vent = ', max_w,'\n',
                  'Min Vent = ', min_w,'\n'
  )
  
  
  
  return(cat(frase))
  
}

analisis_AVERAGES_DAY(dades_2$hourly)



#  FUNCIO 03 = DADES MAX i MIN - EN 3 DIES ( més eficiente)
#  ---------------------------------------

#   -) Puc saver la Màxima i Minimna x dia
#   -) Dades a saber = ( Temp, Humitat i Vent)

#   -) FER MES EFICIENT LO DELS 3 DIES 
#   -) Creo funcions diferents. Unes criden a altres


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

calcul_dies <- function(df){
  
  dia_v1 <- df[1]
  dia_max <- dia_v1[length(dia_v1[,1]),]
  dia_min <- dia_v1[1,]
  
  dia_max_f <- str_split_1(dia_max, "T")[1]
  dia_min_f <- str_split_1(dia_min, "T")[1]
  
  text <- paste0(dia_min_f,' / ',dia_max_f)
  
  return(list(
    dia_max = dia_max_f,
    dia_min = dia_min_f,
    text = text
    
  ))
  
}

analisis_AVERAGES_DAY_v2 <- function(dades){
  
  df <- data.frame(dades)
  
  t <- df[2]
  hum <- df[3]
  w <- df[4]
  
  dia <- calcul_dies(df)

  t_dades <- calcul_dades(t)
  hum_dades <- calcul_dades(hum)
  w_dades <- calcul_dades(w)
  
  frase <- paste0('PERÍODE  = (',dia$text,') \n',
                  '\n',
                  'Max Temp = ',t_dades$text_max,'\n',
                  'Min Temp = ',t_dades$text_min,'\n',
                  '\n',
                  'Max Humitat = ',hum_dades$text_max,'\n',
                  'Min Humitat = ',hum_dades$text_min,'\n',
                  '\n',
                  'Max Vent = ', w_dades$text_max,'\n',
                  'Min Vent = ', w_dades$text_min,'\n'
  )
  
  
  return(cat(frase))
  
  
}

analisis_AVERAGES_DAY_v2(dades_2$hourly)


