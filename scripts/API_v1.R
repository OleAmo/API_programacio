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




#  EXERCICI 01 = DADES
#  -----------------

#   -) Puc saver la MITJA x dia
#   -) Màxima i Minimna x dia
#   -) Quin dia a una mateixa Hora feina més o menys fred

#   -) Buscar relació de Humitat Relativa i Temp
#   -) Buscar relació de Temp i Vent



#  EXERCICI 02 = DADES
#  -----------------

#   -) Buscar INFO de DIVERSES LOCALITZACIONS
#   -) Calcular de cada una si en una SETMANA la TºC puja o no
#   -) Ho assoccio a un poligon de MUNICIPI
#   -) Li donc escala de color
#   -) Ho represento


