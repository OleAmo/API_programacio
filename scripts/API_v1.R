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


frase <- paste('Dades del dia ',dia_v2,
               ', preses a les ', hora,
               '. La Temperatura és de ',temp,temp_units,
               ', el Vent es de ',wind,wind_units)
  
print(frase)

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

#  Creo una FUNCIÓ
#  Aquesta funció em retornarà frases.


analisis <- function(dia,temp,humitat,wind,num){
  
  t <- temp[as.numeric(num)]
  hum <- humitat[as.numeric(num)]
  w <- wind[as.numeric(num)]
  dia_v1 <- dia[as.numeric(num)]
  dia_v2 <- str_split_1(dia_v1, "T")[1]
  hora <- str_split_1(dia_v1, "T")[2]
  
  
  frase <- paste('Dades del dia ',dia_v2,
                 ', preses a les ', hora,
                 '. La Temperatura és de ',t,
                 ', la Humitat R és de ',hum,
                 ', el Vent es de ',w)
  
  
  return(print(frase))
  
}

analisis(dia,temp,humitat,wind,5)
analisis(dia,temp,humitat,wind,10)
