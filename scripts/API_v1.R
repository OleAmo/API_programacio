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

#   L'API OPEN METO permet OBTENIR MÉS DADES
#   Puc OBTENIR
#     Per un intervar de dies
#     Les dades de cada hora



res <- GET(
  "https://api.open-meteo.com/v1/forecast",
  query = list(
    latitude = 41.38,
    longitude = 2.17,
    start_date = "2024-03-01",
    end_date = "2024-03-01",
    hourly = "temperature_2m"
  )
)

text <- content(res, "text")
dades <- fromJSON(text)
