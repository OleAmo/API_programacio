# install.packages("httr")
# install.packages("jsonlite")

library(httr)
library(jsonlite)


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
dia <- dades$current_weather$time

temp_units <- dades$current_weather_units$temperature
wind_units <- dades$current_weather_units$windspeed
dia_units <- dades$current_weather_units$time

str_split(dia_units, "T")

frase <- paste('La Temperatura és de ',temp,temp_units,
               ', el vent es de ',wind,wind_units,
               'en el dia ',dia)
  
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
