# install.packages("httr")
# install.packages("jsonlite")

library(httr)
library(jsonlite)

url <- "https://api.open-meteo.com/v1/forecast"

resposta <- GET(url, query = list(
  latitude = 41.38,
  longitude = 2.17,
  current_weather = TRUE
))

text <- content(resposta, "text")
dades <- fromJSON(text)

dades$current_weather
dades$elevation


#  ---- CURS D'US D'API -----
#  --------------------------

#   CONCEPTES BASICS
#   ----------------

#   GET() → fa la petició
#   content() → treu el text
#   fromJSON() → converteix a R
#   str() → veure estructura

#   GET =
#     -) R envia una petició HTTP a l’API
#     -) L’API respon amb dades (normalment JSON)

url <- "https://api.open-meteo.com/v1/forecast?latitude=41.38&longitude=2.17&current_weather=true"
res <- GET(url)

#   CONTENT =
#     -) agafa el cos de la resposta
#     -) el converteix a text (string)

text <- content(res, "text")

#   fromJSON =
#     -) transforma JSON → objectes de R
#     -) llistes (list)
#     -) dataframes (si és possible)

dades <- fromJSON(text)

str(dades)
dades$current_weather$temperature
