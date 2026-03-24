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

url <- "https://api.open-meteo.com/v1/forecast?latitude=41.38&longitude=2.17&current_weather=true"

res <- GET(url)

text <- content(res, "text")

dades <- fromJSON(text)

str(dades)
dades$current_weather$temperature
