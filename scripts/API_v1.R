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
