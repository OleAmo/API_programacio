# install.packages("httr")
# install.packages("jsonlite")

library(httr)
library(jsonlite)

url <- "https://api.coindesk.com/v1/bpi/currentprice.json"

resposta <- GET(url)
text <- content(resposta, "text")




