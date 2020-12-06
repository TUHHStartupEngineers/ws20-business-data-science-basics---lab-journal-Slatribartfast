library("httr")
library("jsonlite")

res = GET("http://api.open-notify.org/astros.json")
data = fromJSON(rawToChar(res$content))
data