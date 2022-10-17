library(ggplot2)
library(httr)
library(jsonlite)
library("writexl")

# This function gets the whole data from the API ENDPOINT
  #res = GET("https://shopee.sg/api/v4/shop/get_shop_detail?sort_sold_out=0", query = list(shopid = shop_id))
res = GET("https://www.klook.com/v4/hotelapiserv/hotelapi/similar?hotel_id=135423&check_in=2022-10-14&check_out=2022-10-15&adult_num=2&room_num=1&child_num=0&age=")
data <- fromJSON(rawToChar(res$content))
jsonData <- data$data
