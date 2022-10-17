library(ggplot2)
library(httr)
library(jsonlite)
library(stringr)
library("writexl")


#NOTE: Refer to this documentation https://rdrr.io/cran/httr/man/add_headers.html for instructions on adding headers

# Plan: 
#   1. Obtain list of Hotel IDs. 
#   2. Obtain business requirement on list of check-out check-in period
#   3. Obtain list of data which is possible to be scraped and decide what is needed.
# 
# Action plan:
#   1. We need to perform web crawling to obtain the list of hotels, crawl by looking through all the "Nearby hotel" keep only the unique ones.
#   2. For each id we have, we will query each until we have a long list of hotel ids. 
#   3. To test the endpoint in postman, use copy all as CURL (cmd) < important! and import it over. Monitor what is needed the headers and add it in our request here.
#   4. We will be using hotelSearchNeaby to find IDs




# Trip.com scraping the HotelSeachNearby api
apiCallNearbyHotel <- function(masterHotelId){
  res = POST(
    "https://sg.trip.com/restapi/soa2/16708/json/hotelSearchNearby?x-traceID=1658849562341.1pmnm5-1666016479066-1666203502",
    add_headers('authority = sg.trip.com'),
    add_headers('accept = application/json'),
    add_headers('accept-language = en-US,en;q=0.9'),
    add_headers('content-type = application/json;charset=UTF-8'),
    add_headers('cookie = ibu_home_language_match_union_flag=0; _abtest_userid=fb70a796-d789-4b0b-80d8-afdd54b85933; _gcl_au=1.1.562621262.1658849563; _RGUID=751c843f-5682-48a5-a9ff-1e84de692fb8; _RDG=281a5c31eaa24722803983fe33c78ee19f; _RSG=bYaWMz3XSK7u.IQUJe7_dA; _bfaStatusPVSend=1; ibu_online_home_language_match={"isFromTWNotZh" =false,"isFromIPRedirect" =false,"isFromLastVisited" =true,"isRedirect" =false,"isShowSuggestion" =false,"lastVisited" ="https =//sg.trip.com?locale=en-sg"}; cookiePricesDisplayed=SGD; _tp_search_latest_channel_name=hotels; _gid=GA1.2.1760276045.1663081390; _RF1=137.132.215.24; g_state={"i_p" =1663088594680,"i_l" =1}; ibu_h5_site=SG; ibu_h5_group=trip; ibu_h5_local=en-sg; ibu_h5_lang=ensg; ibu_h5_curr=SGD; IBU_TRANCE_LOG_URL=%2Fhotels%2Fsingapore-hotel-detail-996041%2Fparkroyal-on-beach-road-singapore%2F; librauuid=; _clck=5quac4|1|f4u|0; IBU_TRANCE_LOG_P=37682130973; hotel_h5_view_ids=%5B%22996041%22%5D; IBU_showtotalamt=1; ibu-h5-pop-hotel=0; ibu_online_permission_cls_ct=2; ibu_online_permission_cls_gap=1663082802191; hotelhst=1164390341; ibulanguage=SG; ibulocale=en_sg; intl_ht1=h4%3D73_687472%2C73_909897%2C73_994990%2C73_996041; _bfa=1.1658849562341.1pmnm5.1.1658849562341.1663083559409.2.32.10320668588; _bfs=1.18; _ubtstatus=%7B%22vid%22%3A%221658849562341.1pmnm5%22%2C%22sid%22%3A2%2C%22pvid%22%3A32%2C%22pid%22%3A10320668147%7D; hotel=687472; _ga=GA1.2.523270998.1658849562; _uetsid=505649c0337611ed8b2fb3e69a5735ed; _uetvid=3d45d8900cf811edadcee3300718def6; _clsk=kgr7wv|1663083673350|9|0|l.clarity.ms/collect; _bfi=p1%3D10320668147%26p2%3D10320668147%26v1%3D32%26v2%3D31; _bfaStatus=success; _ga_X437DZ73MR=GS1.1.1663081390.3.1.1663083757.0.0.0'),
    add_headers('dnt = 1'),
    add_headers('origin = https =//sg.trip.com'),
    add_headers('p = 37682130973'),
    add_headers('referer = https =//sg.trip.com/hotels/w/detail?hotelid=687472&curr=SGD&locale=en-SG&checkin=2022-09-28&checkout=2022-09-29&hoteluniquekey=H4sIAAAAAAAAAOPqY-HilWA0-A8DjEJMHIxSP5g5fr_dt5rF4regIwMIbJnl4Alm5Mx3COApBDN_tDg0Mgpn-zx-dK7RYRJjLyNn_6GvGjGvGxwEwYzDNQ5KzhzdL5byC4hLbHj3SFWBUROm0RDGsChz1HWWeQ0ywhPGCGLleJApwRLFxrF3jpsEh5IOx_01CwQFtCTOP76npcAKMcQhBGoIkGFxkg-o5yYPUA-DExvHv2XMEiwzGD_80tzICFbyoMhhByPTCcabTAuYLkybwLiLCaroEBMrxy19CZZTQPoJUPslJoZbTAyPmCAOeMXE8IkJ6ohfMC1NzBClXcwMk5ghumcxQ9QvYmY4xQIRusQymTG6Wik7tVLJykRHqSSzJCdVyUopzDFESUcpMTe_NK8EyDUy0TM0BQqUJFZ4poAVJifmJJfmJJakhlQWADWY1epgGhKcWlSWmZyqkJaaimyYsZmepSHCMHMshsXeYmF4xMIUHfuJheEXC8TRTawMXawMk1jZOObtZZJgEWIFR7yUgrGJZVJqmoG5hVFKskmieZKlZYpJUqKJUVKyqYmJoamZAp_G4sNNM9mMWLsYmYLdrZilGN08GIPYTC0MzV1comS4mIPdXQRXn78a9ua8sIMUiKcI4yWxpubpBrtnXOMqYOxi5BBg9GCMYKxgXMXIsIGR8QQj4yVGDmdH3wBHT3e_R4y8Hv4hrj7x7o4-Pq5Bka8YQSYBAAQ5MOS3AgAA'),
    add_headers('sec-ch-ua = "Google Chrome";v="105", "Not)A;Brand";v="8", "Chromium";v="105"'),
    add_headers('sec-ch-ua-mobile = ?1'),
    add_headers('sec-ch-ua-platform = "Android"'),
    add_headers('sec-fetch-dest = empty'),
    add_headers('sec-fetch-mode = cors'),
    add_headers('sec-fetch-site = same-origin'),
    add_headers('user-agent = Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Mobile Safari/537.36'),
    body = sprintf('{"searchCondition":{"masterHotelId":%d,"adult":1,"child":0,"age":"","priceType":1,"priceTypes":"1","cityId":73,"checkIn":"2022-10-26","checkOut":"2022-10-27","roomNum":1,"isBusiness":0},"filterCondition":{},"mapType":null,"isNewMapType":"1","Head":{"Locale":"en-SG","Currency":"SGD","AID":"30074","SID":"1622045","ClientID":"1658849562341.1pmnm5","OUID":"ctag.hash.463e4d8ff02f","CAID":"30074","CSID":"1622045","COUID":"ctag.hash.463e4d8ff02f","TimeZone":"8","PageID":"10320668577","HotelExtension":{"WebpSupport":true,"Qid":201198490140,"hasAidInUrl":false,"group":"TRIP","PID":"1e97f1cd-b771-44f3-9290-d98333b53452"},"Frontend":{"vid":"1658849562341.1pmnm5","sessionID":"9","pvid":"74"},"P":"91033365022","Device":"WAP","Version":"0"}}', as.integer(masterHotelId)),
    encode = "json",
    verbose()
    )
  data <- fromJSON(rawToChar(res$content))
  jsonData <- data$Response$hotelList
  return(jsonData)
}

id <- as.integer(996326)
newData <- apiCallNearbyHotel(id)


# Now we can start crawling for unique hotel IDs. From hotel list -> base$hotelid -> store in a list -> loop and call -> ensure that only unique ID is used -> ensure that we dont repeat call



