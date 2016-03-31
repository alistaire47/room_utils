library(rvest)
library(ggmap)
library(leaflet)
library(rworldmap)

get_location <- function(rel_url){
    user_page <- paste0('http://www.stackoverflow.com/',rel_url,'?tab=profile')
    user_page %>% read_html() %>% html_nodes('.user-links') %>% 
        html_nodes(xpath = "//span[@class='icon-location']/..") %>% html_text(trim = TRUE)
}

update_user_info <- function(){
    room_page <- "http://chat.stackoverflow.com/rooms/info/25312/r-public"
    room_users <<- room_page %>% read_html() %>% html_node('#room-usercards-container') %>% 
        html_nodes('h3') %>%  html_nodes('a') %>% html_attr("href")
    
    room_owners <<- room_page %>% read_html() %>% html_node('#room-ownercards') %>% 
        html_nodes('h3') %>%  html_nodes('a') %>% html_attr("href")
    
    user_df <<- data.frame(user = sub('.*/([^/]+)$', '\\1', room_users), 
                          location = as.character(lapply(room_users, get_location)),
                          stringsAsFactors = FALSE)
    user_df[user_df$location == 'character(0)', 'location'] <<- NA
    
    user_df <<- merge(user_df, data.frame(location = user_df$location[!is.na(user_df$location)], 
                                         geocode(user_df$location[!is.na(user_df$location)])), 
                     all = TRUE)
}

plot_current_users <- function(){
    update_user_info()
    leaflet(user_df) %>% addProviderTiles('CartoDB.PositronNoLabels') %>% 
        addCircleMarkers(popup = paste0('<strong>User: </strong>', user_df$user))
}

plot_using_rworldmap <- function(){
    newmap <- getMap(resolution = "low")
    plot(newmap)
    points(user_df$lon, user_df$lat, col = 'red', cex = 2, pch = 21)
}
