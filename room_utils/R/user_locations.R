#' Lists the Locations of Specified Users 
#' 
#' Uses the Stack Overflow API to list the locations of specified users.
#' 
#' @param user_ids The ID numbers of the users you want to retrieve the 
#' locations of. Can be a character or numeric vector.
#' 
#' @return A \code{data.frame} with the following columns: "reputation", 
#' "user_id", "location", and "display_name".
#' 
#' @examples 
#' 
#' user_location(1270695)
#' user_location(c(324364, 1270695))
#' 
#' @export
user_location <- function(user_ids) {
  p1 <- "http://api.stackexchange.com/2.2/users/"
  users <- paste(sapply(user_ids, as.numeric), collapse = "%3B")
  p2 <- "?order=desc&sort=reputation&site=stackoverflow"
  p3 <- "&filter=!G*klMr_BAt*6rRQ*_i7*TeZrrN"
  x <- httr::GET(sprintf("%s%s%s%s", p1, users, p2, p3))
  jsonlite::fromJSON(rawToChar(x$content))$items
}
NULL

#' Lists the Locations of Users in a Specified Room 
#' 
#' Scrapes the users in a specified Stack Overflow chat room and uses the Stack
#' Overflow API to geocode the locations, when available.
#' 
#' @param room_id The ID number of the room you want to retrieve. Defaults to
#' \code{"25312"} which is the "R Public" room.
#' 
#' @return A \code{data.table} with the following columns: "reputation", 
#' "user_id", "location", "display_name", "lon", and "lat".
#' 
#' @examples 
#' 
#' user_list()
#' user_list("106")
#' 
#' @export
user_list <- function(room_id = "25312") {
  temp <- url_stub(room_id)
  users <- temp %>% 
    xml2::read_html() %>% 
    rvest::html_node('#room-usercards-container') %>%
    rvest::html_nodes('h3') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr("href")
  users <- sapply(strsplit(gsub("^/", "", users), "/", TRUE), `[[`, 2)
  locs <- setDT(user_location(users))
  locs[!is.na(location), c("lon", "lat") := ggmap::geocode(location)][]
}
NULL

#' Plots the Locations of Users in a Specified Room 
#' 
#' Scrapes the users in a specified Stack Overflow chat room, uses the Stack
#' Overflow API to geocode the locations, and plots the locations on a map using
#' either the "leaflet" or "rworldmap" packages.
#' 
#' @param room_id The ID number of the room you want to retrieve. Defaults to
#' \code{"25312"} which is the "R Public" room.
#' @param type Can be either \code{"leaflet"} or \code{"rworldmap"}. Defaults to
#' \code{"leaflet"}.
#' 
#' @return A map.
#' 
#' @examples 
#' 
#' plot_current_users()
#' plot_current_users(type = "rworldmap")
#' plot_current_users("106")
#' 
#' @importFrom dplyr filter
#' @export
plot_current_users <- function(room_id = "25312", type = "leaflet") {
  user_df <- user_list(room_id) %>%
    dplyr::filter(!is.na(lat) & !is.na(lon))
  switch(
    type,
    leaflet = {
      leaflet::leaflet(user_df) %>%
        leaflet::addProviderTiles('CartoDB.PositronNoLabels') %>%
        leaflet::addMarkers(popup = sprintf(
          "User: %s<br />Reputation: %s",
          user_df$display_name, user_df$reputation))
      },
    rworldmap = {
      newmap <- rworldmap::getMap(resolution = "low")
      plot(newmap)
      points(user_df$lon, user_df$lat, col = 'red', cex = 2, pch = 21)
      },
    stop("type must be either 'leaflet' or 'rworldmap'"))
}
NULL
