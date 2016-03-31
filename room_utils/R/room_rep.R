#' Sum of the Reputation of Current Room Occupants 
#' 
#' Sums the reputation of users currently in the specified room.
#' 
#' @param room_id The ID number of the room you want to retrieve. Defaults to
#' \code{"25312"} which is the "R Public" room.
#' 
#' @return A single integer.
#' 
#' @examples 
#' 
#' room_rep()
#' room_rep("106")
#' 
#' @export
room_rep <- function(room_id = "25312") {
  url_stub(room_id) %>%
    xml2::read_html() %>% 
    rvest::html_node('#room-usercards-container') %>% 
    rvest::html_nodes('.reputation-score') %>% 
    rvest::html_attr('title') %>% 
    as.integer() %>% 
    sum()
}
NULL