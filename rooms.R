library(magrittr)
#' Lists all Stack Overflow chat rooms.
#' 
#' @param sort Desired sort order of rooms. Options: "active", "event", "people",
#' or "created". Defaults to "active".
#' 
#' @return A character vector of room URLs.
#' 
#' @examples 
#' 
#' tail(rooms('created'))
#' head(rooms())
#' 
#' @export
rooms <- function(sort = 'active'){
    url <- paste0('http://chat.stackoverflow.com/?tab=all&sort=', sort)
    
    # get maximum page number to loop on
    last_page <- url %>% 
        xml2::read_html() %>%
        rvest::html_nodes('.pager.clear-both a') %>% 
        rvest::html_text(trim = TRUE) %>% 
        tail(2) %>% 
        `[`(1) %>% 
        as.integer()
    
    # loop across pages
    rooms <- lapply(seq_len(last_page), function(x){
        paste0(url, '&page=', x) %>% 
            xml2::read_html() %>%
            rvest::html_nodes('.room-name a') %>% 
            rvest::html_attr('href')
    }) %>% 
        unlist()
    
    return(paste0('https://chat.stackoverflow.com', rooms))
}

