#' Finds a Stack Overflow chat room URL from the name or number.
#' 
#' @param room_name Name of room for which to search. Also accepts room number 
#' or a regular expression. Case insensitive.
#' 
#' @param exact Logical. If `TRUE`, limits search to room names instead of the 
#' full URLs including room numbers. Useful for searching for rooms with short 
#' names, e.g. "R". Defaults to `FALSE`.
#' 
#' @return A room URL as a string. Will return multiple matches as a character 
#' vector.
#' 
#' @examples 
#' 
#' find_room("R-Public")
#' find_room("R", exact = TRUE)
#' 
#' @export
find_room <- function(room_name, exact = FALSE){
    pattern <- ifelse(exact == TRUE, paste0('/', room_name, '$'), room_name)
    grep(pattern, rooms(), value = TRUE, ignore.case = TRUE)
}