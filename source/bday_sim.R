#' Duplicate birthday check function
#'
#' @param n_room, number of people in a room
#'
#' @returns repeated_bday, if there is duplicate birthday
#' 
#' 
bday_sim = function(n_room){
  
  birthdays = sample(1:365, n_room, replace = TRUE)
  
  repeated_bday = length(unique(birthdays)) < n_room
  
  repeated_bday
}