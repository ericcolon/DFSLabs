
#' Create a new data frame from a Fantasy Lab MLB player model data frame.
#' 
#' Receives 10 player names to create a data frame with for further analysis.
#' 
#' @param n1 Name of a player from a player model data frame 
#' @param n2 Name of second player
#' @param n3 Name of third player
#' @param n4 Name of fourth player
#' @param n5 Name of fifth player
#' @param n6 Name of sixth player
#' @param n7 Name of seventh player
#' @param n8 Name of eighth player
#' @param n9 Name of ninth player
#' @param n10 Name of tenth player
#' @param model Data frame model to pull from
#'
#' @return Updated data frame containing only the 10 players in question.
#' @export
#'
#' @examples extract10Mlb(n1="David Ortiz",n2="Jose Altuve",n3="Neil Walker",n4="Mike Trout",n5="Bryce Harper",n6="Curtis Granderson",n7="Lucas Duda",n8="Alex Dickerson",n9="Trea Turner",n10="Freddie Freeman", model=mlbModel)
extract10Mlb <- function(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,model="mlbModel"){
  sR1 <- which(grepl(n1, model$Properties.Player_Name))
  m1 <- model[sR1,]
  
  sR2 <- which(grepl(n2, model$Properties.Player_Name))
  m2 <- model[sR2,]
  
  sR3 <- which(grepl(n3, model$Properties.Player_Name))
  m3 <- model[sR3,]
  
  sR4 <- which(grepl(n4, model$Properties.Player_Name))
  m4 <- model[sR4,]
  
  sR5 <- which(grepl(n5, model$Properties.Player_Name))
  m5 <- model[sR5,]
  
  
  sR6 <- which(grepl(n6, model$Properties.Player_Name))
  m6 <- model[sR6,]
  
  sR7 <- which(grepl(n7, model$Properties.Player_Name))
  m7 <- model[sR7,]
  
  sR8 <- which(grepl(n8, model$Properties.Player_Name))
  m8 <- model[sR8,]
  
  sR9 <- which(grepl(n9, model$Properties.Player_Name))
  m9 <- model[sR9,]
  
  sR10 <- which(grepl(n10, model$Properties.Player_Name))
  m10 <- model[sR10,]
  
  extracted10 <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
  return(extracted10)
}