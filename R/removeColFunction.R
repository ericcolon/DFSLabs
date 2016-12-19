#' Remove DataFrame Column
#'
#' @param df 
#' @param col 
#'
#' @return Data Frame lacking desired column
#' @export
#'
#' @examples df <- removeCol(df,col)
removeCol <- function(df,col){
  df <- df %>% select(-col)
  return(df)
  }