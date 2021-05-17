#' Number of missing values in data
#'
#' @param data
#'
#' @return An integer number of total missing values in data
#' @export
#'
#' @examples
#'

count_miss <- function(data){
  return(length(which(is.na(data))))
}
