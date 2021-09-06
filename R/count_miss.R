#' Number of missing values in data
#'
#' @param data A matrix or data frame object to be evaluated.
#'
#' @return An integer number of total missing values in data.
#' @export
#'
#' @examples

count_miss <- function(data){
  # Exceptions

  if (!is.data.frame(data) & !is.matrix(data)){
    stop("Data to be evaluated (data) should be a data frame or a matrix. If it's in fact a vector, try 'which(is.na(data))")
  }

  if(!all(unlist(lapply(data, is.numeric)), TRUE)){
    stop("Data to be evaluated has a non-numeric element. Chack as.numeric() to convert your data to a numeric class")
  }

  return(length(which(is.na(data))))
}
