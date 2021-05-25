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
  if (!is.data.frame(data) & !is.matrix(data) & !is.vector(data)){
    stop("Data to be evaluated (data) should be a data frame or a matrix")
  }
  if (is.vector(data)){
    warning("Data to be evaluated is a vector and it should be a data frame or a matrix")
  }

  return(length(which(is.na(data))))
}
