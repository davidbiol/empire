#' Missing value position inside data set
#'
#' @param data A matrix or data frame object. It should have variables as columns and observations as rows.
#'
#' @return A data frame with the row (Row) and column (Col) position of each missing value.
#' @export
#'
#' @examples
pos_miss <- function(data){
  # Exceptions
  if (!is.data.frame(data) & !is.matrix(data) & !is.vector(data)){
    stop("Data to be evaluated (data) should be a data frame or a matrix")
  }
  if (is.vector(data)){
    stop("Data to be evaluated is a vector and it should be a data frame or a matrix, change the class object to a matrix or data frame. If it's in fact a vector, try 'which(is.na(data))'")
  }

  if(!all(unlist(lapply(data, is.numeric)), TRUE)){
    stop("Data to be evaluated has a non-numeric element. Chack as.numeric() to convert your data to a numeric class")
  }


  df = data.frame(Row = vector(), Col = vector())

  mv <- which(is.na(data))

  for (k in seq_along(mv)){ #Finding positions with modulo operators
    df[k, 2] = ceiling(mv[[k]] / nrow(data)) #Set column
    if (mv[[k]]%%nrow(data)==0) df[k, 1] = nrow(data) else df[k, 1] = mv[[k]]%%nrow(data) #Set row,the exception is when the na is in the last row
  }

  return(df)
}
