#' Missing value position inside data set
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
pos_miss <- function(data){

  df = data.frame(Row = vector(), Col = vector())

  mv <- which(is.na(data))

  for (k in seq_along(mv)){ #Finding positions with modulo operators
    df[k, 2] = ceiling(mv[[k]] / nrow(data)) #Set column
    if (mv[[k]]%%nrow(data)==0) df[k, 1] = nrow(data) else df[k, 1] = mv[[k]]%%nrow(data) #Set row,the exception is when the na is in the last row
  }

  return(df)
}
