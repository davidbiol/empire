#' Simple median imputation of missing values
#'
#' @param data A matrix or data frame object. It should have variables as columns and observations as rows.
#'
#' @return A list object with positions of missing values (positions), imputed values by observed median (imp_values) and new data with imputed values (new_data).
#' @export
#'
#' @examples
impute_median <- function(data){
  # Exceptions
  if (!is.data.frame(data) & !is.matrix(data)){
    stop("Data to be evaluated (data) should be a data frame or a matrix")
  }

  positions <- pos_miss(data)

  imp_values = vector()
  new_data = data

  for(i in seq_len(nrow(positions))){
    new_data[positions[i,1], positions[i,2]] <- stats::median(data[, positions[i,2]], na.rm=TRUE)
    imp_values[i] <- new_data[positions[i,1], positions[i,2]]
  }

  # List
  my_list <- list("positions" = positions, "imp_values" = imp_values, "new_data" = new_data)

  return(my_list)
}
