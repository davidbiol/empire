#' Simple mean imputation of missing values
#'
#' @param data A matrix or data frame object. It should have variables as columns and observations as rows.
#'
#' @return A list object with positions of missing values (positions), estimated values (est_values) and new data with imputed/estimated values (new_data)
#' @export
#'
#' @examples
impute_mean <- function(data){
  # Exceptions
  if (!is.data.frame(data) & !is.matrix(data)){
    stop("Data to be evaluated (data) should be a data frame or a matrix")
  }
  positions <- pos_miss(data)

  est_values = vector()
  new_data = data

for(i in seq_len(nrow(positions))){
  new_data[positions[i,1], positions[i,2]] <- mean(data[, positions[i,2]], na.rm=TRUE)
  est_values[i] <- new_data[positions[i,1], positions[i,2]]
}

  # List
  my_list <- list("positions" = positions, "est_values" = est_values, "new_data" = new_data)

  return(my_list)
}
