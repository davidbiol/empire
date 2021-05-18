#' Simple median imputation of missing values
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
imp_median <- function(data){
  positions <- pos_miss(data)

  est_values = vector()
  new_data = data

  for(i in seq_len(nrow(positions))){
    new_data[positions[i,1], positions[i,2]] <- stats::median(data[, positions[i,2]], na.rm=TRUE)
    est_values[i] <- new_data[positions[i,1], positions[i,2]]
  }

  # List
  my_list <- list("positions" = positions, "est_values" = est_values, "new_data" = new_data)

}
