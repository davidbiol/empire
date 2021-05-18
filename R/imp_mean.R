#' Simple mean imputation of missing values
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
imp_mean <- function(data){

  positions <- pos_miss(data)

  est_values = vector()
  new_data = data

for(t in seq_len(nrow(positions))){
  new_data[positions[t,1], positions[t,2]] <- mean(data[, positions[t,2]], na.rm=TRUE)
  est_values[t] <- new_data[positions[t,1], positions[t,2]]
}

  # List
  my_list <- list("positions" = positions, "est_values" = est_values, "new_data" = new_data)

}
