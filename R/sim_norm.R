
# Simulate data with or without normality
#'
#' @param n
#' @param k
#' @param mean.v
#' @param sd.v
#' @param lambda
#' @param normality
#'
#' @return
#' @export
#'
#' @examples
#' #Normal data set
#' data_ej_norm <- sim_norm(n = 100,k = 3, normality=TRUE)
#' sapply(data_ej, shapiro.test)
#' #No normal data set
#' data_ej_no_norm <- sim_norm(n = 100, lambda = 4, normality = FALSE)
#' sapply(data_ej_no_norm, shapiro.test)

sim_norm <- function(n, k=3, mean.v=rep(0,k), sd.v=rep(1,k), lambda, normality=TRUE){
  data_sim <- data.frame(matrix(ncol=k, nrow=n))
  if (normality) {
    for(i in seq_len(k)){
      rnorm(n=n, mean=mean.v[i], sd=sd.v[i])-> data_sim[i]
    }
    return(data_sim)
  }
  else {
    for(i in seq_len(k)){
      rpois(n=n, lambda=lambda)-> data_sim[i]
    }
    return(data_sim)
  }
}

