
#' @title Simulate data variables with known distributions
#'
#'
#' @param n number of observations
#' @param k number of variables
#' @param distr
#' @param ...
#'
#' @return
#' sim_distr gives a data frame with the observations as rows and the variables as columns
#' @export
#'
#' @examples

sim_distr <- function(n, k=3, distr, ...){

  df <- data.frame(matrix(ncol=k, nrow=n))
  rdistr <- switch(distr,
         "Normal" = stats::rnorm(n, ...),
         "Exponential" = stats::rexp(n, ...),
         "Poisson" = stats::rpois(n, ...),
         "Weibull" = stats::rweibull(n, ...),
         stop("Distribution unknown, try with a known distribution. See ?sim_norm 'distr' for help"))

  for(i in seq_len(k)) {
    rdistr -> df[i]
  }
  return(df)

}
