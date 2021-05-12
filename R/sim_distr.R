
#' @title Simulate data variables with known distributions
#'
#' @param n number of observations.
#' @param k number of variables.
#' @param distr the distribution to be used. This must be one of "normal", "exponential", "poisson", "weibull", "chisquare", "gamma" or "uniform".
#' @param ... arguments specific to each distribution. See \code{\link[stats:rnorm]{rnorm}}, \code{\link[stats:rexp]{rexp}}, \code{\link[stats:rpois]{rpois}}, \code{\link[stats:rweibull]{rweibull}}, \code{\link[stats:rchisq]{rchisq}}, \code{\link[stats:rgamma]{rgamma}}, \code{\link[stats:runif]{runif}} to know these arguments.
#'
#' @return
#' \code{sim_distr} gives a data frame with the observations as rows and the variables as columns
#' @export
#'
#' @examples
#'
#' @seealso \code{\link[stats:rnorm]{rnorm}},
#' \code{\link[stats:rexp]{rexp}},
#' \code{\link[stats:rpois]{rpois}},
#' \code{\link[stats:rweibull]{rweibull}},
#' \code{\link[stats:rchisq]{rchisq}},
#' \code{\link[stats:rgamma]{rgamma}},
#' \code{\link[stats:runif]{runif}}.

sim_distr <- function(n, k = 3, distr, ...){

  df <- data.frame(matrix(ncol=k, nrow=n))
  rdistr <- switch(distr,
         "normal" = stats::rnorm(n, ...),
         "exponential" = stats::rexp(n, ...),
         "poisson" = stats::rpois(n, ...),
         "weibull" = stats::rweibull(n, ...),
         "chisquare" = stats::rchisq(n, ...),
         "gamma" = stats::rgamma(n, ...),
         "uniform" = stats::runif(n, ...),
         stop("Distribution unknown, try with a known distribution. See ?sim_norm 'distr' for help"))

  for(i in seq_len(k)) {
    rdistr -> df[i]
  }
  return(df)

}
