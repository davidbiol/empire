
#' @title Simulate data variables with known distributions
#'
#' @param n Number of observations.
#' @param k Number of variables.
#' @param distr The distribution to be used. This must be one of "normal", "exponential", "poisson", "weibull", "chisquare", "gamma" or "uniform".
#' @param ... Arguments specific to each distribution. See \code{\link[stats:rnorm]{rnorm}}, \code{\link[stats:rexp]{rexp}}, \code{\link[stats:rpois]{rpois}}, \code{\link[stats:rweibull]{rweibull}}, \code{\link[stats:rchisq]{rchisq}}, \code{\link[stats:rgamma]{rgamma}}, \code{\link[stats:runif]{runif}} to know these arguments.
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

sim_distr <- function(n, k = 3, distr="normal", ...){

  rlang::arg_match0(distr, c("normal", "exponential", "poisson", "weibull", "chisquare", "gamma", "uniform"))

  df <- data.frame(matrix(ncol=k, nrow=n))

  for(i in seq_len(k)) {
    rdistr <- switch(distr,
                     "normal" = stats::rnorm(n, ...),
                     "exponential" = stats::rexp(n, ...),
                     "poisson" = stats::rpois(n, ...),
                     "weibull" = stats::rweibull(n, ...),
                     "chisquare" = stats::rchisq(n, ...),
                     "gamma" = stats::rgamma(n, ...),
                     "uniform" = stats::runif(n, ...))
    rdistr -> df[i]
  }
  return(df)

}
