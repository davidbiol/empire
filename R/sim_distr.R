
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

sim_distr <- function(n, k = 3, distr, ...){

  if (length(distr)==1){
    distr=rep(distr, k)
  }

  tryCatch(if(length(distr)!=k) stop("The number of distributions ('distr') must be the same number of variables ('k').", call.=FALSE))

  df <- data.frame(matrix(ncol=k, nrow=n))

  distr_args <- list(...)
  mean_arg <- c(distr_args$mean,distr_args$sd)
  poisson_arg <- c(distr_args$lambda)


  for(i in seq_len(k)) {
    tryCatch(rlang::arg_match0(distr[i], c("normal", "exponential", "poisson", "weibull", "chisquare", "gamma", "uniform"), arg_nm = substitute("Distribution elements")))

    rdistr <- switch(distr[i],
                     "normal" = stats::rnorm(n, mean_arg),
                     "exponential" = stats::rexp(n, ...),
                     "poisson" = stats::rpois(n, poisson_arg),
                     "weibull" = stats::rweibull(n, ...),
                     "chisquare" = stats::rchisq(n, ...),
                     "gamma" = stats::rgamma(n, ...),
                     "uniform" = stats::runif(n, ...))
    rdistr -> df[i]
  }
  return(df)

}
sim_distr(n=20, k=3, distr=c("normal", "poisson"), mean=2,sd=1, lambda=4)
sim_distr(n=20, k=4, distr=c("normal"), mean=2,sd=1)

