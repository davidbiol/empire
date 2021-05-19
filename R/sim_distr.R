
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

sim_distr <- function(n, k, distr, ...){

  if (length(distr)==1){
    distr=rep(distr, k)
  }

  tryCatch(if(length(distr)!=k) stop("The number of distributions ('distr') must be the same number of variables ('k').", call.=FALSE))

  df <- data.frame(matrix(ncol=k, nrow=n))

  # Assign arguments to each distribution
  distr_args <- list(...)
  normal_arg <- c(distr_args$mean, distr_args$sd)
  exp_arg <- c(distr_args$rate)
  poisson_arg <- c(distr_args$lambda)
  weibull_arg <- c(distr_args$shape, distr_args$scale)
  chisquare_arg <- c(distr_args$df, distr_args$ncp)
  gamma_arg <- c(distr_args$shape, distr_args$rate, distr_args$scale) #It can have issues
  unif_arg <- vector(min=distr_args$min, max=distr_args$max)
  print(unif_arg)

  for(i in seq_len(k)) {
    tryCatch(rlang::arg_match0(distr[i], c("normal", "exponential", "poisson", "weibull", "chisquare", "gamma", "uniform"), arg_nm = substitute("Distribution elements")))

    rdistr <- switch(distr[i],
                     "normal" = stats::rnorm(n, normal_arg),
                     "exponential" = stats::rexp(n, exp_arg),
                     "poisson" = stats::rpois(n, poisson_arg),
                     "weibull" = stats::rweibull(n, weibull_arg),
                     "chisquare" = stats::rchisq(n, chisquare_arg),
                     "gamma" = stats::rgamma(n, gamma_arg),
                     "uniform" = stats::runif(n, unif_arg)) #It's not working
    rdistr -> df[i]
  }
  return(df)

}
sim_distr(n=20, k=3, distr="uniform", min=10, max=11)
runif(10, 10,11)
