
#' @title Simulate data variables with known distributions
#'
#' @param n Number of observations.
#' @param k Number of variables.
#' @param distr A vector of distributions to be used in each variable. It could be "normal", "exponential", "poisson", "weibull", "chisquare", "gamma" or "uniform". In case to use the same distribution for all variables, set it as distr = "distr".
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

  # Exceptions
  if (!is.numeric(n) | n%%1 !=0){
    stop("The number of observations (n) must be an integer number")
  }
  if (!is.numeric(k) | k%%1 !=0){
    stop("The number of variables (k) must be an integer number")
  }

  if (length(distr)==1){
    distr=rep(distr, k)
  }

  tryCatch(if(length(distr)!=k) stop("The number of distributions ('distr') must be the same number of variables ('k').", call.=FALSE))

  df <- data.frame(matrix(ncol=k, nrow=n))

  # Assign arguments to each distribution
  distr_args <- list(...)
  normal_arg <- c(distr_args$mean, distr_args$sd) #assign mean=distr_args$mean...
  exp_arg <- c(distr_args$rate)
  poisson_arg <- c(distr_args$lambda)
  weibull_arg <- c(distr_args$shape, distr_args$scale)
  chisquare_arg <- c(distr_args$df, distr_args$ncp)
  gamma_arg <- c(distr_args$shape, distr_args$rate, distr_args$scale) #It can have issues
  unif_arg <- c(min=distr_args$min, max=distr_args$max)

  for(i in seq_len(k)) {
    tryCatch(rlang::arg_match0(distr[i], c("normal", "exponential", "poisson", "weibull", "chisquare", "gamma", "uniform"), arg_nm = substitute("Distribution elements")))

    rdistr <- switch(distr[i],
                     "normal" = do.call(stats::rnorm, c(list(n), normal_arg)),
                     "exponential" = do.call(stats::rexp, c(list(n), exp_arg)),
                     "poisson" = do.call(stats::rpois, c(list(n), poisson_arg)),
                     "weibull" = do.call(stats::rweibull, c(list(n), weibull_arg)),
                     "chisquare" = do.call(stats::rchisq, c(list(n), chisquare_arg)),
                     "gamma" = do.call(stats::rgamma, c(list(n), gamma_arg)),
                     "uniform" = do.call(stats::runif, c(list(n), unif_arg))) #It's not working
    rdistr -> df[i]
  }
  return(df)

}

