\name{sim_norm}
\alias{sim_norm}
\title{Simulate data with or without normality}
\usage{
sim_norm(
  n = 100,
  k = 3,
  mean.v = rep(0,k),
  sd.v = rep(1,k),
  lambda = 5,
  normality = TRUE)
)
}
\arguments{
n
k
mean.v
}
\description{
Generates simulated data sets with normality (rnorm) or without normality (rpois).
}
\examples{
#Normal data set
data_ej_norm <- sim_norm(n = 100,k = 3, normality=TRUE)
sapply(data_ej, shapiro.test)

#No normal data set
data_ej_no_norm <- sim_norm(n = 100, lambda = 4, normality = FALSE)
sapply(data_ej_no_norm, shapiro.test)
}
