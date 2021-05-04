
# Simulate data with or without normality --------------------------------
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

data_ej <- sim_norm(100, 4, mean.v=c(10,3,20,2), sd.v=c(0.1,0.2,0.14,0.01))

data_ej_no_norm <- sim_norm(100, 4, lambda=3, normality=FALSE)
#Multivariate normality test
library(MVN)

#Mardia’s multivariate skewness and kurtosis coefficients, if both tests indicates multivariate normality, then data follows a multivariate normality distribution at the 0.05 significance level
mvn(data_ej, mvnTest = "mardia")
mvn(data_ej_no_norm, mvnTest = "mardia")

#Royston test, it depends on shapiro-wilk tests
mvn(data_ej, mvnTest = "royston")
mvn(data_ej_no_norm, mvnTest = "royston")

#Doornik-Hansen’s MVN test
mvn(data_ej, mvnTest = "dh")
mvn(data_ej_no_norm, mvnTest = "dh")

#Energy test
mvn(data_ej, mvnTest = "energy")
mvn(data_ej_no_norm, mvnTest = "energy")
