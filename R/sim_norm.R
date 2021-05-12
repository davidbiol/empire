
# Simulate data variables with or without normality --------------------------------
sim_norm <- function(n, k=3, distr, mean.v=rep(0,k), sd.v=rep(1,k), lambda=3, shape=1){
  data_sim <- data.frame(matrix(ncol=k, nrow=n))

  if (distr=="Normal") {
    for(i in seq_len(k)){
      stats::rnorm(n, mean=mean.v[i], sd=sd.v[i])-> data_sim[i]
    }
    return(data_sim)
  } else if (distr=="Poisson") {
    for(i in seq_len(k)){
      stats::rpois(n, lambda)-> data_sim[i]
    }
    return(data_sim)
  } else if (distr=="Weibull") {
    for(i in seq_len(k)){
      stats::rweibull(n,shape)-> data_sim[i]
    }
    return(data_sim)
  } else {
    warning("There is not any distribution (distr) set. Distribution could be Normal, Poisson or Weibull")
  }
}
sim_norm(n=100, k=4, distr="Weibull", shape=1)
library(MASS)
mu <- c(X1 = 0, X2=0, Y = 0)
R <- matrix(c(1, 0.5,0.5,
              0.5,1,0.5,
              0.5,0.5, 1),
            nrow = 3, ncol = 3)
data_mvrnorm <- mvrnorm(100, mu=mu, Sigma=R)

data_ej <- sim_norm(100, 4, mean.v=c(10,3,20,2), sd.v=c(0.1,0.2,0.14,0.01))

data_ej_no_norm <- sim_norm(100, 4, lambda=3, normality=FALSE)
#Multivariate normality test
library(MVN)

#Mardia’s multivariate skewness and kurtosis coefficients, if both tests indicates multivariate normality, then data follows a multivariate normality distribution at the 0.05 significance level
mvn(data_ej, mvnTest = "mardia")
mvn(data_ej_no_norm, mvnTest = "mardia")
mvn(data_mvrnorm, mvnTest = "mardia")

#Royston test, it depends on shapiro-wilk tests
mvn(data_ej, mvnTest = "royston")
mvn(data_ej_no_norm, mvnTest = "royston")
mvn(data_mvrnorm, mvnTest = "royston")

#Doornik-Hansen’s MVN test
mvn(data_ej, mvnTest = "dh")
mvn(data_ej_no_norm, mvnTest = "dh")
mvn(data_mvrnorm, mvnTest = "dh")

#Energy test
mvn(data_ej, mvnTest = "energy")
mvn(data_ej_no_norm, mvnTest = "energy")
mvn(data_mvrnorm, mvnTest = "energy")

#-----------------------------------------

#Distribution tester
iterations <- 100
mrdsk <- mrdku <- roy <- dht <- ene <- vector()
for (i in seq_len(iterations)){
  #Simulate data
  library(MASS)
  #mu <- c(runif(1,0,100), runif(1,0,100),runif(1,0,100), runif(1,0,100)) #Means are random between 0 an 100

  # R <- matrix(c(1, 0.5,0.5,0.5,
  #               0.5,1,0.5,0.5,
  #               0.5,0.5,1,0.5,
  #               0.5,0.5,0.5, 1),
  #             nrow = 4, ncol = 4) #Suppose homocedasticity

  #Distribution
  #data_mvrnorm <- mvrnorm(100, mu=mu, Sigma=R)
  #data_weibull <- sim_norm(n=100, k=3, distr="Weibull", shape=1)
  data_poisson <- sim_norm(n=100, k=3, distr="Poisson", )

  #Test
  mrdsk[i] <- mvn(data_mvrnorm, mvnTest = "mardia")$multivariateNormality$Result[1]
  mrdku[i] <- mvn(data_mvrnorm, mvnTest = "mardia")$multivariateNormality$Result[2]
  roy[i] <- mvn(data_mvrnorm, mvnTest = "royston")$multivariateNormality$MVN
  dht[i] <- mvn(data_mvrnorm, mvnTest = "dh")$multivariateNormality$MVN
  ene[i] <- mvn(data_mvrnorm, mvnTest = "energy")$multivariateNormality$MVN
}

#Data frame with results


par(mfrow=c(3,2))
pie(table(mrdsk), main="mrdsk")
pie(table(mrdku), main="mrdku")
pie(table(roy), main="roy")
pie(table(dht), main="dht")
pie(table(ene), main="ene")

plot(rweibull(10, shape=1))
