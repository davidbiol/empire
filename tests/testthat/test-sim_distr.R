test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

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
  #data_sim <- mvrnorm(100, mu=mu, Sigma=R)
  data_sim <- sim_norm(n=100, k=3, distr="Weibull", shape=1)
  #data_sim <- sim_norm(n=100, k=3, distr="Poisson", lambda = 4)
  #data_sim <- sim_norm(n=50, k=3, distr="Exponential")

  #Test
  mrdsk[i] <- mvn(data_sim, mvnTest = "mardia")$multivariateNormality$Result[1]
  mrdku[i] <- mvn(data_sim, mvnTest = "mardia")$multivariateNormality$Result[2]
  roy[i] <- mvn(data_sim, mvnTest = "royston")$multivariateNormality$MVN
  dht[i] <- mvn(data_sim, mvnTest = "dh")$multivariateNormality$MVN
  ene[i] <- mvn(data_sim, mvnTest = "energy")$multivariateNormality$MVN
}

#Data frame with results


par(mfrow=c(3,2))
pie(table(mrdsk), main="mrdsk")
pie(table(mrdku), main="mrdku")
pie(table(roy), main="roy")
pie(table(dht), main="dht")
pie(table(ene), main="ene")

