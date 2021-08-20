## Testing on structure
test_that("Function returns correct type", {
  expect_equal(class(sim_distr(n=10, k=5, distr="normal")), "data.frame")
  expect_equal(class(sim_distr(n=10, k=4, distr=c("poisson", "chisquare", "weibull", "uniform"), lambda=4, df=3, ncp=1, scale=2, shape=1, min=1, max=4)), "data.frame")
})

test_that("Function returns correct number of observations", {
  n <- sample(1:1000, 1)
  expect_equal(nrow(sim_distr(n=n, k=5, distr="normal")), n)
  expect_equal(nrow(sim_distr(n=n, k=3, distr=c("normal", "exponential", "weibull"), mean=1, sd=0.2, rate=0.5, scale=2, shape=3)), n)
})

## Mean and SD accuracy
mean_x1 <- sd_x1 <- vector()
#Take a random integer as population mean and sd
mean_p <- sample(50:1000,1)
sd_p <- sample(1:100, 1)
for (i in seq_len(1000)){

  X <- sim_distr(n=100, k=3, distr=c("exponential", "normal", "weibull"), scale=2, shape=1,sd=sd_p, mean=mean_p)

  mean_x1[i] <- mean(X[,2])
  sd_x1[i] <- sd(X[,2])
}

test_that("Mean and Standard deviation are accurately calculated", {
  expect_equal(mean(mean_x1), mean_p, tolerance = 1)
  expect_equal(mean(sd_x1), sd_p, tolerance = 1)
})

## Error testing
test_that("Error for number of observations not an integer", {
  expect_error(sim_distr(n="hello", k=5, distr="normal"))
  expect_error(sim_distr(n=TRUE, k=4, distr=c("poisson", "chisquare", "weibull", "uniform"), lambda=4, df=3, ncp=1, scale=2, shape=1, min=2, max=4))
  expect_error(sim_distr(n=1.4, k=2, distr=c("poisson", "normal"), lambda=4, mean=40, sd=3))
  expect_error(sim_distr(n=3i, k=4, distr=c("weibull", "uniform"), scale=2, shape=1, min=1, max=4))
})

test_that("Error for number of variables not an integer", {
  expect_error(sim_distr(n=1000, k="hello", distr="normal"))
  expect_error(sim_distr(n=80, k=TRUE, distr=c("poisson", "chisquare", "weibull", "uniform"), lambda=4, df=3, ncp=1, scale=2, shape=1, min=1, max=4))
  expect_error(sim_distr(n=40, k=1.4, distr=c("poisson", "normal"), lambda=4, mean=40, sd=3))
  expect_error(sim_distr(n=10, k=3i, distr=c("weibull", "uniform"), scale=2, shape=1, min=1, max=4))
})

test_that("Error for missing distribution arguments", {
  expect_error(sim_distr(n=50, k=3, distr=c("normal", "exponential", "weibull")))
  expect_error(sim_distr(n=50, k=3, distr=c("chisquare", "uniform", "poisson")))
})

# #Multivariate normality test
# library(MVN)
#
# #Mardia’s multivariate skewness and kurtosis coefficients, if both tests indicates multivariate normality, then data follows a multivariate normality distribution at the 0.05 significance level
# mvn(data_ej, mvnTest = "mardia")
# mvn(data_ej_no_norm, mvnTest = "mardia")
# mvn(data_mvrnorm, mvnTest = "mardia")
#
# #Royston test, it depends on shapiro-wilk tests
# mvn(data_ej, mvnTest = "royston")
# mvn(data_ej_no_norm, mvnTest = "royston")
# mvn(data_mvrnorm, mvnTest = "royston")
#
# #Doornik-Hansen’s MVN test
# mvn(data_ej, mvnTest = "dh")
# mvn(data_ej_no_norm, mvnTest = "dh")
# mvn(data_mvrnorm, mvnTest = "dh")
#
# #Energy test
# mvn(data_ej, mvnTest = "energy")
# mvn(data_ej_no_norm, mvnTest = "energy")
# mvn(data_mvrnorm, mvnTest = "energy")

#-----------------------------------------
#
# #Distribution tester
# library(empire)
# iterations <- 50
# mrdsk <- mrdku <- roy <- dht <- ene <- vector()
# for (i in seq_len(iterations)){
#   #Simulate data
#   library(MASS)
#   mu <- c(runif(1,0,100), runif(1,0,100),runif(1,0,100), runif(1,0,100)) #Means are random between 0 an 100
#
#   R <- matrix(c(1, 0.5,0.5,0.5,
#                 0.5,1,0.5,0.5,
#                 0.5,0.5,1,0.5,
#                 0.5,0.5,0.5, 1),
#               nrow = 4, ncol = 4) #Suppose homocedasticity
#
#   #Distribution
#   #data_sim <- mvrnorm(100, mu=mu, Sigma=R)
#   data_sim <- sim_distr(n=100, k=4, distr=c("normal","exponential","poisson"))
#   #data_sim <- sim_norm(n=100, k=3, distr="Poisson", lambda = 4)
#   #data_sim <- sim_norm(n=50, k=3, distr="Exponential")
#
#   #Test
#   mrdsk[i] <- mvn(data_sim, mvnTest = "mardia")$multivariateNormality$Result[1]
#   mrdku[i] <- mvn(data_sim, mvnTest = "mardia")$multivariateNormality$Result[2]
#   roy[i] <- mvn(data_sim, mvnTest = "royston")$multivariateNormality$MVN
#   dht[i] <- mvn(data_sim, mvnTest = "dh")$multivariateNormality$MVN
#   ene[i] <- mvn(data_sim, mvnTest = "energy")$multivariateNormality$MVN
# }
#
# #Data frame with results
#
#
# par(mfrow=c(3,2))
# pie(table(mrdsk), main="mrdsk")
# pie(table(mrdku), main="mrdku")
# pie(table(roy), main="roy")
# pie(table(dht), main="dht")
# pie(table(ene), main="ene")

