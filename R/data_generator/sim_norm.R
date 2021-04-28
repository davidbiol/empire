
# Simulate data with or without normality --------------------------------
gen_sim_norm <- function(n, k=3, mean.v=rep(0,k), sd.v=rep(1,k), lambda, normality=TRUE){
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

#Normal data set
data_ej_norm <- gen_sim_norm(n=100,k=3)
sapply(data_ej, shapiro.test)

#No normal data set
data_ej_no_norm <- gen_sim_norm(n=100,lambda=4, normality = FALSE)
sapply(data_ej_no_norm, shapiro.test)
