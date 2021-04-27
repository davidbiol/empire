library(mice)
#uses pmm (predictive mean matching)

which(is.na(wine1))
pmm.imputed <- mice(wine1, method="pmm")
pmm.imputed$imp[7] #no estoy seguro si cada uno de los valores son independientes y en este caso cu?l deber?a tomarse

