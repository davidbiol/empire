install.packages("Amelia")
library(Amelia)
#uses EMB Algorithm
amelia.imputed <- amelia(wine1)
amelia.imputed$imputations[[1]][16,7] #Igual que mice, da vairos imputed outputs
