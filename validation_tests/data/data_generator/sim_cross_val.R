library(caret) #Para realizar vaidación cruzada
library(nnet) #Para la red neuronal
library(parallel) #Realizar computación paralela


# Funcion de pliegue ------------------------------------------------------

rmse_fold <- function(pliegue, form, datos, nn_size){
  pliegue_logic <- seq_len(nrow(datos)) %in% pliegue #Vector lógico
  prueba <- subset(datos, pliegue_logic) #Traer las filas correspondientes a los datos de prueba
  entrena <- subset(datos, !pliegue_logic)
  modelo <- nnet(form, data=entrena, size=nn_size, linout=TRUE, trace=FALSE)
  response_name <- setdiff(names(datos), modelo$coefnames)
  y_pronosticado <- predict(modelo, newdata=prueba)
  rmse <- RMSE(y_pronosticado, prueba[[response_name]]) #Calcula el error estandar
  rmse
}


# Modelo no lineal --------------------------------------------------------


tamano_muestral <- 30

genera_y <- function(x){
  cos(x) + rnorm(length(x), 0, 0.5)
}

X <- seq(0, 3*pi, length.out = tamano_muestral)
Y <- genera_y(X)

data.frame(X, Y) -> muestra


# Validación cruzada ------------------------------------------------------

n_pliegues <- 5
neuronas <- 10

createFolds(muestra$Y, k=n_pliegues) -> pliegues

#Trabajar en todos los pliegues al mismo tiempo
mclapply(
  pliegues,
  rmse_fold,
  Y ~ X, #Esta es la fórmula
  muestra,
  nn_size = neuronas,
  mc.cores= 1#Número de núcleos a utilizar en el computador 
) -> rmse_pliegues

rmse_pliegues <- unlist(rmse_pliegues) #Vectorizarlo

plot(rmse_pliegues)
abline(h=mean(rmse_pliegues))

#Cuando los RMSE varían mucho, puede ser porque el modelo se ajusta mejor a algunas submuestras que a otras