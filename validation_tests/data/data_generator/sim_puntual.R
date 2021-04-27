#--------------------------------------------------------------------------

##Simulando estimadores puntuales
##Observar el espacio de parámetros

#Distribución normal

tamano_muestral <- 35
iteraciones <- 100

medias <- vector()
desv_est <- vector()

media_poblacional <- 3
desv_est_poblacional <- 5

#iteraciones
for(i in seq_len(iteraciones)){
  muestra <- rnorm(tamano_muestral, media_poblacional, desv_est_poblacional)
  medias[i] <- mean(muestra)
  desv_est[i] <- sd(muestra)
  
}

#Gráfica de espacio de parámetros
plot(medias, desv_est)
points(media_poblacional,desv_est_poblacional,col=2, cex=2,pch=20)


#Regresión lineal
tamano_muestral <- 100
iteraciones <- 1000

beta_0 <- 1
beta_1 <- -0.3

beta_0_est <- vector()
beta_1_est <- vector()

generar_y <- function(x, beta_0, beta_1){
  beta_0 + x*beta_1 + rnorm(x, 0, 0.1)
}

for(i in seq_len(iteraciones)){
  X <- seq(-3, 3, length.out = tamano_muestral)
  Y <- generar_y(X, beta_0, beta_1)
  betas_estimados <- coef(lm(Y ~ X))
  beta_0_est[i] <- betas_estimados[1]
  beta_1_est[i] <- betas_estimados[2]
}

#Espacio de parámetros
plot(beta_0_est, beta_1_est)
points(beta_0, beta_1, col=2, cex=2, pch=20)
