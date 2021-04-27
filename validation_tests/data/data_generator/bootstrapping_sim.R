
# Bootstrapping en un modelo ----------------------------------------------
#Se basa en el remuestreo

tamano_muestral <- 23
iteraciones <- 10000
beta_0 <-  1
beta_1 <- -0.3
desv_est_error <- 0.5

#Crear generadores de datos
genera_x <- function(n){
  seq(-3,3,length.out=n)
}

genera_y <- function(x, b0, b1){
  y <- b1*x + b0 + rnorm(length(x),0,desv_est_error)
}

datos_x <- genera_x(tamano_muestral)

datos_y <- genera_y(datos_x, beta_0, beta_1)

#Regresión
modelo_m <- lm(datos_y~datos_x)
coeficientes_muestrales <- coefficients(modelo_m)
confint(modelo_m)#Intervalo de confianza de los coeficientes


# Simulación --------------------------------------------------------------

beta_0_estimado <- beta_1_estimado <- vector()

for(i in seq_len(iteraciones)){
  muestra <- sample(seq_along(datos_x), length(datos_x), replace=TRUE)
  muestra_x <- datos_x[muestra]#Remuestreo
  muestra_y <- datos_y[muestra]#Remuestreo
  modelo <- lm(muestra_y~muestra_x)
  coeficientes <- coefficients(modelo)
  beta_0_estimado[i] <- coeficientes[1]
  beta_1_estimado[i] <- coeficientes[2]
  
}

#Tabla de intervalo bootstrapping
intervalo_bootstrapping <- data.frame(
  limite = c("LI","LS"),
  beta_0=quantile(beta_0_estimado, c(0.025,0.975)),
  beta_1=quantile(beta_1_estimado, c(0.025,0.975))
)

plot(beta_0, beta_1)
points(coeficientes_muestrales[1], coeficientes_muestrales[2])
points(beta_0_estimado, beta_1_estimado)
points(beta_0, beta_1, col=2, pch=20, cex=3)
rect(
  intervalo_bootstrapping$beta_0[1],
  intervalo_bootstrapping$beta_1[1],
  intervalo_bootstrapping$beta_0[2],
  intervalo_bootstrapping$beta_1[2],
  border=4,
  lwd=4
)
#Rectángulo del modelo muestral
# rect(
#   confint(modelo_m)[1,1],
#   confint(modelo_m)[2,1],
#   confint(modelo_m)[1,2],
#   confint(modelo_m)[2,2],
#   border=6,
#   lwd=4
# )

#Se puede observar que el parámetro está dentro del intervalo del bootstrapping
