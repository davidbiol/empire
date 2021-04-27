#Datos simulados

#Con distribución normal estándar
Y <- rnorm(100)
hist(Y)
plot(density(Y))
 
#Con distribución normal con media cinco y desviación estándar dos
Y <- rnorm(100,5,2)
hist(Y)
plot(density(Y))

#Con distribución uniforme
Y <- runif(100)
hist(Y)
plot(density(Y))

#Con distribución uniforme desde 5 hasta 10
Y <- runif(100,5,10)
hist(Y)
plot(density(Y))

#Ejemplo edad y lugar

escuela <- data.frame(
  Edad = rnorm(50, 10, 1.2),
  Lugar = "Escuela"
)

universidad <- data.frame(
  Edad = rnorm(80, 21, 2.5),
  Lugar = "Universidad"
)

edad_lugar <- rbind(escuela,universidad)
boxplot(Edad ~ Lugar, data=edad_lugar)


#Modelo lineal
X <- seq(0, 3*pi,length.out = 100)
Y <- -0.3*X + 1 + rnorm(100,0,0.5) #rnorm(100,0,0.5) es el error estándar
Z <- -0.3*X + 1 #Modelo teórico

datos_lineal <- data.frame(X,Y,Z)

plot(Y~X, data=datos_lineal)
lines(Z~X, data=datos_lineal, col="red", lwd=2) #Línea ajustada a los datos
plot(Z~X, data=datos_lineal) #Gráfico modelo teórico

#Modelo no lineal
X <- seq(0, 3*pi,length.out = 100)
Y <- cos(X)+ rnorm(100,0,0.5) #rnorm(100,0,0.5) es el error estándar
Z <- cos(X) #Modelo teórico

datos_no_lineal <- data.frame(X,Y,Z)

plot(Y~X, data=datos_no_lineal)
lines(Z~X, data=datos_no_lineal, col="red", lwd=2) #Línea ajustada a los datos
plot(Z~X, data=datos_no_lineal) #Gráfico modelo teórico

#Ejemplo de data frame
X1 <- 1000 + rnorm(300, 15, 100)
X2 <- 1000 + rnorm(300, 26, 200)
X3 <- 1000 + rnorm(300, 1500, 1000)

a <- data.frame(X1,X2,X3)
kappa(a) #Número de condición



