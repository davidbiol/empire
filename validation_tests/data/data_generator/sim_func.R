#Kernel

#Distribución uniforme

tamano_muestral <- 700
a <- 3 #Mínimo poblacional
b <- 8 #Máximo poblacional
iteraciones <- 100

x <- seq(2,9, length.out = 100)

Y <- runif(tamano_muestral, a, b)

#Estimador Kernel

estimador_kernel <- density(Y)
plot(estimador_kernel)
lines(x, dunif(x,a,b), col=2, lwd=2) #Densidad teórica

#Simulación

plot(estimador_kernel)

for(i in seq_len(iteraciones)){
  Y <- runif(tamano_muestral, a, b)
  estimador_kernel <- density(Y)
  estimador_kernel <- density(Y)
  lines(estimador_kernel) #Para graficar encima y no hacer un gráfico nuevo
}
lines(x, dunif(x,a,b), col=2, lwd=2)

##El estimador Kernel es un estimador sesgado.

#---------------------------------------------------
#ECDF - Función de densidad acumulada empírica

#Distribución uniforme
tamano_muestral <- 70
a <- 3 #Mínimo poblacional
b <- 8 #Máximo poblacional
iteraciones <- 100

x <- seq(2,9, length.out = 100)

Y <- runif(tamano_muestral, a, b)

estimador_ecdf <- ecdf(Y)
plot(estimador_ecdf, pch="", verticals=TRUE)
lines(x, punif(x,a,b), col=2, lwd=2) #Densidad acumulada teórica

#Simulación
plot(estimador_ecdf, pch="", verticals=TRUE)

for(i in seq_len(iteraciones)){
  Y <- runif(tamano_muestral, a, b)
  estimador_ecdf <- ecdf(Y)
  estimador_ecdf <- ecdf(Y)
  lines(estimador_ecdf, pch="", verticals=TRUE) #Para graficar encima y no hacer un gráfico nuevo
}
lines(x, punif(x,a,b), col=2, lwd=2)

#El estimador ECDF es insesgado, ya que el parámetro siempre está dentro de la estimación
