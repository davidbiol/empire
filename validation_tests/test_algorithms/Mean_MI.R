
#Algoritmo datos faltantes
#MEDIA


#Ejemplo tabla1.5
x1 <- c(NA, 1.40, 1.42, 1.54, 1.30, 1.55, 1.50, 1.60, 1.41, 1.34)
x2 <- c(51, 60, 69, 73, 56, 75, 80, 76, 58, 70)
x3 <- c(4.8, NA, 5.8, 6.5, 5.3, 7.0, 8.1, 7.8, 5.9, 6.1)
x4 <- c(115, 130, 138, 148, 122, 152, 160, 155, 135, 140)
a <- data.frame(x1,x2,x3,x4)
a


#Numero de datos faltantes
cantidad.df <- function(x){
  length(which(is.na(x)==TRUE))
}
cantidad.df((a))




#Funcion para ver posicion de NA
posicion.df <- function(x, i=1, p=1, e=c()){
  while(i<=ncol(x)){
    v <- x[i]
    v <- is.na(v)
    w <- which(v==TRUE)
    for(m in w){
      e[p] <- m #Para almacenar las filas
      e[p+1] <- i #Para almacenar las columnas
      p = p+2
    }
    i= i+1
  }
  e <- data.frame(row=e[seq(1,(p-1),2)], col=e[seq(2,(p-1),2)]) #data.frame con col y row
  print(e)
}
posicion.df(a)


#Determinar datos faltantes
determinar.mean.df <- function(x, i=1, p=1, e=c()){
  print(x)
  

  while(i<=ncol(x)){
    v <- x[i]
    v <- is.na(v)
    w <- which(v==TRUE)
    for(m in w){
      e[p] <- m #Para almacenar las filas
      e[p+1] <- i #Para almacenar las columnas
      p = p+2
    }
    i= i+1
  }
  e <- data.frame(row=e[seq(1,(p-1),2)], col=e[seq(2,(p-1),2)]) #data.frame con col y row

  mvn = c()
  xc = x #Para que todos los NA en la misma columna obtengan el mismo valor
for(t in seq(1:nrow(e))){
  xc[e[t,1],e[t,2]] <- mean(x[,e[t,2]], na.rm=TRUE)
  mvn[t] <- xc[e[t,1],e[t,2]]
}

  
  
  
  #elementos a la función
  my_list <- list("posicion" = e, "valest" = mvn, "datosnuevos" = xc)
  
  
}


est.mean <- determinar.mean.df(irisa)
est.mean
summary(est.mean)
est.mean$posicion
est.mean$valest
est.mean$datosnuevos











