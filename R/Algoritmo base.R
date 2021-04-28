library(matlib)
library(glmnet)
#Algoritmo datos faltantes


## NA RANDOM
irisa <- iris[,1:4]
i=1
n=10
while(i<=n){
  r = sample(1:nrow(irisa), 1)
  c = sample(1:ncol(irisa), 1)
  irisa[r,c] = NA
  i=i+1
}

#Ejemplo tabla1.5
x1 <- c(1.30, 1.40, NA, 1.54, 1.30, NA, 1.50, 1.60, 1.41, 1.34)
x2 <- c(51, 60, 69, 73, 56, 75, 80, NA, 58, 70)
x3 <- c(4.8, NA, 5.8, 6.5, 5.3, 7.0, 8.1, 7.8, 5.9, 6.1)
x4 <- c(115, 130, 138, 148, 122, 152, 160, 155, 135, 140)
a <- data.frame(x1,x2,x3,x4)


## RECTANGULAR
x1 <- c(1, 5, 8, 7)
x2 <- c(4, 7, 8, 6)
x3 <- c(5, NA, 8, 7)
x4 <- c(2, 4, 8, 2)
a1 <- data.frame(x1,x2,x3,x4)

#Numero de datos faltantes
cantidad.df <- function(x){
  length(which(is.na(x)==TRUE))
}



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



#Determinar datos faltantes
determinar.df <- function(x, i=1, p=1, e=c()){
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


  estimate.df <- function(v){
    c.t <- x[complete.cases(x),]
    Y = as.matrix(c.t[e[v,2]])
    X = as.matrix(cbind(c(rep(1, nrow(Y))), c.t[-(e[v,2])]))
    XtX <- t(X)%*%X
    XtX.1 <- solve(XtX)
    B <- XtX.1%*%t(X)%*%Y #Calcular los coeficientes

    regresion <- function(b1,b2){   #Función para generalizar la regresión y modularizar el código
      B[b1]*x[e[v,1],b2]
    }


    e1 <- B[1] #El valor obtenido por la regresión
    b=2
    for (c in seq(1:ncol(x))){ #Esto es para omitir el valor de la columna diana, es decir, reemplazar en las demás columnas menos en la que está el NA
      if (c != e[v,2]){
        e1 = e1 + regresion(b,c) #El último c puede cambiar de valor dependiendo de donde se encuentre el dato faltante
        b=b+1
      }
    }

    #Incluir a e1 en el Global Env
    assign("e1",e1,.GlobalEnv)

    #print(x)
  }

  #Iterar los estimadores
  repeat{
    #mvo es el missing value old
    mvo = c()
    g = 1
    #Correr la función estimate.df() en cada NA
    for (df in seq(1:nrow(e))){
      estimate.df(df)
      mvo[g] <- e1
      g=g+1
    }
    print(mvo)

    for(t in seq(1:nrow(e))){
      x[e[t,1],e[t,2]] <- mvo[t]
    }


    #mvn es el missing value new
    mvn = c()
    g=1

    for (df in seq(1:nrow(e))){
      estimate.df(df)
      mvn[g] <- e1
      g=g+1
    }

    print(mvn)

    for(t in seq(1:nrow(e))){
      x[e[t,1],e[t,2]] <- mvn[t]
    }


    #Test de salida
    conv <- mvo-mvn
    print(conv)
    ifelse(all((lapply(conv, abs) < 1e-12)==TRUE), break, next)



  }



  #elementos a la función
  my_list <- list("posicion" = e, "valest" = mvn, "datosnuevos" = x)


}
x1 <- c(2, NA, 1, 3)
x2 <- c(3, 1, 2, 1)
x3 <- c(1, 5, 1, 2)
x4 <- c(1, 3, NA, 5)
a <- data.frame(x1,x2,x3,x4)


# est <- determinar.df(irisa)
#
#
# summary(est)
# est$posicion
# est$valest
# est$datosnuevos

##Valores reales
# i=1
# valreal = c()
# while(i<=nrow(est$posicion)){
#   r <- est$posicion[i,1]
#   c <- est$posicion[i,2]
#   valreal[i] <- iris[r,c]
#   i=i+1
# }


# ##Validación por menor error relativo
#
# err.alg <- round((abs(est$valest-valreal)/valreal),6)
# err.mean <- round((abs(est.mean$valest-valreal)/valreal),6)
# err.median <- round((abs(est.median$valest-valreal)/valreal),6)
# err.con.ridge <- round((abs(est.r$valestRidge-valreal)/valreal),6)
#
# validationData <- data.frame(est$valest, est.mean$valest, est.median$valest, est.r$valestRidge, valreal, err.alg, err.mean, err.median,err.con.ridge)
# validationData
# Best <- c()
# for(i in seq(1:nrow(validationData))){
#   for(j in c(6:9)){
#     if(validationData[i,j]==min(validationData[i,6:9])){
#       Best[i] <- colnames(validationData[j])
#     }
#   }
# }
# validationData = cbind(validationData, Best)
# validationData
# par(mfrow=c(1,3))
# pie(prop.table(table(validationData$Best)))
#
# #Utilizar chi cuadrado para observar si es mayor significativamente
