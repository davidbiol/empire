library(glmnet)
library(matlib)

#Algoritmo datos faltantes

View(iris)
## NA RANDOM
irisa <- iris[,1:4] 
i=1
n=1
while(i<=n){
  r = sample(1:nrow(irisa), 1)
  c = sample(1:ncol(irisa), 1)
  irisa[r,c] = NA
  i=i+1
}
View(irisa)

#Ejemplo tabla1.5
x1 <- c(2, NA, 1, 3)
x2 <- c(3, 3, 3, 1)
x3 <- c(1, 5, 1, 2)
x4 <- c(1, 3, NA, 3)
a <- data.frame(x1,x2,x3,x4)

a


#Numero de datos faltantes
cantidad.df <- function(x){
  length(which(is.na(x)==TRUE))
}
cantidad.df((irisa))




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
determinar.rp.df <- function(x, i=1, p=1, e=c()){
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
  
  ##Aplicar Ridge al principio
  ridge.est <- function(v){
    c.t <- x[complete.cases(x),]
    Y = as.matrix(c.t[e[v,2]])
    if (length(x[e[v,1],][is.na(x[e[v,1],])])!=1){
      X = as.matrix(cbind(c(rep(1, nrow(Y))), c.t[-c(e[v,2], c(which(is.na(x[e[v,1],]))))]))
    }
    else {
      X = as.matrix(cbind(c(rep(1, nrow(Y))), c.t[-(e[v,2])]))
    }
    
    XtX <- t(X)%*%X  #Se calcula la transpuesta de X por X
    
    
    #Regularización de Ridge
    set.seed(1)
    cv.out=cv.glmnet(X,Y,alpha=0)
    bestlam=cv.out$lambda.min
    
    B <- solve(XtX + bestlam*diag(1,nrow(XtX)))%*%t(X)%*%Y #Calcular los coeficientes con Lambda de Ridge
    
    regresion <- function(b1,b2){   #Función para generalizar la regresión y modularizar el código
      B[b1]*x[e[v,1],b2]
    }
    
    e2 <- B[1] #El valor obtenido por la regresión
    b=2
    for (c in seq(1:ncol(x))){ #Esto es para omitir el valor de la columna diana, es decir, reemplazar en las demás columnas menos en la que está el NA
      if (all(c != c(e[v,2],c(which(is.na(x[e[v,1],])))))){
        e2 = e2 + regresion(b,c) #El último c puede cambiar de valor dependiendo de donde se encuentre el dato faltante
        b=b+1
        
      }
    }
    #Incluir a e2 en el Global Env
    assign("e2",e2,.GlobalEnv)
  }
  
  
  
  #Matriz con la regularización de Ridge  
  
  mvr = c()
  
  g=1
  for (df in seq(1:nrow(e))){
    ridge.est(df)
    mvr[g] <- e2
    g=g+1
  }
  print("Con regularización de Ridge")
  
  
  for(t in seq(1:nrow(e))){
    x[e[t,1],e[t,2]] <- mvr[t]
    
  }
  print(mvr)
  matrix_ridge_x <- x
  
  
  estimate.df <- function(v){
    c.t <- x[complete.cases(x),]
    Y = as.matrix(c.t[e[v,2]])
    if (length(x[e[v,1],][is.na(x[e[v,1],])])!=1){
      X = as.matrix(cbind(c(rep(1, nrow(Y))), c.t[-c(e[v,2], c(which(is.na(x[e[v,1],]))))]))
    }
    else {
      X = as.matrix(cbind(c(rep(1, nrow(Y))), c.t[-(e[v,2])]))
    }
    
    
    XtX <- t(X)%*%X  #Se calcula la transpuesta de X por X
    #  if(det(XtX)<=1.19464e-18){ 
    #    XtX.1 <- Ginv(XtX) #Aplicar la inversa generalizada de Moore Penrose
    #  } else{
    #    XtX.1 <- solve(XtX) #Se calcula la inversa de XtX 
    #    }
    XtX.1 <- solve(XtX)
    
    
    B <- XtX.1%*%t(X)%*%Y #Calcular los coeficientes
    
    regresion <- function(b1,b2){   #Función para generalizar la regresión y modularizar el código
      
      B[b1]*x[e[v,1],b2]
    }
    
    
    e1 <- B[1] #El valor obtenido por la regresión
    b=2
    for (c in seq(1:ncol(x))){ #Esto es para omitir el valor de la columna diana, es decir, reemplazar en las demás columnas menos en la que está el NA
      if (all(c != c(e[v,2],c(which(is.na(x[e[v,1],])))))){ #AQUI VOY, REVISAR TODO FALSE
        e1 = e1 + regresion(b,c) #El último c puede cambiar de valor dependiendo de donde se encuentre el dato faltante
        b=b+1
      }
    }
    
    #Incluir a e1 en el Global Env
    assign("e1",e1,.GlobalEnv)
    
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
    ifelse(all((lapply(conv, abs) < 1e-8)==TRUE), break, next)
    
    
    
  }
  
  #datos nuevos está con Ridge
  #elementos a la función
  my_list <- list("posicion" = e, "valest" = mvn, "valestRidge" = mvr ,"datosnuevos" = x, "datosridge" = matrix_ridge_x)
  
  
}

est.rp <- determinar.rp.df(a)
est.rp
summary(est)
est.rp$posicion
est.rp$valest
est.rp$valestRidge
est.rp$datosnuevos


