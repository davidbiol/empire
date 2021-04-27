#Algoritmo datos faltantes


#Ejemplo tabla1.5
x1 <- c(1.30, 1.40, NA, 1.54, 1.30, NA, 1.50, 1.60, 1.41, 1.34)
x2 <- c(51, 60, 69, 73, 56, 75, 80, NA, 58, 70)
x3 <- c(4.8, NA, 5.8, 6.5, 5.3, 7.0, 8.1, 7.8, 5.9, 6.1)
x4 <- c(115, 130, 138, 148, 122, 152, 160, 155, 135, 140)
a <- data.frame(x1,x2,x3,x4)
a

#Determinar datos faltantes
determinar.df <- function(x, i=1, p=1, e=c()){
  print(x)
  
  
  #d <- c() #Para el data.frame que calcula los decimales
  #D <- c() #El vector con los decimales mayores de cada variables
  
  
  #  for(k in a){ #Para determinar numero de decimales
  #    for(j in k){
  #      j<-gsub("(.*)(\\.)|([0]*$)","",j)
  #      d[q] <- nchar(j)
  #      q=q+1
  #    }
  #  }
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
  
  #  d <- data.frame(matrix(d, ncol=ncol(x))) #Hacer un data.frame con el número de decimales
  #  for(o in d){
  #    D[r] <- max(o, na.rm = TRUE)
  #    r=r+1
  #  }
  
  
  estimate.df <- function(v){
    c.t <- x[complete.cases(x),]
    Y = as.matrix(c.t[e[v,2]])
    X = as.matrix(c.t[-e[v,2]])
    df = as.data.frame(cbind(Y,X))
    df
    B <- lm(df, data=x)
    prtors <- c()
    t = 1
    for (c in seq(1:ncol(x))){
      if (c != e[v,2]){
        prtors[t] = x[e[v,1],c]
        t = t+1
      }
    }
    prtors = data.frame(prtors)
    e1 = predict(B, newdata=prtors)
    e1 = as.numeric(e1[1])
    print(e1)
    
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
    ifelse(all((lapply(conv, abs) < 1e-6)==TRUE), break, next)
    
    
    
  }
  
  
  
  #elementos a la función
  my_list <- list("posicion" = e, "valest" = mvn, "datosnuevos" = x)
  
  
}

est <- determinar.df(a)
est
summary(est)
est$posicion
est$valest
est$datosnuevos
