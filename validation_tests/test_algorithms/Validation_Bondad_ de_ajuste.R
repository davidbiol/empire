#Librerias
library(data.table)
library(impute)
library(Amelia)


wine <- read.csv("C:/Users/david/Documents/Algoritmo_datos_faltantes/wine (1).data", header=FALSE)
wine1 <- wine[,-1]
View(wine)
sort(colMeans(abs(cor(wine1))))
par(mfrow=c(1,1))

#Para Graficar boxplots de cada uno de los algoritmos en todas las variables simultaneamente
for (q in c(1:7)){
  boxplot(c(as.numeric(dv_v8[[q]])),as.numeric(dv_v7[[q]]),as.numeric(dv_v13[[q]]),
          as.numeric(dv_v14[[q]]),as.numeric(dv_v12[[q]]),as.numeric(dv_v9[[q]]),
          as.numeric(dv_v10[[q]]),as.numeric(dv_v5[[q]]),as.numeric(dv_v3[[q]]),
          as.numeric(dv_v2[[q]]),as.numeric(dv_v11[[q]]),as.numeric(dv_v6[[q]]),
          as.numeric(dv_v4[[q]]), main = paste("Grafica", colnames(dv_v8)[q]))
}


# Imputar NA
v=1
while(v<=10){
w=1
data_validation <- data.frame("Time differences in secs" = double(), "err.alg.r" = double(),"err.median" = double(), 
                              "err.ridge.p" = double(), "err.alg"= double(), "err.mean" = double(), "err.ridge.u" = double(), 
                              "err.knn" = double(), "err.emb" = double())

while(w<=10){
  h = 1
  while(h<2){
    start_time <- Sys.time()
    best.alg.total <- c()
    i=1
    while(i<=100){
      wine1 <- wine[,-1] #Creo que esto no lo hice con iris
      set.seed(Sys.time()) #Unique set.seed()
      t=1
      n=50
      while(t<=n){
        r = sample(1:nrow(wine1), 1)
        #c = sample(1:ncol(irisa), 1)
        c=v
        if(is.na(wine1[r,c])==FALSE){
          wine1[r,c] = NA
          t=t+1
        }
      }
      # set.seed(Sys.time())
      # t=1
      # n=1
      # #Ingresar NA a una segunda columna
      # while(t<=n){
      #   r = sample(1:nrow(irisa), 1)
      #   #c = sample(1:ncol(irisa), 1)
      #   c=3
      #   if(is.na(irisa[r,c])==FALSE){
      #     irisa[r,c] = NA
      #     t=t+1
      #   }
      # }
      
      #Estimador
      knn.imputed <- impute.knn(as.matrix(wine1))
      amelia.imputed <- amelia(wine1)
      est.ru <- determinar.ru.df(wine1)
      est.mean <- determinar.mean.df(wine1)
      est.median <- determinar.median.df(wine1)
      est.rp <- determinar.rp.df(wine1)

      
      ##Obtener Valores reales y Obtener valor imputado por knn
      j=1
      valreal = c()
      knn.iv = c() #Imputed values by knn
      emb.iv = c() #Imputed values by emb
      while(j<=nrow(est.ru$posicion)){
        r <- est.ru$posicion[j,1]
        c <- est.ru$posicion[j,2]
        valreal[j] <- wine[,-1][r,c]
        #knn
        knn.iv[j] <- knn.imputed$data[est.ru$posicion[j,1],est.ru$posicion[j,2]]

        #emb - Amelia
        emb.iv[j] <- amelia.imputed$imputations[[1]][est.ru$posicion[j,1],est.ru$posicion[j,2]]
        j=j+1
      }
      #Numero de condición
      
      # kappa(est.ru$datosnuevos)
      # kappa(est.mean$datosnuevos)
      # kappa(est.median$datosnuevos)
      # kappa(est.ru$datosridge)
      # kappa(est.rp$datosnuevos)
      # kappa(est.rp$datosridge)
      # kappa(iris[1:4])
      


      
      ##Validación por menor error relativo
      
      err.alg <- round((abs(est.ru$valest-valreal)/valreal),6)
      err.mean <- round((abs(est.mean$valest-valreal)/valreal),6)
      err.median <- round((abs(est.median$valest-valreal)/valreal),6)
      err.ridge.u <- round((abs(est.ru$valestRidge-valreal)/valreal),6)
      err.alg.r <- round((abs(est.rp$valest-valreal)/valreal),6)
      err.ridge.p <- round((abs(est.rp$valestRidge-valreal)/valreal),6)
      err.knn <- round((abs(knn.iv-valreal)/valreal),6)
      err.emb <- round((abs(emb.iv-valreal)/valreal),6)
      
      validationData <- data.frame(est.ru$valest, est.mean$valest, est.median$valest, est.ru$valestRidge, est.rp$valest, est.rp$valestRidge, 
                                   knn.iv, emb.iv, valreal, err.alg, err.mean, err.median,err.ridge.u, err.alg.r, err.ridge.p, err.knn, err.emb)
      Best <- c()
      for(k in seq(1:nrow(validationData))){
        for(j in c(10:17)){
          if(validationData[k,j]==min(validationData[k,10:17])){
            Best[k] <- colnames(validationData[j])
          }
        }
      }
      validationData = cbind(validationData, Best)
      
      # utilizar chi cuadrado para observar si es mayor significativamente
      if (length(table(validationData$Best))==1){
        best.alg.total[i] <- names(table(Best))
      }
      else {
        if (chisq.test(table(validationData$Best))$p.value < 0.05){
          best.alg <- names(which.max(table(validationData$Best)))
          best.alg.total[i] <- best.alg
        }
      }
      i=i+1
    }
    end_time <- Sys.time()
    time_val <- as.numeric(end_time - start_time)
    plot(table(best.alg.total), main = w)
    h=h+1
    data_validation <- rbindlist(list(data_validation, as.list(c("Time.differences.in.secs" = time_val, table(best.alg.total)))), fill=TRUE)
  }
  w=w+1
}
#View(data_validation)
#save(data_validation, file=paste("C:/Users/david/Documents/Algoritmo_datos_faltantes/data_validation_wine_v",v+1,"_1.Rda", sep=""))
#summary(data_validation)
boxplot(data_validation[,-1])
v=v+1
}


