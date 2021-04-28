#--------------------------------------------------------------------------

##Simulando intervalos de confianza

#Intervalos de confianza de la media

tamano_muestral <- 35
iteraciones <- 100
media_poblacional_A <- 5
media_poblacional_B <- 3
desv_est_poblacional <- 2

plot(media_poblacional_A, media_poblaciona_B, xlim=c(0,7), ylim=c(0,5))

for(i in seq_len(iteraciones)){
  muestra_A <- rnorm(tamano_muestral, media_poblacional_A, desv_est_poblacional)
  t_test_A <- t.test(muestra_A)
  intervalo_A <- t_test_A$conf.int
  LI_A <- min(intervalo_A)
  LS_A <- max(intervalo_A)
  
  muestra_B <- rnorm(tamano_muestral, media_poblacional_B, desv_est_poblacional)
  t_test_B <- t.test(muestra_B)
  intervalo_B <- t_test_B$conf.int
  LI_B <- min(intervalo_B)
  LS_B <- max(intervalo_B)
  
  rect(LI_A, LI_B, LS_A, LS_B)
}

abline(0,1,col=2)

