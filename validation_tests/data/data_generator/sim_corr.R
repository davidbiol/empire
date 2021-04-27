# Simular datos con variables correlacionadas --------------------------
library(faux)
dat <- rnorm_multi(n = 100,
                   mu = c(20, 20, 20),
                   sd = c(1, 5, 5),
                   r = c(0.5, 0.6, 0.99),
                   varnames = c("A", "B", "C"),
                   empirical = TRUE)

#Empirical=TRUE es para que los datos sean la poblacion, es decir, el promedio, la sd y el r son exactos.

cor(dat)
# Para saber si los r son permitidos: -------------------------------------

r = c(.2, .3, .4, .2,
      .3, -.1, .2,
      .4, .5,
      .3)

r %>% cormat_from_triangle() %>% is_pos_def()
