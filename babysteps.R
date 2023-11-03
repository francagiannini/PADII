library(tidyverse)
theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# Instalamos INLA ----

# install.packages(
#   "INLA",
#   repos = c(getOption("repos"), 
#   INLA = "https://inla.r-inla-download.org/R/stable"),
#   dep = TRUE
# )

# install.packages("BiocManager")
# BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

library(INLA)

# distribuciones o modelos que GMRF

names(inla.models()$likelihood)

#info de funcion de enlace de hyperparametros y otros con 

inla.doc()

# Simulamos rapidito ----

N <- 500
x <- rnorm(N, mean = 6, sd = 2)
y <- rnorm(N, mean = x, sd = 1)
data <- as.data.frame(bind_cols(x = x, y = y))

# Ajuste con lm

gau_lm <- lm(y ~ x,
             data = data)

summary(gau_lm)

# Ajuste con INLA

gau <- inla(y ~ x,
     family = "gaussian",
     data = data#,
     #control.predictor = list(link = 1)
)


# Exploramos un output
summary(gau)

plot(gau)

# Gómez-Rubio, Virgilio (2020). 
# Bayesian Inference with INLA. Chapman & Hall/CRC Press. Boca Raton, FL.
# https://becarioprecario.bitbucket.io/inla-gitbook/index.html

library("MASS")
summary(cement)

GGally::ggpairs(cement)

m1_lm <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
summary(m1_lm)

m1 <- inla(y ~ x1 + x2 + x3 + x4, data = cement
           ,control.compute = list(config = TRUE))

summary(m1)

m1$cpo

m1.samp <- inla.posterior.sample(100, m1)

names(m1.samp[[1]])

m1.samp[[1]]

x1x2.samp <- inla.posterior.sample.eval(function(...) {x1 * x2},
                                        m1.samp)

summary(as.vector(x1x2.samp))

prec.samp <- inla.hyperpar.sample(1000, m1)

tab <- data.frame(prec = prec.samp[, 1])

ggplot(tab, aes(x = prec)) +
  geom_histogram(aes(y=..density..), bins = 18) +
  geom_line(data = as.data.frame(m1$marginals.hyperpar[[1]]),
            aes(x = x, y = y)) + 
  xlab(expression(tau)) +
  ylab(expression(paste(pi, "(", tau, " | ", bold(y), ")"))) +
  xlim(0, 0.8)








