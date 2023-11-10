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

inla.doc("zeroinflatedpoisson")

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
     data = data
)


# Exploramos un output
summary(gau)

plot(gau)

# BI_INLA_Cap2 ----
# Gómez-Rubio, Virgilio (2020). 
# Bayesian Inference with INLA. Chapman & Hall/CRC Press. Boca Raton, FL.
# https://becarioprecario.bitbucket.io/inla-gitbook/index.html

library("MASS")
summary(cement)

GGally::ggpairs(cement)

m1_lm <- lm(log(y) ~ x1 + x2 + x3 + x4, data = cement)
summary(m1_lm)


m1 <- inla(y ~ x1 + x2 + x3 + x4, data = cement
           ,control.compute = list(config = TRUE, cpo=TRUE))
summary(m1)


m1$summary.fitted.values

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


# ts_Thimothy E.Moore ----
# https://tem11010.github.io/timeseries-inla/

#el conjunto de datos 'greatLakes' del paquete DAAG. 
#Se trata de un conjunto de datos de promedios anuales del nivel del agua
#de los Grandes Lagos (Erie, Míchigan/Huron, Ontario y St. Clair) 
#desde 1918 hasta 2009. Las obs se almacenan como una serie temporal multivariada.

require(INLA)
library(DAAG)

lakes <- greatLakes

head(greatLakes)

plot(lakes)

class(lakes)

lakes.df <- data.frame(as.matrix(lakes), year = time(lakes))

values <- lakes.df["Erie"]

plot(lakes_decomp)

#Debido a que tenemos datos de series temporales 
#a intervalos regulares, podemos utilizar un modelo 
#de caminata aleatoria de orden 2 (RW2). 
# Según Zuur et al., estos modelos generan una tendencia más suavizada RW1.

i1 <-
  inla(
    Erie ~ f(year, model = "rw2"),
    control.compute = list(dic = TRUE),
    verbose = TRUE,
    family = "gaussian", data = lakes.df)

lakes.df.pred <- bind_cols(lakes.df,
                           as.data.frame((i1$summary.fitted.values)))

lakes.df.pred |> ggplot(aes(x=year,y=Erie))+
  geom_point()+
  geom_line(aes(x = year, y = mean))+
  geom_ribbon(aes(x = year, ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.2)+
  xlab("Year")+
  ylab("Lake Height")+
  theme_bw()




