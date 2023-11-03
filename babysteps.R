# install.packages(
#   "INLA",
#   repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
#   dep = TRUE
# )
# 
# install.packages("BiocManager")
# BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)
#https://mcmc-jags.sourceforge.io/
install.packages("rjags")


library(INLA)
Sys.setenv(JAGS_HOME="C:/Program Files/JAGS/JAGS-4.3.1")
library(rjags)

# distribuciones o modelos que GMRF

names(inla.models()$likelihood)

#info de funcion de enlace de hyperparametros y otros con 

inla.doc()

#Simulamos rapidito 

N <- 100 # 500, 5000, 25000, 100000
x <- rnorm(N, mean = 6, sd = 2)
y <- rnorm(N, mean = x, sd = 1)
data <- list(x = x, y = y, N = N)

model <- function() {
  for(i in 1:N) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta * x[i]
  }
  alpha ~ dnorm(0, 0.001)
  beta  ~ dnorm(0, 0.001)
  tau   ~ dgamma(0.01, 0.01)
}

# Ajuste con JAGS

params <- c("alpha", "beta", "tau", "mu")

rjags::jags.model(
  data = data,
  param = params,
  n.chains = 3,
  n.iter = 50000,
  n.burnin = 5000,
  model.file = model
)


# Ajuste con INLA

gau <- inla(y ~ x,
     family = "gaussian",
     data = data,
     control.predictor = list(link = 1)
)


# Exploramos un output
summary(gau)

# Simulamos rapidito para un conteo

N <- 100 # 500, 5000, 25000, 100000
x <- rnorm(N, mean = 5, sd = 1)
nu <- rnorm(N, 0, 0.1)
mu <- exp(1 + 0.5 * x + nu)
y <- rpois(N, mu)


# Con JAGS

model_p <-  function() {
  for(i in 1:N) {
    y[i] ~ dpois(mu[i])
    log(mu[i]) <- alpha + beta * x[i] + nu[i]
    nu[i] ~ dnorm(0, tau.nu)
  }
  alpha  ~ dnorm(0, 0.001)
  beta   ~ dnorm(0, 0.001)
  tau.nu ~ dgamma(0.01, 0.01)
}

params model =  function() {
  for(i in 1:N) {
    y[i] ~ dpois(mu[i])
    log(mu[i]) <- alpha + beta*x[i] + nu[i]
    nu[i] ~ dnorm(0, tau.nu)
  }
  alpha  ~ dnorm(0, 0.001)
  beta   ~ dnorm(0, 0.001)
  tau.nu ~ dgamma(0.01, 0.01)
}
params <- c("alpha", "beta", "tau.nu", "mu")

jags(
  data = data,
  param = params,
  n.chains = 3,
  n.iter = 50000,
  n.burnin = 5000,
  model.file = model
)

# Con INLA

nu <- 1:N

poi <- inla(y ~ x + f(nu, model = "iid"),
     family = "poisson",
     data = data,
     control.predictor = list(link = 1)
)


