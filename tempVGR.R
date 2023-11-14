library(tidyverse)
library(nlme)
library(sf)
library(tmap)
library(INLA)
library(inlabru)

# Cap # Bayesian inference with INLA
# Gómez-Rubio 2021

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# Anomalias en la temperatura en grados C, en el emisferio Norte

climate <- read.table(file = "datos/moberg2005.raw.txt", header = TRUE)
summary(climate)

climate |> ggplot(aes(x = year, y = temp)) +
  geom_line() +
  xlab("Year") +
  ylab(expression('Temperature ('*degree*'C)'))


climate.ar1 <- inla(temp ~ 1 + f(year, model = "ar1"), data = climate,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
summary(climate.ar1)

climate$pred_ar_of<- climate.ar1$summary.fitted.values$mean

climate |> ggplot(aes(x = temp, y = pred_ar_of)) +
  geom_point()+
  geom_abline(slope = 1)
 
##sobreajusta los datos
#es necesario proporcionar  información previa sobre lo que esperamos 
#que sea la variación del proceso. 
#En el próximo ejemplo se utiliza una priori Gamma con parámetros 10 y 100, 
#de manera que la precisión esté centrada en 0.1 y tenga una pequeña varianza de
#0.001
#esta priori está más cerca de la escala de los datos reales que la priori predeterminada, 
#ya que la varianza de "temp" es 0.0484. 
#La priori se establece utilizando el argumento "hyper" dentro de la función "f()" 
#donde se define el efecto latente "ar1" 
#y en el argumento "control.family" (para la precisión de la verosimilitud).


climate.ar1 <- inla(temp ~ 1 + f(year, model = "ar1", 
  hyper = list(prec = list(param = c(10, 100)))), data = climate,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.family = list(hyper = list(prec = list(param = c(10, 100))))
)
summary(climate.ar1)


climate.rw1 <- inla(temp ~ 1 + f(year, model = "rw1", constr = FALSE,
    hyper = list(prec = list(param = c(10, 100)))), data = climate,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.family = list(hyper = list(prec = list(param = c(10, 100))))
)
summary(climate.rw1)

tab.ar1 <- data.frame(x = climate$year,
  y = climate.ar1$summary.fitted.values[, "mean"],
  ll95 = climate.ar1$summary.fitted.values[, "0.025quant"],
  ul95 = climate.ar1$summary.fitted.values[, "0.975quant"])
tab.rw1 <- data.frame(x = climate$year,
  y = climate.rw1$summary.fitted.values[, "mean"],
  ll95 = climate.rw1$summary.fitted.values[, "0.025quant"],
  ul95 = climate.rw1$summary.fitted.values[, "0.975quant"])


p1 <- ggplot(climate, aes(x = year, y = temp)) +
  geom_point(shape = 20, alpha = 0.3) +
  xlab("Year") +
  ylab(expression('Temperature ('*degree*'C)')) +
  geom_line(aes(x = x, y = y), linetype = 1, col = "blue", data = tab.ar1) +
  geom_ribbon(aes(x = x, y = y, ymin = ll95, ymax = ul95), alpha = 0.75, data = tab.ar1) +
  ggtitle ("Autoregressive model of order 1")

p2 <- ggplot(climate, aes(x = year, y = temp)) +
  geom_point(shape = 20, alpha = 0.3) +
  xlab("Year") +
  ylab(expression('Temperature ('*degree*'C)')) +
  geom_line(aes(x = x, y = y), linetype = 1, col = "red", data = tab.rw1) +
  geom_ribbon(aes(x = x, y = y, ymin = ll95, ymax = ul95), alpha = 0.5, data = tab.rw1) +
  ggtitle("Random walk of order 1")

library("gridExtra")
grid.arrange(p1, p2, nrow = 2)


#Para ambos modelos se han calculado el AIC y WAIC, 
#y claramente favorecen al modelo ar1 (rw1 tmb sobreajusta los datos). 
# Veamos CPO y comparemos.


# ar1
- sum(log(climate.ar1$cpo$cpo))

# rw1
- sum(log(climate.rw1$cpo$cpo))


# Terremotos para datos no normales 
# contiene el número de terremotos grandes 
#(de magnitud 7 o mayor) en el mundo desde 1900 hasta 2006

load(file ="datos/earthquake.rda" )

#Add year
earthquake$year <- 1900:2006

#Summary of the data
summary(earthquake)


#"earthquake"

earthquake |> ggplot(aes(x = year, y = number)) + 
  geom_line() + xlab("Year") + ylab("Number of major earthquakes")

quake.ar1 <- inla(number ~ 1 + f(year, model = "ar1"), data = earthquake,
  family = "poisson", control.predictor = list(compute = TRUE))
summary(quake.ar1)

quake.rw1 <- inla(number ~ 1 + f(year, model = "rw1"), data = earthquake,
  family = "poisson", control.predictor = list(compute = TRUE))
summary(quake.rw1)

tab.ar1 <- data.frame(x = earthquake$year,
  y = quake.ar1$summary.fitted.values[, "mean"],
  ll95 = quake.ar1$summary.fitted.values[, "0.025quant"],
  ul95 = quake.ar1$summary.fitted.values[, "0.975quant"])
tab.rw1 <- data.frame(x = earthquake$year,
  y = quake.rw1$summary.fitted.values[, "mean"],
  ll95 = quake.rw1$summary.fitted.values[, "0.025quant"],
  ul95 = quake.rw1$summary.fitted.values[, "0.975quant"])


p1 <- ggplot(earthquake, aes(x = year, y = number)) +
  geom_point(shape = 20, alpha = 0.3) +
  xlab("Year") +
  ylab("Number of major earthquakes")+
  geom_line(aes(x = x, y = y), linetype = 1, col = "blue", data = tab.ar1) +
  geom_ribbon(aes(x = x, y = y, ymin = ll95, ymax = ul95), alpha = 0.75, data = tab.ar1) +
  ggtitle("Autoregressive model of order 1")

p2 <- ggplot(earthquake, aes(x = year, y = number)) +
  geom_point(shape = 20, alpha = 0.3) +
  xlab("Year") +
  ylab("Number of major earthquakes")+
  geom_line(aes(x = x, y = y), linetype = 1, col = "red", data = tab.rw1) +
  geom_ribbon(aes(x = x, y = y, ymin = ll95, ymax = ul95), alpha = 0.75, data = tab.rw1) +
  ggtitle("Random walk of order 1")


library("gridExtra")
grid.arrange(p1, p2, nrow = 2)

# Pronostico :)
quake.pred <- rbind(earthquake, 
  data.frame(number = rep(NA, 14), year = 2007:2020))


quake.ar1.pred <- inla(number ~ 1 + f(year, model = "ar1"), data = quake.pred,
  family = "poisson", control.predictor = list(compute = TRUE, link = 1))
summary(quake.ar1.pred)


tab.ar1 <- data.frame(x = quake.pred$year,
  y = quake.ar1.pred$summary.fitted.values[, "mean"],
  ll95 = quake.ar1.pred$summary.fitted.values[, "0.025quant"],
  ul95 = quake.ar1.pred$summary.fitted.values[, "0.975quant"])

tab.labels <- data.frame(
  x = c(1953, 2015),
  y = c(40, 40),
  text = c("observed", "predicted")
)

ggplot(earthquake, aes(x = year, y = number)) +
  geom_point(shape = 20, alpha = 0.3) +
  xlab("Year") +
  ylab("Number of major earthquakes")+
  geom_line(aes(x = x, y = y), linetype = 1, col = "blue", data = tab.ar1) +
  geom_ribbon(aes(x = x, y = y, ymin = ll95, ymax = ul95), alpha = 0.75, data = tab.ar1) +
  geom_ribbon(aes(x = x, y = y, ymin = ll95, ymax = ul95), alpha = 0.5, data = tab.ar1[-(1:nrow(earthquake)), ]) +
  geom_vline(xintercept = 2007, linetype = "dashed", colour = "red") +
  geom_text(data = tab.labels, aes(x = x, y = y, label = text))
  


library("KFAS")
data(alcohol)

summary(alcohol)


# los datos deben organizarse en una matriz de dos columnas, 
# ya que el modelo contendrá dos verosimilitudes, 
# En la primera columna, incluiremos el número de muertes 
#en el grupo de edad de 30 a 39 años, y en la segunda columna las "observaciones falsas cero". 

n <- nrow(alcohol) - 1 #There is an NA
Y <- matrix(NA, ncol = 2, nrow = n + (n - 1))
Y[1:n, 1] <- alcohol[1:n, 1]
Y[-c(1:n), 2] <- 0

# offset
oset <- c(alcohol[1:n, 5], rep(NA, n - 1))

# se deben crear una serie de índices y pesos para el efecto latente, 
# los cuales se describen en la formulación del modelo  
# todos los índices tienen una longitud de n+n+1
# porque modelo estará compuesto por dos verosimilitudes. 
# La primera es una verosimilitud de Poisson para modelar la respuesta y 
# la segunda es una verosimilitud gaussiana para modelar los "falsos ceros" 
# latentes que definen la variable x_t
# Cuando un índice solo afecta a una verosimilitud, 
# entonces los valores correspondientes para la otra verosimilitud se establecerán como NA.


#x_t
i <- c(1:n, 2:n)
#x_(t-1) 2:n
j <- c(rep(NA, n), 1:(n - 1))
# Weight to have -1 * x_(t-1)
w1 <- c(rep(NA, n), rep(-1, n - 1))
#x_(t-1), 2:n
l <- c(rep(NA, n), 2:n)
# Weight to have  * omega_(t-1)
w2 <- c(rep(NA, n), rep(-1, n - 1))


prec.prior <- list(prec = list(param = c(0.001, 0.001)))

alc.inla <- inla(Y ~ 0 + offset(log(oset)) +
    f(i, model = "iid",
      hyper = list(prec = list(initial = -10, fixed = TRUE))) +
    f(j, w1, copy = "i") + f(l, w2, model = "iid"),
  data = list(Y = Y, oset = oset), family = c("poisson", "gaussian"),
  control.family = list(list(), 
    list(hyper = list(prec = list(initial = 10, fixed = TRUE)))),
  control.predictor = list(compute = TRUE)
)

summary(alc.inla)

tab.alc <- data.frame(x = 1969:2012,
  deaths = alcohol[1:n, 1],
  pred = alc.inla$summary.fitted[1:n, "mean"], 
  ll95 = alc.inla$summary.fitted.values[1:n, "0.025quant"],
  ul95 = alc.inla$summary.fitted.values[1:n, "0.975quant"])

ggplot(tab.alc, aes(x = x, y = deaths)) +
  geom_point(shape = 20, alpha = 0.3) +
  xlab("Year") +
  ylab("Number of deaths")+
  geom_line(aes(x = x, y = pred), linetype = 1, col = "blue") +
  geom_ribbon(aes(x = x, y = pred, ymin = ll95, ymax = ul95), alpha = 0.75)

