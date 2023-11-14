library(nlme)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(INLA)
library(inlabru)

climate <- read.table(file = "data/moberg2005.raw.txt", header = TRUE)
summary(climate)

library("ggplot2")
ggplot(climate, aes(x = year, y = temp)) +
  geom_line() +
  xlab("Year") +
  ylab(expression('Temperature ('*degree*'C)'))


climate.ar1 <- inla(temp ~ 1 + f(year, model = "ar1"), data = climate,
  control.predictor = list(compute = TRUE)
)
summary(climate.ar1)


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
  geom_ribbon(aes(x = x, y = y, ymin = ll95, ymax = ul95), alpha = 0.75, data = tab.rw1) +
  ggtitle("Random walk of order 1")

library("gridExtra")
grid.arrange(p1, p2, nrow = 2)


# ar1
- sum(log(climate.ar1$cpo$cpo))
# rw1
- sum(log(climate.rw1$cpo$cpo))

library("MixtureInf")
data(earthquake)

#Add year
earthquake$year <- 1900:2006

#Summary of the data
summary(earthquake)


#"earthquake"

ggplot(earthquake, aes(x = year, y = number)) + 
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

n <- nrow(alcohol) - 1 #There is an NA
Y <- matrix(NA, ncol = 2, nrow = n + (n - 1))
Y[1:n, 1] <- alcohol[1:n, 1]
Y[-c(1:n), 2] <- 0

#offset
oset <- c(alcohol[1:n, 5], rep(NA, n - 1))

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


library("DClusterm")
data(brainNM)


# "Standardized Mortality Ratio (SMR) of brain cancer in New Mexico from 1973 to 1991."
library("viridis")
library("spacetime")

splitvalues <- function(xx) {
  res <- cut(xx, c(0, 0.5, 0.8, 1.0, 1.2, 1.5, Inf), include.lowest = TRUE)
  levels(res) <- c("0.0 - 0.5", "0.5 - 0.8", "0.8 - 1.0", "1.0 - 1.2",
     "1.2 - 1.5", "1.5+")
  return(res)
}

brainst@data$SMRcat <- splitvalues(brainst@data$SMR)
stplot(brainst[, , "SMRcat"], col.regions = hcl.colors(6, "Blue-Red"),
  names.attr = 1973:1991, 
  main = "Standardized mortality ratio")


nm.adj <- poly2nb(brainst@sp)
adj.mat <- as(nb2mat(nm.adj, style = "B"), "Matrix")

# Prior of precision
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

brain.st <- inla(Observed ~ 1 + f(Year, model = "rw1",
      hyper = prec.prior) + 
    f(as.numeric(ID), model = "besag", graph = adj.mat,
      hyper = prec.prior),
  data = brainst@data, E = Expected, family = "poisson",
  control.predictor = list(compute = TRUE, link = 1))
summary(brain.st)


brainst@data$RR <- brain.st$summary.fitted.values[ , "mean"] 

stplot(brainst[, , "RR"], col.regions = rev(magma(32)), 
  names.attr = 1973:1991, at = seq(0.75, 1.25, length.out = 32),
  main = "Relative risk estimates")

names(inla.models()$group)


brainst@data$ID.Year <- brainst@data$Year - 1973 + 1
brainst@data$ID2 <- brainst@data$ID


brain.st2 <- inla(Observed ~ 1 + 
    f(as.numeric(ID2), model = "besag", graph = adj.mat,
      group = ID.Year, control.group = list(model = "ar1"),
        hyper = prec.prior),
  data = brainst@data, E = Expected, family = "poisson",
  control.predictor = list(compute = TRUE, link = 1))
summary(brain.st2)


brainst@data$RRgroup <- brain.st2$summary.fitted.values[ , "mean"]

#brainst@data$RRgroupcat <- splitvalues(brainst@data$RRgroup)
stplot(brainst[, , "RRgroup"], col.regions = rev(magma(32)),
  names.attr = 1973:1991, at = seq(0.75, 1.25, length.out = 32),
  main = "Relative risk estimates")

