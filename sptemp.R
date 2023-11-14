library(tidyverse)
library(nlme)
library(sf)
library(tmap)
library(INLA)
library(inlabru)

# Modelos espaciotemporales 
# Modelos separables


# El conjunto de datos brainNM del paquete 
#DClusterm (Gómez-Rubio, Moraga, et al. 2019) 
#proporciona recuentos observados y 
#esperados de casos de cáncer cerebral en los condados de Nuevo México 
#desde 1973 hasta 1991.

library("DClusterm")
data(brainNM)

class(brainst)

summary(STFDF)

# Una estimación simple del riesgo es la proporción de mortalidad estandarizada 
#(SMR, por sus siglas en inglés), 
# que es una estimación simple del riesgo relativo y 
#se calcula como el número de casos observados dividido por los casos esperados. 

# Visualizacion
# SMR of brain cancer in New Mexico from 1973 to 1991"
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

# Definicion de la estructura de adyacencia

nm.adj <- poly2nb(brainst@sp)
adj.mat <- as(nb2mat(nm.adj, style = "B"), "Matrix")

# Como es un modelo separable para datos latice vamos a utilizar para
# el efecto espacial un modelo ICAR y#
# para la tendencia temporal mediante un efecto latente rw1. 
# Utilizaremos no informativas para evitar el sobreajuste.


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

# Solo la dinamica temporal
plot(brain.st$summary.random$Year$ID,
     brain.st$summary.random$Year$mean, type = "l",
     ylim = c(-0.25, 0.25), xlab = "Year", ylab = "Effect")
lines(brain.st$summary.random$Year$ID,
      brain.st$summary.random$Year$"0.025quant", lty = 2)
lines(brain.st$summary.random$Year$ID,
      brain.st$summary.random$Year$"0.975quant", lty = 2)
abline(h = 0, lty = 3, col = "red")

# Solo la dinamica espacial
NMsp <- brainst@sp
NMsp <- SpatialPolygonsDataFrame(
  NMsp,
  data.frame(
    SPATIAL = brain.st$summary.random$"as.numeric(ID)"$mean,
    SPATIAL025 = brain.st$summary.random$"as.numeric(ID)"$"0.025quant",
    SPATIAL975 = brain.st$summary.random$"as.numeric(ID)"$"0.975quant"
  ),
  match.ID = FALSE
)
spplot(NMsp, c("SPATIAL", "SPATIAL025", "SPATIAL975"))

# la variación en cada condado es la suma 
# del efecto aleatorio espacial y la tendencia temporal general
# no se consideran  patrones específicos del condado. 

# Aun siendo un modelo separables pueden incluir términos  
# que tengan en cuenta esta interacción espacio-temporal

# Modelos que aceptan el agrupamiento 

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


stplot(brainst[, , "RRgroup"], col.regions = rev(magma(32)),
       names.attr = 1973:1991, at = seq(0.75, 1.25, length.out = 32),
       main = "Relative risk estimates")


plot(brain.st2$summary.fitted.values[ , "mean"],brain.st$summary.fitted.values[ , "mean"])

names(brain.st2$summary.random)

idx <- seq(1, 32 * 19, by = 32)
cols <- rep("black", 32)
lwd <- rep(1, 32)
cols[1] <- "red"
cols[4] <- "blue"
lwd[1] <- 3
lwd[4] <- 3

par(mfrow = c(1, 2))

plot(1973:1991, brain.st2$summary.random[[1]]$mean[idx], type = "l",
     xlab = "Year", ylab = "Effect", ylim = c(-0.15, 0.15), col = cols[1],
     lwd = lwd[1])
for(i in 2:32) {
  lines(1973:1991, brain.st2$summary.random[[1]]$mean[idx + i],
        col = cols[i], lwd = lwd[i])
}

plot(NMsp)
plot(NMsp[1, ], col = "red", add = TRUE)
plot(NMsp[4, ], col = "blue", add = TRUE)



brainst@data$GROUP <- brain.st2$summary.random[[1]]$mean

stplot(brainst[, , "GROUP"], col.regions = rev(plasma(32)),
       names.attr = 1973:1991, at = seq(-0.15, 0.15, length.out = 32),
       main = "Relative risk estimates")

## Pero para datos geoestadisticos 
# Moraga

# contaminacion del aire EEA por materia particulada

library(lwgeom)
library(raster)

m <- getData(name = "GADM", country = "Spain", level = 0)

m <- m |> 
  st_as_sf() |> 
  st_cast("POLYGON") |> 
  mutate(area = st_area(m))  |> 
  arrange(desc(area)) |> 
  slice(1)

tm_shape(m)+
  tm_polygons()

m <- m %>% st_transform(25830)
ggplot(m) + geom_sf() + theme_bw() + coord_sf(datum = st_crs(m))

d <- d[, c(
  "ReportingYear", "StationLocalId",
  "SamplingPoint_Longitude",
  "SamplingPoint_Latitude",
  "AQValue"
)]
names(d) <- c("year", "id", "long", "lat", "value")

d_sf <- st_as_sf(d,coords = c("long", "lat"), crs=st_crs(4326))

p <- st_as_sf(data.frame(long = d$long, lat = d$lat),
              coords = c("long", "lat"))
st_crs(p) <- st_crs(4326)
p <- p %>% st_transform(25830)
d[, c("x", "y")] <- st_coordinates(p)

ind <- st_intersects(m, p)
d <- d[ind[[1]], ]

ggplot(m) + geom_sf() + coord_sf(datum = st_crs(m)) +
  geom_point(data = d, aes(x = x, y = y)) + theme_bw()

# tm_shape(m)+
#   tm_polygons()+
# tm_shape(d_sf)+
#   tm_dots(fill='value', size=2)

ggplot(d) +
  geom_histogram(mapping = aes(x = value)) +
  facet_wrap(~year, ncol = 1) +
  theme_bw()

ggplot(m) + geom_sf() + coord_sf(datum = NA) +
  geom_point(
    data = d, aes(x = x, y = y, color = value),
    size = 2
  ) +
  labs(x = "", y = "") +
  scale_color_viridis() +
  facet_wrap(~year) +
  theme_bw()


ggplot(d, aes(x = year, y = value, group = id, color = id)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2015, 2016, 2017)) +
  theme_bw() + theme(legend.position = "none")


# modeling 

coo <- cbind(d$x, d$y)
bnd <- inla.nonconvex.hull(st_coordinates(m)[, 1:2])
mesh <- inla.mesh.2d(
  loc = coo, boundary = bnd,
  max.edge = c(100000, 200000), cutoff = 1000
)

plot(mesh)
points(coo, col = "red")


spde <- inla.spde2.pcmatern(
  mesh = mesh, alpha = 2, constr = TRUE,
  prior.range = c(10000, 0.01), # P(range < 10000) = 0.01
  prior.sigma = c(3, 0.01) # P(sigma > 3) = 0.01
)


timesn <- length(unique(d$year))
indexs <- inla.spde.make.index("s",
                               n.spde = spde$n.spde,
                               n.group = timesn
)
lengths(indexs)


group <- d$year - min(d$year) + 1
A <- inla.spde.make.A(mesh = mesh, loc = coo, group = group)


bb <- st_bbox(m)
x <- seq(bb$xmin - 1, bb$xmax + 1, length.out = 50)
y <- seq(bb$ymin - 1, bb$ymax + 1, length.out = 50)
dp <- as.matrix(expand.grid(x, y))
plot(dp, asp = 1)


p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]),
              coords = c("x", "y")
)
st_crs(p) <- st_crs(25830)
ind <- st_intersects(m, p)
dp <- dp[ind[[1]], ]
plot(dp, asp = 1)


dp <- rbind(cbind(dp, 1), cbind(dp, 2), cbind(dp, 3))
head(dp)

coop <- dp[, 1:2]
groupp <- dp[, 3]
Ap <- inla.spde.make.A(mesh = mesh, loc = coop, group = groupp)

stk.e <- inla.stack(
  tag = "est",
  data = list(y = d$value),
  A = list(1, A),
  effects = list(data.frame(b0 = rep(1, nrow(d))), s = indexs)
)

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = rep(1, nrow(dp))), s = indexs)
)

stk.full <- inla.stack(stk.e, stk.p)

rprior <- list(theta = list(prior = "pccor1", param = c(0, 0.9)))

formula <- y ~ 0 + b0 + f(
  s,
  model = spde,
  group = s.group,
  control.group = list(model = "ar1", hyper = rprior)
)

res <- inla(formula,
            data = inla.stack.data(stk.full),
            control.predictor = list(
              compute = TRUE,
              A = inla.stack.A(stk.full)
            )
)

list_marginals <- list(
  "b0" = res$marginals.fixed$b0,
  "precision Gaussian obs" =
    res$marginals.hyperpar$"Precision for the Gaussian observations",
  "range" = res$marginals.hyperpar$"Range for s",
  "stdev" = res$marginals.hyperpar$"Stdev for s",
  "rho" = res$marginals.hyperpar$"GroupRho for s"
)


marginals <- data.frame(do.call(rbind, list_marginals))
marginals$parameter <- rep(names(list_marginals),
                           times = sapply(list_marginals, nrow)
)

ggplot(marginals, aes(x = x, y = y)) + geom_line() +
  facet_wrap(~parameter, scales = "free") +
  labs(x = "", y = "Density") + theme_bw()

index <- inla.stack.index(stack = stk.full, tag = "pred")$data

dp <- data.frame(dp)
names(dp) <- c("x", "y", "time")

dp$pred_mean <- res$summary.fitted.values[index, "mean"]
dp$pred_ll <- res$summary.fitted.values[index, "0.025quant"]
dp$pred_ul <- res$summary.fitted.values[index, "0.975quant"]

library(reshape2)
dpm <- melt(dp,
            id.vars = c("x", "y", "time"),
            measure.vars = c("pred_mean", "pred_ll", "pred_ul")
)

head(dpm)

ggplot(m) + geom_sf() + coord_sf(datum = NA) +
  geom_tile(data = dpm, aes(x = x, y = y, fill = value)) +
  labs(x = "", y = "") +
  facet_wrap(variable ~ time) +
  scale_fill_viridis("PM2.5") +
  theme_bw()

# Krainsky con inlabru 
# https://inlabru-org.github.io/inlabru/articles/svc.html


