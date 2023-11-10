library(nlme)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(INLA)
library(inlabru)

datos <- read.table("datos/suelos_cba.txt", header = T)


datos_sf <-
  sf::st_as_sf(datos,
               coords = c('X', 'Y'),
               crs = 32720)

tmap_mode('view')


tm_shape(datos_sf) +
  tm_dots(fill = 'COS',
          fill.scale = tm_scale_continuous(values = "carto.ag_grn_yl"),
          size = 0.5)

tm_shape(datos_sf) +
  tm_dots(fill = 'elevacion',
          size = 0.5)


datos |> ggplot(aes(COS)) +
  geom_histogram(aes(y = after_stat(count / sum(count)))) +
  labs(y = 'Frecuencia Relativa')


ggplot(datos, aes(COS, arcilla)) +
  geom_point()


pred_cos_gls <- gls(COS ~ elevacion+ twi+ arcilla + pH , 
                   data = datos, 
                   method = 'REML')
summary(cos_gls)


pred_cos_gls_corr <- gls(
  COS ~ elevacion+ twi+ arcilla + pH ,
  correlation = corExp(
    form =  ~ as.numeric(as.character(X)) + 
      as.numeric(as.character(Y)),
    metric = "euclidean",
    nugget = FALSE
  ),
  data = datos,
  method = 'REML'
)
summary(pred_cos_gls_corr)


predichos_lm <- datos
predichos_lm$pred_cos_gls <- predict(cos_gls)
predichos_lm$pred_cos_gls_corr <- predict(pred_cos_gls_corr)


predichos_lm |> ggplot(aes(pred_cos_gls, COS)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

predichos_lm |> ggplot(aes(pred_cos_gls_corr, COS)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)



inla.setOption(inla.mode = 'experimental')



# generacion-grilla-inla

loc <- st_coordinates(datos_sf)

mesh <- INLA::inla.mesh.2d(
  loc = loc,
  offset = c(10000, 40000),
  cutoff = 100,
  max.edge = c(5000, 10000),
  max.n = 5)

ggplot(datos_sf) +
  gg(mesh) +
  geom_sf()


spde <- INLA::inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(25000, 0.01),
  prior.sigma = c(2, 0.01)
)


cos_bru_spde <-
  bru(
    COS ~ Intercept(1) + elevacion + twi + arcilla + pH + site(main = coordinates, model = spde),
    family = "gaussian",
    data = as_Spatial(datos_sf)
  )

summary(cos_bru_spde)

plot(cos_bru_spde, "Intercept")
plot(cos_bru_spde, "arcilla")

spde.posterior(cos_bru_spde, "site", what = "matern.covariance") -> covplot
spde.posterior(cos_bru_spde, "site", what = "matern.correlation") -> corplot
spde.posterior(cos_bru_spde, "site", what = "range") -> rngplot
spde.posterior(cos_bru_spde, "site", what = "log.range") -> lgrngplot
spde.posterior(cos_bru_spde, "site", what = "variance") -> varplot
spde.posterior(cos_bru_spde, "site", what = "log.variance") -> lgvarplot

multiplot(plot(covplot), plot(corplot),
          plot(rngplot), plot(lgrngplot),
          plot(varplot), plot(lgvarplot))


pred_mesh <-
  predict(cos_bru_spde, 
          as_Spatial(datos_sf), ~ Intercept + elevacion + twi + arcilla + pH + site)
predichos <- st_as_sf(pred_mesh)

ggplot(predichos, aes(mean, COS)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

predichos_lm$pred_cos_inla <- predichos$mean

ggplot() +
  geom_point(data = predichos_lm, 
             aes(pred_cos_gls, COS, color = 'REML iid')) +
  geom_point(data = predichos_lm, 
             aes(pred_cos_gls_corr, COS, color = 'REML Corr')) +
  geom_point(data = predichos_lm, 
             aes(pred_cos_inla, COS, color = 'INLA Corr')) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = 'COS Predichos', y = 'COS Observado', color = 'Estimación')



