library(sf)
library(spdep)
library(tmap)
library(INLA)
library(spData)

# COVID cordoba
datos <- st_read("datos/Base_07_07_radios.gpkg", quiet = TRUE)

# Exploramos

# Cálculo de número de casos esperados por radio censal

datos$E <- datos$Poblacion * sum(datos$Casos) / sum(datos$Poblacion)

tm_shape(datos) +
  tm_polygons(fill = 'E')

# Cálculo de cociente de infección estandarizada 
#(Standardized Infection Ratio, SIR) por radio censal

datos$SIR <- datos$Casos / datos$E


tm_shape(datos) +
  tm_polygons(fill = 'SIR')

# Definimos la referencias para el efecto aleatorio y el término del error

datos$re_u <- 1:nrow(datos)
datos$re_v <- 1:nrow(datos)


# Generación de lista con vecindarios
nb <- poly2nb(datos)

head(nb)

plot(st_geometry(datos), border = "grey", lwd = 0.5)
plot(
  nb,
  coordinates(as(datos, "Spatial")),
  add = TRUE,
  col = "blue",
  points = FALSE,
  lwd = 0.5
)


# Generación de vecindarios para INLA
file_adj <- tempfile("map.adj1")
nb2INLA(file_adj, nb)
g <- inla.read.graph(filename = file_adj)

# Ajuste del Modelo inflado en ceros
summary(datos)

formula  <-
  Casos ~ 
  Fragmentacion +  
  HogaresNBI + 
  HogaresHacinamiento +
  HogaresJefe.Univ. + 
  Bancos + 
  f(re_u, model = "besag", graph = g) + 
  f(re_v, model = "iid")


res_inflpoi <-
  inla(
    formula,
    family = "zeroinflatedpoisson1",
    data = datos,
    E = E,
    control.predictor = list(compute = TRUE),
    control.compute= list(return.marginals.predictor=TRUE)
  )
summary(res_inflpoi)


# Distribucion posteriori para los efectos fijos 

marginal <- inla.smarginal(res_inflpoi$marginals.fixed$Fragmentacion)
marginal <- data.frame(marginal)
ggplot(marginal, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") + theme_bw()


# Mapeo de los Riesgos Relativos

datos$RR <- res_inflpoi$summary.fitted.values[, "mean"]
datos$RR_LI <- res_inflpoi$summary.fitted.values[, "0.025quant"]
datos$RR_LS <- res_inflpoi$summary.fitted.values[, "0.975quant"]

popup_vars <- c(
  "Fragmentación (Nivel)" = 'Fragmentacion',
  "Valor Tierra Medio ($/m2)" = 'ValorTierra',
  "Hogares NBI (%)" = 'HogaresNBI',
  "Hogares Hacinamiento (%)" = 'HogaresHacinamiento',
  "Hogares Jefe Univ.(%)" = 'HogaresJefe.Univ.',
  "Bancos",
  "Casos",
  "Población" = 'Poblacion',
  "E",
  "SIR",
  "RR_LI",
  "RR",
  "RR_LS"
)


tmap_mode('view')

mapas <-
  tm_basemap(c(Urbano = "OpenStreetMap", 
               Satelite = "Esri.WorldImagery")) +
  tm_shape(datos,
           name = 'Casos')  +
  tm_polygons(
    fill = 'Casos',
    fill.scale = tm_scale(
      style = "fixed",
      breaks = c(0, 1, 2, 3, 5, 10, 15, 25, 36),
      values = "brewer.yl_or_br"),
    fill.legend = tm_legend(title =  "Casos por Radio",
                            scientific = TRUE,
                            format = "f",
                            digits = 0
    ),
    col = "gray50",
    col_alpha = .5,
    popup.vars = popup_vars
  ) +
  
  tm_shape(datos,
           name = 'Tasa de Infección - SIR')  +
  tm_polygons(
    fill = "SIR",
    fill.scale = tm_scale(
      style = "fixed",
      breaks = c(0, 5, 10, 15, 20, 25, 30, 50, 60, 90),
      values = "brewer.yl_or_br"),
    fill.legend = tm_legend( 
      title = 'SIR',
      scientific = TRUE,
      format = "f",
      digits = 0
    ),
    col = "gray50",
    col_alpha = .5,
    popup.vars = popup_vars
  ) +
  
  tm_shape(datos,
           name = 'Riesgo Relativo')  +
  tm_polygons(
    fill = "RR",
    fill.scale = tm_scale(
      style = "fixed",
      breaks = c(0.4, 1, 2, 4, 8, 10, 15, 20, 30, 50, 85, 96),
      values = "brewer.yl_or_br"),
    fill.legend = tm_legend( 
      title = "Riesgo Relativo",
      scientific = TRUE,
      format = "f",
      digits = 1
    ),
    col = "gray50",
    col_alpha = .5,
    popup.vars = popup_vars
  )
mapas


# Podemos calcular la probabilidad de que la SIR supere un valor determinado

marg <- res_inflpoi$marginals.fitted.values[[1]]

1 - inla.pmarginal(q = 2, marginal = marg)


exc <- sapply(res_inflpoi$marginals.fitted.values,
              FUN = function(marg){1 - inla.pmarginal(q = 2, marginal = marg)})
