library(tidyverse)

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# Monitoreo co2 Mauna Loa Observatory in Hawai’i
co2 <- read.table("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_daily_mlo.txt")
colnames(co2) <- c("yr", "mon", "day", "decimal","co2ppm")

co2 |> ggplot(aes(x=co2ppm))+geom_histogram()

co2 |> ggplot(aes(x=decimal, y=co2ppm))+geom_point()

head(co2)

co2_ts <- ts(co2$co2ppm, deltat=1/365, start = c(1974, 5, 17))

co2_ts <- ts(co2$co2ppm, frequency = 365, start = c(1974,5, 17))

plot(co2_ts)

# co2ppm=m_t+s_t+e_t

ts_decompose <- decompose(co2_ts)

plot(ts_decompose)

co2_d2 <- diff(co2_ts, differences = 1)

plot(co2_d2)

co2_d2d <- diff(co2_d2, lag = 365)

plot(co2_d2d)

acf(co2_ts)

