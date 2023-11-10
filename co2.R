library(tidyverse)

theme_set(theme_bw())
theme_update(panel.grid = element_blank())


co2 <- read.table("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_daily_mlo.txt")
colnames(co2) <- c("yr", "mon", "day", "decimal","co2ppm")

co2 |> ggplot(aes(x=co2ppm))+geom_histogram()

co2 |> ggplot(aes(x=decimal, y=co2ppm))+geom_point()

min(co2$mon)

co2_ts <- ts(co2$co2ppm, deltat=1/365, start = c(1974, 1))

plot(co2_ts)

ggAcf(co2_ts,lag=365)

ts_decompose <- decompose(co2_ts)

plot(ts_decompose)
