library("raster")
library("rgdal")
library("landscapemetrics")
library(dplyr)
library(purrr)

# version 2 data sources----
arid14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/AridNa2015.TIF')
arid18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/AridNa2017.TIF')
arid20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/AridNa2019.TIF')

negev14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/NegevNa2015.TIF')
negev18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/NegevNa2017.TIF')
negev20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/NegevNa2019.TIF')

inland14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/InnerSandsNa2015.TIF')
inland18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/InnerSandsNa2017.TIF')
inland20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/InnerSandsNa2019.TIF')

batha14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/BathaNa2015.TIF')
batha18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/BathaNa2017.TIF')
batha20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/BathaNa2019.TIF')

coastal14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/CostalNa2015.TIF')
coastal18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/CoastalNa2017.TIF')
coastal20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/CoastalNa2019.TIF')

forest14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/ForestsNa2015.TIF')
forest18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/ForestsNa2017.TIF')
forest20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/ForestsNa2019.TIF')

maquis14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/HoreshNa2015.TIF')
maquis18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/HoreshNa2017.TIF')
maquis20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/HoreshNa2019.TIF')

loess14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/LoessNa2015.TIF')
loess18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/LoessNa2017.TIF')
loess20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/LoessNa2019.TIF')

sfar14 <- raster(x = 'Data/rasters/from_Michal/2015/ver2/SfarNa2015.TIF')
sfar18 <- raster(x = 'Data/rasters/from_Michal/2017/ver2/SfarNa2017.TIF')
sfar20 <- raster(x = 'Data/rasters/from_Michal/2019/ver2/SfarNa2019.TIF')


#----
raster_names <- c("arid", "batha", "coastal", "forest", "inland", "loess", "maquis", "negev", "sfar")
raster_names_t0 <- paste0(raster_names,"14")
raster_names_t2 <- paste0(raster_names,"18")
raster_names_t3 <- paste0(raster_names,"20")

metrics_t0 <- vector(mode = "list", length = length(raster_names))
metrics_t2 <- vector(mode = "list", length = length(raster_names))
metrics_t3 <- vector(mode = "list", length = length(raster_names))

# natural patch area (ca=class area), patch density, patch edge density
for (i in 1:length(metrics_t0)) {
  code <- paste0("curr_raster <- ",raster_names_t0[i])
  eval(parse(text = code))
  out <- calculate_lsm(landscape = curr_raster, metric = c("ca","pd","ed"), directions = 8, progress = TRUE)
  metrics_t0[[i]] <- out
}

for (i in 1:length(metrics_t2)) {
  code <- paste0("curr_raster <- ",raster_names_t2[i])
  eval(parse(text = code))
  out <- calculate_lsm(landscape = curr_raster, metric = c("ca","pd","ed"), directions = 8, progress = TRUE)
  metrics_t2[[i]] <- out
}

for (i in 1:length(metrics_t3)) {
  code <- paste0("curr_raster <- ",raster_names_t3[i])
  eval(parse(text = code))
  out <- calculate_lsm(landscape = curr_raster, metric = c("ca","pd","ed"), directions = 8, progress = TRUE)
  metrics_t3[[i]] <- out
}

Lmetrics <- vector(mode = "list", length = length(raster_names))
for (i in 1:length(raster_names)) {
  a <- metrics_t0[[i]] %>% dplyr::filter(level=="landscape" | (metric=="ca" & class==1)) %>%
    cbind(raster_names[i],.) %>% mutate(eco_unit=raster_names[i], value_t0=value) %>%
    dplyr::select(eco_unit,metric,value_t0)
  b <- metrics_t2[[i]] %>% dplyr::filter(level=="landscape" | (metric=="ca" & class==1)) %>%
    cbind(raster_names[i],.) %>% mutate(eco_unit=raster_names[i], value_t2=value) %>%
    dplyr::select(eco_unit,metric,value_t2)
  c <- metrics_t3[[i]] %>% dplyr::filter(level=="landscape" | (metric=="ca" & class==1)) %>%
    cbind(raster_names[i],.) %>% mutate(eco_unit=raster_names[i], value_t3=value) %>%
    dplyr::select(eco_unit,metric,value_t3)
  Lmetrics[[i]] <- inner_join(inner_join(a,b, by = c("eco_unit","metric")), c, by = c("eco_unit","metric"))
}
metrics <- do.call(rbind,Lmetrics)
save(list = c("metrics_t0","metrics_t2","metrics_t3","Lmetrics","metrics"),file = "Data/rasters/results_ver2/T0_T2_T3_results.RData")
write.csv(x = metrics, file="Data/rasters/results_ver2/T0_T2_T3_results.csv")


