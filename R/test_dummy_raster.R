# test on dummy raster
# Region #1: 5.043 Km²    
# Region #2: 2.080 Km²    
# Region #3: 0.738125 Km²    
# Region #4: 11.928 Km²    
# Region #5: 3.292 Km²  
# 
# Total area including regions: 100 Km²
# Total regions: 23.080 Km²
# mean: 4.616225 Km2
# sd: 4.383773 Km2

library("raster")
library("landscapemetrics")
library("magrittr")

dummy <- raster(x = "Data/ForTest_Ron_v2.tif")
check_landscape(landscape = dummy)
out_dummy <- calculate_lsm(landscape = dummy, what = c("lsm_c_area_mn","lsm_c_area_sd","lsm_c_np","lsm_c_pd","lsm_c_ed","lsm_c_ca","lsm_l_ta"), directions = 8, progress = TRUE)
print(out_dummy)
