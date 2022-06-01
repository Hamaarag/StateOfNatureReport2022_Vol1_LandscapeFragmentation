library("raster")
library("landscapemetrics")
library("magrittr")
library("data.table")

# load data----
code_table <- data.table(unit_in_filename = c("Batha","Horesh","Coastal","Negev","InnerSands","Forest","Sfar","Loess","Arid"),
                         unit = c("batha","maquis","coastal_sands","negev_highlands","inland_sands",
                                  "conifer_forest","desert_med_trans","loess","arid_south"))
years <- c("2015","2017","2020")
output <- data.table(unit=character(), year=numeric(), p_area_mn = numeric(), p_area_sd=numeric(), p_total_area_km2=numeric(),
                     p_max_area_km2 = numeric(), p_number=numeric(), edge_density_km_km2=numeric(), p_density_n_km2=numeric(),
                     unit_total_area_km2=numeric(),p_area_km2 = list())

# calculate metrics----
# use <lsm_abbreviations_names %>% print(n=Inf)> or <list_lsm()> to get a list of the metrics abbreviations
for (i in 1:length(years)) {
  for (j in 1:nrow(code_table)) {
    var <- paste0("Rezef",years[i],"_",code_table[j,unit_in_filename],".tif")
    print(var)
    curr_raster <- raster(x = paste0("Data/",var))
    curr_out <- calculate_lsm(landscape = curr_raster, what = c("lsm_p_area","lsm_c_ed","lsm_l_ta"),directions = 8, progress = F) %>%
      as.data.table()
    curr_out <- curr_out[class==1L | is.na(class),]
    new_output_row <- data.table(unit=code_table[j,unit],
                                 year=as.numeric(years[i]),
                                 edge_density_km_km2=curr_out[metric=="ed",value]/10,
                                 unit_total_area_km2=curr_out[metric=="ta",value]/100,
                                 p_area_km2 = list(curr_out[level=="patch",value]/100))
    
    new_output_row[1, `:=`(p_area_mn = mean(p_area_km2[[1]]),
                   p_area_sd = sd(p_area_km2[[1]]),
                   p_total_area_km2=sum(p_area_km2[[1]]),
                   p_max_area_km2 = max(p_area_km2[[1]]),
                   p_number = length(p_area_km2[[1]]))]
    
    new_output_row[1,p_density_n_km2 := p_number / unit_total_area_km2]
    
    setcolorder(x = new_output_row, neworder = names(output))
    
    output <- rbind(output,new_output_row)
  }
}
fwrite(output, file = "Output/results_table.csv")
