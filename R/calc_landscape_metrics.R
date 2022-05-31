library("raster")
library("landscapemetrics")
library("magrittr")
library("data.table")

# load data----
code_table <- data.table(unit_in_filename = c("Batha","Horesh","Coastal","Negev","InnerSands","Forest","Sfar","Loess","Arid"),
                         unit = c("batha","maquis","coastal_sands","negev_highlands","inland_sands",
                                  "conifer_forest","desert_med_trans","loess","arid_south"))
years <- c("2015","2017","2020")
output <- data.table(unit=character(), year=numeric(), p_area_mn = numeric(), p_area_sd=numeric(), p_total_area=numeric(),
                     p_number=numeric(), edge_density=numeric(), p_density=numeric(),unit_total_area=numeric())

# calculate metrics----
# use <lsm_abbreviations_names %>% print(n=Inf)> or <list_lsm()> to get a list of the metrics abbreviations
for (i in 1:length(years)) {
  for (j in 1:nrow(code_table)) {
    var <- paste0("Rezef",years[i],"_",code_table[j,1,with=F],".tif")
    print(var)
    curr_raster <- raster(x = paste0("Data/",var))
    curr_out <- calculate_lsm(landscape = curr_raster, what = c("lsm_c_area_mn","lsm_c_area_sd","lsm_c_np","lsm_c_pd",
                                                                "lsm_c_ed","lsm_c_ca","lsm_l_ta"),
                              directions = 8, progress = F) %>% as.data.table()
    curr_out <- curr_out[class==1L | is.na(class),]
    new_output_row <- data.table(unit=code_table[j,unit],
                                 year=as.numeric(years[i]),
                                 p_area_mn = curr_out[metric=="area_mn",value],
                                 p_area_sd=curr_out[metric=="area_sd",value],
                                 p_total_area=curr_out[metric=="ca",value],
                                 p_number=curr_out[metric=="np",value],
                                 edge_density=curr_out[metric=="ed",value],
                                 p_density=curr_out[metric=="pd",value],
                                 unit_total_area=curr_out[metric=="ta",value])
    output <- rbind(output,new_output_row)
  }
}
fwrite(output, file = "Output/results_table.csv")
