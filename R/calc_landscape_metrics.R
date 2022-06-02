library("raster")
library("landscapemetrics")
library("magrittr")
library("data.table")
library("ggplot2")

# load data----
code_table <- data.table(unit_in_filename = c("Batha","Horesh","Coastal","Negev","InnerSands","Forest","Sfar","Loess","Arid"),
                         unit = c("batha","maquis","coastal_sands","negev_highlands","inland_sands",
                                  "conifer_forest","desert_med_trans","loess","arid_south"))
years <- c("2015","2017","2020")
output <- data.table(unit=character(), year=numeric(), p_area_mn = numeric(), p_area_sd=numeric(), p_total_area_km2=numeric(),
                     p_area_max_km2 = numeric(), p_number=numeric(), edge_density_km_km2=numeric(), p_density_n_km2=numeric(),
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
                   p_area_max_km2 = max(p_area_km2[[1]]),
                   p_number = length(p_area_km2[[1]]))]
    
    new_output_row[1,p_density_n_km2 := p_number / unit_total_area_km2]
    
    setcolorder(x = new_output_row, neworder = names(output))
    
    output <- rbind(output,new_output_row)
  }
}

# calculate percent difference in metrics----

perc_diff <- output[,.(year, area_mn=(p_area_mn-p_area_mn[year==2015])/p_area_mn[year==2015]*100,
                       area_max=(p_area_max_km2-p_area_max_km2[year==2015])/p_area_max_km2[year==2015]*100,
                       edge_density=(edge_density_km_km2-edge_density_km_km2[year==2015])/edge_density_km_km2[year==2015]*100,
                       patch_density=(p_density_n_km2-p_density_n_km2[year==2015])/p_density_n_km2[year==2015]*100),
                    by=unit]

A <- perc_diff[,lapply(.SD,mean),by=year,.SDcols=-"unit"]
A[,unit:=rep("average",3)]
setcolorder(x = A, neworder = names(perc_diff))
perc_diff <- rbind(perc_diff,A)

# write data to file----

fwrite(output, file = "Output/results_table.csv")
fwrite(perc_diff, file = "Output/perc_diff_table.csv")

# plot results----
perc_diff[,highlight:=unit=="average"]

ggplot(perc_diff, aes(x=year,y=area_mn, group=unit, color=unit, size=highlight, linetype=highlight, alpha = highlight)) + geom_line() + ggtitle("Change in average patch size") +
  ylab("Percent change") + scale_size_manual(values = c(1.2, 2)) + scale_linetype_manual(values = c("dashed", "solid")) + scale_alpha_manual(values = c(0.4,1)) + theme_dark() +
  guides(size="none",linetype="none",alpha="none")

ggplot(perc_diff, aes(x=year,y=patch_density, group=unit, color=unit, size=highlight, linetype=highlight, alpha = highlight)) + geom_line() + ggtitle("Change in patch density") +
  ylab("Percent change") + scale_size_manual(values = c(1.2, 2)) + scale_linetype_manual(values = c("dashed", "solid")) + scale_alpha_manual(values = c(0.4,1)) + theme_dark() +
  guides(size="none",linetype="none",alpha="none")

# calculate and plot CDF for 2015 for all units----
par(mfrow=c(3,3))
for (i in 1:nrow(code_table)) {
  curr_unit <- code_table[i,unit]
  area_v <- output[year==2015 & unit==curr_unit,p_area_km2] %>% strsplit(.,split = '\\|') %>% unlist() %>%
    as.numeric() %>% sort(.,decreasing = T)
  # pct_v <- area_v/sum(area_v)
  # cdf_v <- cumsum(pct_v)
  # cdf <- ecdf(area_v)
  # plot(cdf, main = curr_unit)
  hist(log10(area_v), main = curr_unit)
}
