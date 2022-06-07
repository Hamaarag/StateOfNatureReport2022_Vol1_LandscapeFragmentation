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
                     p_area_max_km2 = numeric(), p_num=numeric(), edge_density_km_km2=numeric(), p_density_n_km2=numeric(),
                     unit_total_area_km2=numeric(), p_contig_area_km2=list(), p_contig_prop=numeric(), p_data = list())

# calculate metrics----
# use <lsm_abbreviations_names %>% print(n=Inf)> or <list_lsm()> to get a list of the metrics abbreviations

contiguous_area_thr <- 0.7  # proportion of most contiguous area to include in analysis

for (i in 1:length(years)) {
  for (j in 1:nrow(code_table)) {
    #var <- paste0("Rezef",years[i],"_",code_table[j,unit_in_filename],".tif")
    var <- paste0(code_table[j,unit_in_filename],"Na",years[i],".tif")
    print(var)
    curr_raster <- raster(x = paste0("Data/natural_and_forested_landscape/20220607/",var))
    curr_out <- calculate_lsm(landscape = curr_raster, what = c("lsm_p_area","lsm_p_perim","lsm_c_ed","lsm_l_ta"),directions = 8, progress = F) %>%
      as.data.table()
    curr_out <- curr_out[class==1L | is.na(class),]
    
    # table of patch data
    p_data <- dcast.data.table(curr_out[level=="patch",], formula = id ~ metric, value.var = 'value')
    p_data$area <- p_data$area/100
    p_data$perim <- p_data$perim/1000
    p_data <- p_data[order(-area),]
    setnames(p_data,c("area","perim"),c("area_km2","perim_km"))
    
    # build list of most contiguous patches
    patch_area_v <- p_data[,area_km2] 
    pct_v <- patch_area_v/sum(patch_area_v)
    cdf_v <- cumsum(pct_v)
    idx <- match(T,cdf_v>contiguous_area_thr)
    
    
    # build new results row
    new_output_row <- data.table(unit=code_table[j,unit],
                                 year=as.numeric(years[i]),
                                 edge_density_km_km2=curr_out[metric=="ed",value]/10,
                                 unit_total_area_km2=curr_out[metric=="ta",value]/100,
                                 p_data = list(p_data))
    
    new_output_row[1, `:=`(p_area_mn = mean(p_area_km2[[1]]),
                   p_area_sd = sd(p_area_km2[[1]]),
                   p_total_area_km2=sum(p_area_km2[[1]]),
                   p_area_max_km2 = max(p_area_km2[[1]]),
                   p_num = length(p_area_km2[[1]]))]
    
    new_output_row[1,p_density_n_km2 := p_num / unit_total_area_km2]
    
    new_output_row[1, `:=`(p_contig_area_km2 = list(patch_area_v[1:idx]),
                           p_contig_prop = cdf_v[idx],
                           p_contig_num = idx,
                           )]
    
    setcolorder(x = new_output_row, neworder = names(output))
    
    output <- rbind(output,new_output_row)
  }
}


# calculate percent difference in metrics----

perc_diff <- output[,.(year,
                       area_mn_perc=(p_area_mn-p_area_mn[year==2015])/p_area_mn[year==2015]*100,
                       area_max=(p_area_max_km2-p_area_max_km2[year==2015])/p_area_max_km2[year==2015]*100,
                       edge_density=(edge_density_km_km2-edge_density_km_km2[year==2015])/edge_density_km_km2[year==2015]*100,
                       patch_density=(p_density_n_km2-p_density_n_km2[year==2015])/p_density_n_km2[year==2015]*100),
                    by=unit]

A <- perc_diff[,lapply(.SD,mean),by=year,.SDcols=-"unit"]
A[,unit:=rep("average",3)]
setcolorder(x = A, neworder = names(perc_diff))
perc_diff <- rbind(perc_diff,A)

diff_natural <- output[order(year),lapply(.SD,diff),by=unit, .SDcols = "p_total_area_km2"] %>% cbind(.,data.table(year=c(2017,2020)))
print(diff_natural)

# write data to file----

fwrite(output, file = "Output/results_table_natural_and_forested.csv")
fwrite(perc_diff, file = "Output/perc_diff_table_natural_and_forested.csv")

# plot results----
perc_diff[,highlight:=unit=="average"]

ggplot(perc_diff, aes(x=year,y=area_mn, group=unit, color=unit, size=highlight, linetype=highlight, alpha = highlight)) + geom_line() + ggtitle("Change in average patch size") +
  ylab("Percent change") + scale_size_manual(values = c(1.2, 2)) + scale_linetype_manual(values = c("dashed", "solid")) + scale_alpha_manual(values = c(0.4,1)) + theme_dark() +
  guides(size="none",linetype="none",alpha="none")

ggplot(perc_diff, aes(x=year,y=patch_density, group=unit, color=unit, size=highlight, linetype=highlight, alpha = highlight)) + geom_line() + ggtitle("Change in patch density") +
  ylab("Percent change") + scale_size_manual(values = c(1.2, 2)) + scale_linetype_manual(values = c("dashed", "solid")) + scale_alpha_manual(values = c(0.4,1)) + theme_dark() +
  guides(size="none",linetype="none",alpha="none")

# calculate and plot CDF for 2015 for all units----
# par(mfrow=c(3,3))
# for (i in 1:nrow(code_table)) {
#   curr_unit <- code_table[i,unit]
#   area_v <- output[year==2015 & unit==curr_unit,p_area_km2] %>% strsplit(.,split = '\\|') %>% unlist() %>%
#     as.numeric() %>% sort(.,decreasing = T)
#   # pct_v <- area_v/sum(area_v)
#   # cdf_v <- cumsum(pct_v)
#   # cdf <- ecdf(area_v)
#   # plot(cdf, main = curr_unit)
#   hist(log10(area_v), main = curr_unit)
# }
