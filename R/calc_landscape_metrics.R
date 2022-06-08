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
output <- data.table(unit=character(), year=numeric(), p_area_mn = numeric(), p_area_sd=numeric(), p_area_tot_km2=numeric(),
                     p_area_max_km2 = numeric(), p_num=numeric(), edge_density_km_km2=numeric(), p_density_n_km2=numeric(),
                     unit_area_tot_km2=numeric(), p_contig_area_tot_km2=numeric(),
                     p_contig_prop = numeric(), p_contig_num = numeric(), p_contig_area_mn = numeric(),
                     p_contig_area_md = numeric(), p_contig_density_n_km2 = numeric(), 
                     p_contig_edge_dens_km_km2 = numeric())

p_data <- data.table(unit=character(), year=numeric(), id=numeric(), area_km2=numeric(), perim_km=numeric())

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
    curr_p_data <- dcast.data.table(curr_out[level=="patch",], formula = id ~ metric, value.var = 'value')
    curr_p_data$area <- curr_p_data$area/100
    curr_p_data$perim <- curr_p_data$perim/1000
    curr_p_data <- curr_p_data[order(-area),]
    setnames(curr_p_data,c("area","perim"),c("area_km2","perim_km"))
    curr_p_data <- cbind(data.table(unit=code_table[j,unit],year=years[i]),curr_p_data)
    p_data <- rbind(p_data,curr_p_data)
    
    # build list of most contiguous patches
    patch_area_v <- curr_p_data[,area_km2] 
    pct_v <- patch_area_v/sum(patch_area_v)
    cdf_v <- cumsum(pct_v)
    idx <- match(T,cdf_v>contiguous_area_thr)
    curr_contig_p <- curr_p_data[1:idx,]
    
    # build new results row
    new_output_row <- data.table(unit=code_table[j,unit],
                                 year=as.numeric(years[i]),
                                 edge_density_km_km2=curr_out[metric=="ed",value]/10,
                                 unit_area_tot_km2=curr_out[metric=="ta",value]/100)
    
    new_output_row[1, `:=`(p_area_mn = mean(curr_p_data[,area_km2]),
                   p_area_sd = sd(curr_p_data[,area_km2]),
                   p_area_tot_km2=sum(curr_p_data[,area_km2]),
                   p_area_max_km2 = max(curr_p_data[,area_km2]),
                   p_num = length(curr_p_data[,area_km2]),
                   p_contig_area_tot_km2 = sum(curr_contig_p[,area_km2]),
                   p_contig_prop = cdf_v[idx],
                   p_contig_num = idx,
                   p_contig_area_mn = mean(curr_contig_p[,area_km2]),
                   p_contig_area_md = median(curr_contig_p[,area_km2]))]
    
    new_output_row[1, `:=`(p_density_n_km2 = p_num / unit_area_tot_km2,
                           p_contig_density_n_km2 = idx/p_contig_area_tot_km2,
                           p_contig_edge_dens_km_km2 = sum(curr_contig_p[,perim_km])/p_contig_area_tot_km2)]
    
    
    setcolorder(x = new_output_row, neworder = names(output))
    
    output <- rbind(output,new_output_row)
  }
}

# write data to file----

fwrite(output, file = "Output/results_table_natural_and_forested.csv")
fwrite(p_data, file = "Output/patch_data.csv")


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

diff_natural <- output[order(year),lapply(.SD,diff),by=unit, .SDcols = "p_area_tot_km2"] %>% cbind(.,data.table(year=c(2017,2020)))
print(diff_natural)

# write data to file
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
