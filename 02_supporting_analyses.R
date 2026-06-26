
library(tidyverse)
library(terra)

#New analysis for reviewer replies and supporting figures.

#significance tests for % change in spring GPP during drought ------

#import raster stack
ngp_df <- terra::rast('data/ngp/ngp_raster_stack.tif') %>%
  as.data.frame(xy=T) %>%
  dplyr::mutate(drought_sens = (((mean_npp - drought_npp)/mean_npp)*100)/(mean_precip - drought_precip)) %>%
  dplyr::mutate(drought_sens_abs = ((mean_npp - drought_npp))/(mean_precip - drought_precip))

#loop through random draws and do a one-sided t-test
p_val_list <- list()
for(i in 1:100){

ngp_sample <- ngp_df[sample(nrow(ngp_df), 100, replace = TRUE), ]

test <- t.test(ngp_sample$percent_change_spring, mu = 0, alternative = "greater")

#mean(ngp_sample$drought_spring_gpp)

p_val_list[[i]] <- test$p.value

}

#quick look at output (% change and P values)
hist(ngp_sample$percent_change_spring)

pva_vec <- unlist(p_val_list)
hist(pva_vec)
mean(pva_vec)
#not anywhere below .05, consistent with the high variability in spring GPP responses

#cleanup
rm(ngp_df,ngp_sample,p_val_list,test,i,pva_vec)

#-----------------------------------------------------------

#mapping of drought years -----

#import raster stack
ngp_stack <- terra::rast('data/ngp/ngp_raster_stack.tif') 

#to df
ngp_df <- ngp_stack %>%
  as.data.frame(xy=T) %>%
  dplyr::mutate(drought_sens = (((mean_npp - drought_npp)/mean_npp)*100)/(mean_precip - drought_precip)) %>%
  dplyr::mutate(drought_sens_abs = ((mean_npp - drought_npp))/(mean_precip - drought_precip),
                drought_year = as.integer(drought_year))

#isolate to just drought years
ngp_year_rast <- ngp_stack$drought_year
ngp_year <- as.data.frame(ngp_year_rast$drought_year,xy = TRUE)
ngp_year$drought_year <- as.integer(ngp_year$drought_year) #ensure years are integers

#23 unique drought events in the analysis
length(unique(ngp_year$drought_year))

#create a plot showing drought year vs. spring GPP response
drought_year_plot <- ngp_df %>%
  group_by(drought_year) %>%
  dplyr::summarise(mean_change = mean(percent_change_spring),
                   sd_change = sd(percent_change_spring),
                   n_change = (length(percent_change_spring)/nrow(ngp_df))*100) %>%
ggplot(aes(x=drought_year,y=mean_change)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(size=n_change)) +
  geom_errorbar(aes(ymin = mean_change-sd_change, ymax = mean_change+sd_change),width = 0.5) +
  xlab('Drought year')  +
  ylab('Change in spring GPP (%)') +
  labs(size = '% of droughts') +
  theme(
    axis.text.x = element_text(color='black',size=12), 
    axis.text.y = element_text(color='black',size=12),
    axis.title.x = element_text(color='black',size=20),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.text = element_text(size=15),
    legend.position = c(0.6,0.2),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#get the geopolitical boundaries for context in the map

#albers conic projection
Albers <-
  terra::crs(
    '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
  )

na <- geodata::gadm("GADM", country=c('USA','CAN'), level=1,download=TRUE)

na <- na[na$NAME_1 %in% c("Montana","South Dakota","Wyoming",'Nebraska',
                          "North Dakota","Alberta","Saskatchewan"),]

na <- terra::project(na,Albers)

#map it
drought_year_map <- ggplot() +
    tidyterra::geom_spatraster(data =ngp_year_rast) +
    scale_fill_viridis_c(na.value = "transparent",name = "Drought\nyear") +
    geom_sf(data=na,fill = NA, color = "black") +
    coord_sf(
             xlim = c(-1388578, -131291.4),
             ylim = c(1966455, 3321902)) +
    xlab('') +
    ylab('') +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(color = 'black', size = 10),
      axis.title.y = element_text(color = 'black', size = 10),
      axis.ticks = element_blank(),
      legend.key = element_blank(),
      legend.key.width = unit(2, "cm"),
      #legend.title = element_blank(),
      legend.position = 'top',
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(size = 10),
      panel.background = element_rect(fill = NA),
      panel.border = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank()) 
   
  
#save
png(height = 1500,width=3100,res=300,'figures/drought_year_supporting.png')

cowplot::plot_grid(drought_year_map,drought_year_plot, ncol=2,
                   labels = c('(a)','(b)'),label_fontface = "bold")

dev.off()

#cleanup
rm(na,ngp_df,ngp_stack,ngp_year,ngp_year_rast,Albers,drought_year_plot,drought_year_map)
  
#-----------------------------------------------------------

#cper GCC plot -----

cper_gcc_data <- 
  read.csv('data/sgs/cperagm_GR_1000_1day.csv')

dry_normal_years <- cper_gcc_data %>%
  dplyr::filter(year %in% c(2021,2022))  %>%
  dplyr::select(year,doy,gcc_90)
  
dry_normal_years$doy_2 <- as.Date(dry_normal_years$doy)
  
cper_gcc <- ggplot(dry_normal_years,aes(doy_2,gcc_90,color=as.factor(year))) +
    geom_point(size=1) +
    geom_line() +
    xlab('') +
    ylab('Daily canopy greenness (GCC)') +
    scale_color_manual(values=c('2021'='blue','2022'='red'),
                       labels = c('Average year','Extreme drought')) +
    scale_x_date(date_labels = "%b", breaks = "month") +
    theme(
      axis.text.x = element_text(color='black',size=12), 
      axis.text.y = element_text(color='black',size=12),
      axis.title.x = element_text(color='black',size=20),
      axis.title.y = element_text(color='black',size=20),
      axis.ticks = element_line(color='black'),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=15),
      legend.position = c(0.18,0.8),
      strip.background =element_rect(fill="white"),
      strip.text = element_text(size=10),
      panel.background = element_rect(fill=NA),
      panel.border = element_blank(), 
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"))
  
  
#make the precip anomaly inset
  
#import precip-temp ata
cper_precip_temp <- read.csv('data/sgs/sgs_daymet_precip_temp_2.csv') %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    annual_precip = sum(prcp..mm.day.))
  
cper_precip <- cper_precip_temp %>%
    dplyr::filter(year %in% c(2021,2022)) %>%
    ggplot(aes(as.factor(year),annual_precip,fill=as.factor(year))) +
    stat_summary(geom = 'bar',fun.y = 'mean',color='black') +
    xlab('') +
    ylab('Annual precipitation (mm)') +
    scale_fill_manual(values=c('2021'='blue','2022'='red')) +
    scale_y_continuous(expand=c(0,0)) +
    theme(
      axis.text.x = element_text(color='black',size=15), 
      axis.text.y = element_text(color='black',size=10),
      axis.title.x = element_text(color='black',size=15),
      axis.title.y = element_text(color='black',size=12),
      axis.ticks = element_line(color='black'),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=14),
      legend.position = 'none',
      strip.background =element_rect(fill="white"),
      strip.text = element_text(size=10),
      panel.background = element_rect(fill=NA),
      panel.border = element_blank(), 
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"))
  
  
#try to make inset
vp <- grid::viewport(width = 0.44, height = 0.39, x = 0.77,y=0.7)
  
#executing the inset, you create a function the utlizes all the previous code
full <- function() {
    print(cper_gcc)
    print(cper_precip, vp = vp)
    }

  #save
png(height = 1700,width=2100,res=300,'figures/cper_gcc.png')
  
full()
  
dev.off()

#cleanup
rm(cper_gcc_data,cper_precip_temp,dry_normal_years,vp,cper_gcc,cper_precip,full)

#-----------------------------------------------------------

#look at multi-year drought ------


#import raster stack
ngp_stack <- terra::rast('data/ngp/ngp_raster_stack.tif')

#reproject
ngp_stack_decimal <- project(ngp_stack,"EPSG:4326", method = "bilinear")

#turn to df
ngp_df_multi_year_look <- as.data.frame(ngp_stack_decimal,xy=T) %>%
  dplyr::mutate(drought_year = as.integer(drought_year)) %>%
  dplyr::select(x,y,drought_year,drought_precip,mean_precip)

#rows to iterate through; the loop often fails, so have to start where it ended (e.g., row 82122)
nrow(ngp_df_multi_year_look)

rm(ngp_stack,ngp_stack_decimal)

#Loop through vector of years to download/see what prior year was; do this once
# previous_yr_weather_list <- list()
# for (i in 82122:nrow(ngp_df_multi_year_look)) {
#   
#   print(i)
#   
#   current_value <- as.numeric(ngp_df_multi_year_look$drought_year[i])
#   #print(current_value)
#   
#   #find previous year
#   previous_year <- current_value - 1
#   #print(previous_year)
#   
#   previous_yr_weather <- daymetr::download_daymet(
#     lat = ngp_df_multi_year_look$y[i],
#     lon = ngp_df_multi_year_look$x[i],
#     start = previous_year,
#     end = previous_year
#   ) %>%
#     
#     .$data %>%
#     
#     # #--- get date from day of the year ---#
#     # mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
#     
#     dplyr::mutate(mean_temp = (tmax..deg.c. + tmin..deg.c.)/2) %>%
#     dplyr::summarise(
#       annual_precip = sum(prcp..mm.day.),
#       annual_temp = mean(mean_temp),
#       average_max_temp = mean(tmax..deg.c.),
#       average_min_temp = mean(tmin..deg.c.)) %>%
#     dplyr::mutate(x = ngp_df_multi_year_look$x[i],
#                   y = ngp_df_multi_year_look$y[i],
#                   drought_year = ngp_df_multi_year_look$drought_year[i],
#                   drought_precip = ngp_df_multi_year_look$drought_precip[i],
#                   mean_precip = ngp_df_multi_year_look$mean_precip[i])
#   
#   
#   write.table(previous_yr_weather, 
#               file = 'data/ngp/multi_year_look/previous_yr_weather.csv',
#               append = TRUE, 
#               sep = ",", 
#               dec = ".",
#               row.names = FALSE,
#               col.names = !file.exists('data/ngp/multi_year_look/previous_yr_weather.csv'))
#   
#   #previous_yr_weather_list[[i]] <- previous_yr_weather
#   
# }

#import the resulting dataframe
previous_yr_weather_df <- read.csv('data/ngp/multi_year_look/previous_yr_weather.csv') %>%
  dplyr::mutate(drought_perc_reduc = ((drought_precip-mean_precip)/mean_precip)*100,
                prev_yr_perc_reduc = ((annual_precip-mean_precip)/mean_precip)*100) %>%
  dplyr::mutate(filter_metric = drought_perc_reduc + 10) #only consider years within 10% of drought yr

#quick look
hist(previous_yr_weather_df$prev_yr_perc_reduc)
hist(previous_yr_weather_df$drought_perc_redu)

#see how many sites were multi-yr droughts, as defined here:
previous_yr_weather_df %>%
  dplyr::filter(prev_yr_perc_reduc < filter_metric) %>%
  nrow()/nrow(previous_yr_weather_df)

#about 7.7% of drought analyzed were preceded by a similarly extreme drought, making it part
#of a multi-year drought.

#cleanup
rm(ngp_df_multi_year_look,previous_yr_weather_df)

#-----------------------------------------------------------
#seasonality look ----

#cper
cper_seasonality <- read.csv('data/sgs/sgs_daymet_precip_temp_2.csv') %>%
  dplyr::mutate(month_val = lubridate::month(as.Date(yday))) %>%
  dplyr::group_by(year,month_val) %>%
    dplyr::summarise(monthly_precip = sum(prcp..mm.day.),
                     monthly_temp = mean(tmax..deg.c.))  %>%
  dplyr::group_by(month_val) %>%
  dplyr::summarise(monthly_precip = mean(monthly_precip),
                   monthly_temp = mean(monthly_temp))

cor(cper_seasonality$monthly_precip,cper_seasonality$monthly_temp,method='spearman')
#0.84

#NGP/ap
ap_seasonality <- read.csv('data/ap/daymet_precip_temp_ap.csv') %>%
  dplyr::mutate(month_val = lubridate::month(as.Date(yday))) %>%
  dplyr::group_by(year,month_val) %>%
  dplyr::summarise(monthly_precip = sum(prcp..mm.day.),
                   monthly_temp = mean(tmax..deg.c.)) %>%
  dplyr::group_by(month_val) %>%
  dplyr::summarise(monthly_precip = mean(monthly_precip),
                   monthly_temp = mean(monthly_temp))
  

cor(ap_seasonality$monthly_precip,ap_seasonality$monthly_temp,method='spearman')
plot(ap_seasonality$monthly_precip,ap_seasonality$monthly_temp,method='spearman')
#0.89

#-----------------------------------------------------------
#spatial block bootstrapping ------

#See if inferences change when done on a randomized subset (stratified by drought year)

#import raster stack
ngp_stack_bootstrapping <- terra::rast('data/ngp/ngp_raster_stack.tif') %>%
  as.data.frame(ngp_stack,xy=T) %>%
  dplyr::mutate(drought_sens = (((mean_npp - drought_npp)/mean_npp)*100)/(mean_precip - drought_precip)) %>%
  dplyr::mutate(drought_sens_abs = ((mean_npp - drought_npp))/(mean_precip - drought_precip)) %>%
  dplyr::filter(percent_change_summer < 0 & percent_change_spring > 0) %>%
  dplyr::mutate(compensation_abs = ((drought_spring_gpp - mean_spring_gpp) - (drought_summer_gpp - mean_summer_gpp))*.01) %>%
  dplyr::mutate(compensation_rel = percent_change_spring - percent_change_summer) %>%
  dplyr::mutate(compensation_abs_2 = abs(((drought_spring_gpp - mean_spring_gpp)/(drought_summer_gpp - mean_summer_gpp)))) %>%
  dplyr::filter(compensation_abs_2 <= 2) %>%
  dplyr::mutate(drought_year = as.integer(drought_year)) %>%
  dplyr::select(x,y,drought_year,mean_precip,mean_temp,compensation_abs_2,drought_sens)

#lists to store iterations
cor_list_temp <- list()
cor_list_precip <- list()
cor_list_comp <- list()

for(i in 1:10000){

compensation_iteration <- splitstackshape::stratified(indt = ngp_stack_bootstrapping,group = "drought_year", size = 0.1)

cor_list_precip[[i]] <- cor(compensation_iteration$mean_precip,compensation_iteration$drought_sens,method = 'spearman')
cor_list_comp[[i]] <- cor(compensation_iteration$compensation_abs_2,compensation_iteration$drought_sens,method = 'spearman')
cor_list_temp[[i]] <- cor(compensation_iteration$mean_temp,compensation_iteration$drought_sens,method = 'spearman')

rm(compensation_iteration)

}


#turn to dataframes
cor_list_precip_df <- data.frame(do.call('rbind',cor_list_precip)) %>%
  dplyr::mutate(var = 'Mean annual precipitation') %>%
  dplyr::rename(cor = do.call..rbind...cor_list_precip.)

cor_list_comp_df <- data.frame(do.call('rbind',cor_list_comp)) %>%
  dplyr::mutate(var = 'Spring compensation') %>%
  dplyr::rename(cor = do.call..rbind...cor_list_comp.)

cor_list_temp_df <- data.frame(do.call('rbind',cor_list_temp)) %>%
  dplyr::mutate(var = 'Mean annual temperature') %>%
  dplyr::rename(cor = do.call..rbind...cor_list_temp.)

cor_list_df <- rbind(cor_list_precip_df,cor_list_comp_df,cor_list_temp_df)

#remove
rm(cor_list_precip_df,cor_list_comp_df,cor_list_temp_df,
   cor_list_precip,cor_list_comp,cor_list_temp)

#plot it out
#first make a df for the 'global' correlation estimate that does not
#randomize/subset to address spatial autocorrelation/year effects

vline_data <- data.frame(
  var= c("Mean annual precipitation", "Spring compensation", 
         "Mean annual temperature"),
  intercept = c(0.15, -.74, 0.33)
)

#save
png(height = 1000,width=3100,res=300,'figures/correlation_boostraps.png')

ggplot(cor_list_df,aes(x = cor)) +
  facet_wrap(~var,scales = 'free') +
  geom_histogram(fill = 'white',color = 'black') +
  geom_vline(data = vline_data, aes(xintercept = intercept), 
             color = "red", size = 1) +
  xlab('Correlation with drought sensitivity') +
  ylab('Count') +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(color = 'black', size = 9),
    axis.text.y = element_text(color = 'black', size = 11),
    axis.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


dev.off()

#cleanup
rm(vline_data,cor_list_df,i)

#now look at correlations within each drought year
within_drought_years <- ngp_stack_bootstrapping %>%
  dplyr::group_by(drought_year) %>%
  summarise(cor_precip = cor(mean_precip,drought_sens, method = 'spearman'),
            cor_temp = cor(mean_temp,drought_sens, method = 'spearman'),
            cor_comp = cor(compensation_abs_2,drought_sens),
            `Number of pixels` = length(drought_year),.groups = "drop") %>%
  tidyr::pivot_longer(cols = starts_with("cor_"),
                      names_to = "var",
                      values_to = "correlation") %>%
  dplyr::mutate(var = case_match(
    var,
    "cor_precip" ~"Mean annual precipitation",
    "cor_temp" ~"Mean annual temperature",
    "cor_comp" ~"Spring compensation",
    
  )) %>%
  dplyr::mutate(var = factor(var,levels = c("Spring compensation",
                                            "Mean annual precipitation",
                                            "Mean annual temperature"))) %>%
  dplyr::mutate(correlation = round(correlation,2))

#first look
summary(within_drought_years)

outlier <- within_drought_years[within_drought_years$correlation == 0.58, ]

#save
png(height = 1500,width=2000,res=300,'figures/correlation_each_drought_year.png')

within_drought_years %>% dplyr::filter(correlation < 0.58) %>% #remove then re-add as outlier
ggplot(aes(x = var, y = correlation)) +
  geom_hline(yintercept = 0) +
  #geom_point(aes(size=no_pixels),alpha=.2,geom_jitter(width = 0.01)) +
  geom_boxplot(outliers = F) + 
  geom_jitter(width = 0.05,aes(size=`Number of pixels`),alpha=.2) +
  xlab('') +
  geom_point(data = outlier, aes(x = var, y = correlation), color = "red", size =1.5,alpha=.2) +
  ylab('Correlation with drought sensitivity') +
  theme(
    axis.text.x = element_text(color = 'black', size = 11),
    axis.text.y = element_text(color = 'black', size = 11),
    axis.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = 'top',
    #legend.position = c(0.5,0.9),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


dev.off()

#cleanup
rm(ngp_stack_bootstrapping,outlier,within_drought_years)


#-----------------------------------------------------------

