
#code for making main figures

#load packages
library_vector <- c('tidyverse','terra','scico')
lapply(library_vector,library,character.only = TRUE)
rm(library_vector)

#---------------code----------------------------------------
#### make plot for cross-biome analysis of phenocam greenness data ####


#test import of files
phenocam_files_summarized <- list.files('data/phenocam/summarized_data/', pattern = '.csv')

#loop through each file 
temp_list <- list()
for(i in phenocam_files_summarized){
  
  temp_file <- read.csv(paste0('data/phenocam/summarized_data/',i))
  temp_file$vegetation <- substr(i,str_length(i) - 26,str_length(i) - 25)
  temp_list[[i]] <- temp_file
  
}

#combine list elements into a single dataframe 
phenocam_drought_phenology_db_gr <- do.call(rbind,temp_list) 

#remove
rm(temp_list,temp_file,i,phenocam_files_summarized)

#filter and summarize data
phenocam_drought_phenology_db_gr_temperate <- phenocam_drought_phenology_db_gr %>%
  dplyr::group_by(site) %>%
  dplyr::filter(smooth_gcc_90_mean == max(smooth_gcc_90_mean)) %>%
  dplyr::select(site,doy) %>%
  dplyr::rename(doy_max = doy) %>%
  dplyr::left_join(phenocam_drought_phenology_db_gr,join_by(site)) %>%
  dplyr::filter(doy_max > 152 & doy_max < 243) %>% #filters to temperate systems
  dplyr::filter(control_years > 3) %>% #need at least 4 control yrs
  dplyr::filter(vegetation %in% c("DB" ,"GR")) %>% #focus on grasslands & forests
  na.exclude() %>%
  dplyr::filter(drought_years > 0) %>%
  dplyr::mutate(drought_impact_rel = ((smooth_gcc_90_drought - smooth_gcc_90_mean)/smooth_gcc_90_mean)*100)

#remove large initial dataset
rm(phenocam_drought_phenology_db_gr)

#DOY for seasons
#spring: 61-152
#summer: 153-244

#number of unique sites to analyze 
#length(unique(phenocam_drought_phenology_db_gr_temperate$site))

#med sites/climates
#forbes,jasperridge, kamuela (hawaii),vaira,tonzi,waahaila (hawaii)

# quick look at average curves to ensure a single, temperate seasonal cycle
# ggplot(phenocam_drought_phenology_db_gr_temperate ,aes(doy,smooth_gcc_90_mean, color=vegetation)) +
#   geom_smooth(method = 'loess') +
#   geom_point(size=.1) +
#   facet_wrap(~site,scale = 'free') 

#create date variable
phenocam_drought_phenology_db_gr_temperate$doy_2 <- 
  as.Date(phenocam_drought_phenology_db_gr_temperate$doy)

#make the inset plot
phenocam_main_impact_inset_plot <- ggplot(phenocam_drought_phenology_db_gr_temperate,
                                          aes(doy_2,drought_impact_absolute)) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess',color='black') +
  stat_summary(fun = 'mean',geom = 'point',size=.05,alpha=0.5) +
  xlab('') +
  ylab('Drought impact') +
  scale_x_date(date_labels = "%b", breaks = "month") +
  theme(
    axis.text.x = element_text(color = 'black', size = 5,angle = 30),
    axis.text.y = element_text(color='black',size=5),
    axis.title.x = element_text(color='black',size=7),
    axis.title.y = element_text(color='black',size=7),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.80),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# main panel split up by biome type
phenocam_impact_by_veg_plot <- 
  ggplot(phenocam_drought_phenology_db_gr_temperate,aes(doy_2,drought_impact_absolute, color=vegetation)) +
  geom_hline(yintercept = 0) +
  annotate('text',x=as.Date("1970-08-01"), y=0.00025, label="Multi-year Average",size=2.1) +
  scale_colour_manual(values=c('DB'='blue','GR'='red'),
                      labels=c('DB'='Forest','GR'='Grassland')) +
  stat_summary(fun = 'mean',geom = 'point',size=.05) +
  geom_smooth(method = 'loess') +
  xlab('') +
  ylab('Drought impact to greenness') +
  geom_hline(yintercept = 0) +
  xlab('') +
  ylab('Drought impact to greenness (GCC)') +
  scale_x_date(date_labels = "%b", breaks = "month") +
  geom_hline(yintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=11),
    axis.text.y = element_text(color='black',size=11),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=13),
    legend.position = c(0.85,0.2),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#make inset
vp <- grid::viewport(width = 0.3, height = 0.3, x = 0.27,y=0.25)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(phenocam_impact_by_veg_plot)
  print(phenocam_main_impact_inset_plot, vp = vp)
}

#save
png(height = 1700,width=2100,res=300,'figures/drought_gcc_grasslands_forest.png')

full()

dev.off()

#remove
rm(vp,phenocam_impact_by_veg_plot,phenocam_main_impact_inset_plot,full)

#summarize the data for supporting table
#head(drought_phenology_db_gr_temperate,1)

phenocam_summary_table <- phenocam_drought_phenology_db_gr_temperate %>%
  dplyr::group_by(site,vegetation) %>%
  dplyr::summarise(control_years = mean(control_years),
            drought_year = mean(drought_years),
            MAP = mean(map),
            MAT = mean(mat))

write.csv(phenocam_summary_table,'figures/phenocam_summary_table.csv')

#take a look at add in site coordinate for final table
#look <- read.csv('figures/phenocam_summary_table.csv')

# look %>% group_by(vegetation) %>% 
#   summarise(sites = length(vegetation))

#add in site coordinates to the table
# summary_table <- read.csv('figures/phenocam_summary_table.csv')
# metadata <- read.csv('data/phenocam/metadata.csv') %>%
#   dplyr::select(site,lat,lon) %>%
#   dplyr::right_join(summary_table,join_by(site)) %>%
#   dplyr::mutate(MAP = round(MAP,2),
#                 MAT = round(MAT,2))
# 
# cor(metadata$)
# 
# #take a look
# # metadata %>% group_by(vegetation) %>% 
# #   summarise(sites = length(vegetation))
# # mean(metadata$control_years)
# 
# #save updated file
# write.csv(metadata,'figures/phenocam_summary_table.csv')

#remove
rm(look,metadata,phenocam_drought_phenology_db_gr_temperate,summary_table,
   phenocam_summary_table)

#-----------------------------------------------------------

#### make plot for single-site central MT, USA drought impact ####

#import
ap_gpp_data <- read.csv('data/ap/ap_modis_gpp_2.csv')
#head(ap_gpp,1)

#compare 2017 gpp to long-term mean gpp
ap_mean <- ap_gpp_data %>%
  dplyr::filter(year %in% 2000:2023) %>%
  dplyr::filter(!year %in% 2017) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise(mean_gpp = mean(gpp_mean))

#join with just 207 to calculate deviations from the mean
ap_mean <- ap_gpp_data %>%
  dplyr::filter(year == 2017) %>%
  dplyr::left_join(ap_mean,join_by(doy)) %>%
  dplyr::mutate(gpp_dev = gpp_mean - mean_gpp) %>%
  dplyr::mutate(Month = lubridate::month(calendar_date))

#carbon uptake plot
ap_drought_anom_plot <- 
  ggplot(ap_mean, aes(x = as.Date(calendar_date), y = gpp_dev)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point(pch=21,fill = 'white',size=3.5) +
  xlab("") +
  scale_x_date(date_labels = "%b", breaks = "month") +
  ylab(
    expression("Drought impact to carbon uptake "(g~C~m^-2~'8 days'^-1))) +
  annotate('text',x=as.Date("2017-04-23"), y=0.35, label="Long-term Average",size=3) +
  theme(
    axis.text.x = element_text(color = 'black', size = 11),
    axis.text.y = element_text(color = 'black', size = 11),
    axis.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#simple barplot of npp difference between 2017 and mean

#import
ap_npp_data <- read.csv('data/ap/ap_modis_npp_2.csv')

ap_npp_data %>%
  dplyr::filter(!year %in% 2017) %>%
  dplyr::summarise(mean_npp = mean(npp),
                   sd = sd(npp))


ap_npp_impact <- data.frame(
  
  npp = c(193.34,191.73),
  trt = c('Extreme Drought',"Long-term Average"),
  sd = c(0,30.43)
  
)

#make barplot of this for inset
ap_drought_barplot <- 
  ggplot(ap_npp_impact , aes(x = trt, y = npp)) +
  stat_summary(geom = 'bar',fun = 'mean',color='black') +
  geom_errorbar(aes(ymin = npp - sd, 
                    ymax = npp + sd), width = 0.05) +
  scale_y_continuous(expand=c(0,0),limit = c(0,223)) +
  xlab("") +
  ylab(bquote('Net primary productivity ('*'g C'~ m^-2*')')) +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    axis.text.y = element_text(color = 'black', size = 8),
    axis.title = element_text(color = 'black', size = 9),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#make inset plot
#try to make inset
vp <- grid::viewport(width = 0.44, height = 0.39, x = 0.77,y=0.78)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(ap_drought_anom_plot)
  print(ap_drought_barplot, vp = vp)
}

ap_drought_phenology_plot <- full()

#make the temp-precip anomaly inset

#import precip-temp ata
ap_precip_temp <- read.csv('data/ap/daymet_precip_temp_ap.csv')

#2017 identified as driest year
ap_weather_2017 <- ap_precip_temp %>%
  dplyr::filter(year == 2017)

ap_temp_summaries <- ap_precip_temp %>%
  dplyr::filter(!year %in% 2017) %>%
  dplyr::group_by(yday) %>%
  dplyr::mutate(mean_temp = (tmax..deg.c. + tmin..deg.c.)/2) %>%
  dplyr::summarise(mean_temp = mean(mean_temp),
                   mean_precip = mean(prcp..mm.day.)) %>%
  dplyr::left_join(ap_weather_2017[c('yday','tmax..deg.c.','prcp..mm.day.')],join_by(yday)) %>%
  dplyr::mutate(temp_anamoly = tmax..deg.c. - mean_temp,
                precip_anamoly = prcp..mm.day. - mean_precip)

#monthly total precip anoms
ap_monthly_precip <- ap_temp_summaries %>% 
  group_by(month = lubridate::floor_date(as.Date(yday), 'month')) %>%
  summarize(monthly_precip_anoms = sum(precip_anamoly))

#slightly re-do this one
ap_temp_anom_plot_inset <- 
  ggplot(ap_temp_summaries, aes(x = as.Date(yday), y = temp_anamoly)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth=0.25,color='red') +
  xlab("") +
  scale_x_date(date_labels = "%b", breaks = "month") +
  ylab(expression('Daily anomaly ('*~degree*C*')')) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1, name = "Monthly anomaly (mm)")) +
  geom_line(data = ap_monthly_precip,aes(x=as.Date(month),y = monthly_precip_anoms),
            color='blue',linewidth=0.25) +
  geom_point(data = ap_monthly_precip,aes(x=as.Date(month),y = monthly_precip_anoms),
             size=.5,pch=19) +
  theme(
    axis.text.x = element_text(color = 'black', size = 5,angle = 30),
    axis.text.y = element_text(color = 'black', size = 5),
    axis.title.y.left = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#insert second inset
vp_2 <- grid::viewport(width = 0.35, height = 0.33, x = 0.27,y=0.26)

#executing the inset, you create a function the utlizes all the previous code
full_2 <- function() {
  full()
  print(ap_temp_anom_plot_inset , vp = vp_2)
}

full_2()

#save
png(height = 1700,width=2100,res=300,'figures/nmp_drought_impact_inset.png')

full_2()

dev.off()

#quick estimate of 47% difference in 2017 versus mean
# precip_look <- ap_precip_temp %>% group_by(year) %>% 
#   summarise(annual_precip = sum(prcp..mm.day.)) 
# 
# (mean(precip_look$annual_precip)-175.57)/mean(precip_look$annual_precip)
# 
# 
# #climate normals
# precip_temp %>%
#   dplyr::mutate(mean_daily_temp = (tmax..deg.c. + tmin..deg.c.)/2) %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(annual_temp = mean(mean_daily_temp),
#                    annual_precip = sum(prcp..mm.day.)) %>% 
#   summary()

#remove
rm(ap_mean,ap_monthly_precip,ap_npp_data,ap_npp_impact,ap_precip_temp,
   ap_temp_anom_plot_inset,ap_temp_summaries,ap_weather_2017,vp,vp_2,
   ap_drought_anom_plot,ap_drought_barplot,ap_drought_phenology_plot,
   ap_gpp_data)

#-----------------------------------------------------------

#### make plot for single-site northern CO, USA drought impact ####

#import
cper_gpp_data <- read.csv('data/sgs/sgs_modis_gpp_2.csv')
#head(ap_gpp,1)

#compare 2017 gpp to long-term mean gpp
cper_mean <- cper_gpp_data %>%
  dplyr::filter(year %in% 2000:2023) %>%
  dplyr::filter(!year %in% 2022) %>%
  dplyr::group_by(doy) %>%
  dplyr::summarise(mean_gpp = mean(gpp_mean))

cper_mean <- cper_gpp_data %>%
  dplyr::filter(year == 2022) %>%
  dplyr::left_join(cper_mean,join_by(doy)) %>%
  dplyr::mutate(gpp_dev = gpp_mean - mean_gpp) %>%
  dplyr::mutate(Month = lubridate::month(calendar_date))

# make a nice ggplot of this
cper_drought_anom_plot <- 
  ggplot(cper_mean, aes(x = as.Date(calendar_date), y = gpp_dev)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point(pch=21,fill = 'white',size=3.5) +
  xlab("") +
  scale_x_date(date_labels = "%b", breaks = "month") +
  ylab(
    expression("Drought impact to carbon uptake "(g~C~m^-2~'8 days'^-1))) +
  annotate('text',x=as.Date("2022-06-23"), y=0.35, label="Long-term Average",size=3) +
  theme(
    axis.text.x = element_text(color = 'black', size = 11),
    axis.text.y = element_text(color = 'black', size = 11),
    axis.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#simple barplot of npp difference between 2017 and mean

#import
cper_npp_data <- read.csv('data/sgs/sgs_modis_npp_2.csv')

#take the mean of all other non-drought years
cper_npp_data  %>%
  dplyr::filter(!year %in% 2022) %>%
  dplyr::summarise(mean_npp = round(mean(npp),2),
                   sd = round(sd(npp),2))


cper_npp_impact <- data.frame(
  
  npp = c(128.92,231.45),
  trt = c('Extreme Drought',"Long-term Average"),
  sd = c(0,57.44)
  
)

#44% reduction
(231.45 -128.92)/231.45

#make barplot of this for inset
cper_drought_barplot <- 
  ggplot(cper_npp_impact , aes(x = trt, y = npp)) +
  stat_summary(geom = 'bar',fun = 'mean',color='black') +
  geom_errorbar(aes(ymin = npp - sd, 
                    ymax = npp + sd), width = 0.05) +
  scale_y_continuous(expand=c(0,0),limit = c(0,293)) +
  xlab("") +
  ylab(bquote('Net primary productivity ('*'g C'~ m^-2*')')) +
  theme(
    axis.text.x = element_text(color = 'black', size = 8),
    axis.text.y = element_text(color = 'black', size = 8),
    axis.title = element_text(color = 'black', size = 8),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#make inset plot
#try to make inset
vp <- grid::viewport(width = 0.4, height = 0.35, x = 0.785,y=0.28)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(cper_drought_anom_plot)
  print(cper_drought_barplot, vp = vp)
}

cper_drought_phenology_plot <- full()


#try to add a daily GCC plot first

cper_gcc_data <- 
  read.csv('data/sgs/cperagm_GR_1000_1day.csv')

dry_normal_years <- cper_gcc_data %>%
  dplyr::filter(year %in% c(2021,2022))  %>%
  dplyr::select(year,doy,gcc_90)

dry_normal_years$doy_2 <- as.Date(dry_normal_years$doy)

cper_gcc_plot <- ggplot(dry_normal_years,aes(doy_2,gcc_90,color=as.factor(year))) +
  geom_point(size=0.25) +
  geom_line(size=0.25) +
  xlab('') +
  ylab('Daily greenness (GCC)') +
  scale_x_date(date_labels = "%b", breaks = "month") +
  scale_color_manual(values=c('2021'='darkgreen','2022'='purple'),
                     labels = c('Average','Drought')) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  theme(
    axis.text.y = element_text(color='black',size=5),
    axis.title.y = element_text(color='black',size=8.5),
    axis.line.x = element_blank(),  # Removes the x-axis line
    axis.text.x = element_blank(),  # Removes the x-axis labels (numbers/text)
    axis.ticks.y = element_line(color='black'),
    axis.ticks.x = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=6.5),
    legend.position = c(0.78,0.7),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=5),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.y = element_line(colour = "black"))

#insert
vp_2 <- grid::viewport(width = 0.27, height = 0.31, x = 0.245,y=0.5)

#executing the inset, you create a function the utlizes all the previous code
full_2 <- function() {
  full()
  print(cper_gcc_plot, vp = vp_2)
}

full_2()


#make the temp-precip anomaly inset

#import precip-temp ata
cper_precip_temp_data <- read.csv('data/sgs/sgs_daymet_precip_temp_2.csv')

#2022 identified as driest year
cper_weather_2022 <- cper_precip_temp_data %>%
  dplyr::filter(year == 2022)

cper_temp_summaries <- cper_precip_temp_data %>%
  dplyr::filter(!year %in% 2022) %>%
  dplyr::group_by(yday) %>%
  dplyr::mutate(mean_temp = (tmax..deg.c. + tmin..deg.c.)/2) %>%
  dplyr::summarise(mean_temp = mean(mean_temp),
                   mean_precip = mean(prcp..mm.day.)) %>%
  dplyr::left_join(cper_weather_2022[c('yday','tmax..deg.c.','prcp..mm.day.')],join_by(yday)) %>%
  dplyr::mutate(temp_anamoly = tmax..deg.c. - mean_temp,
                precip_anamoly = prcp..mm.day. - mean_precip)

#monthly total precip anoms
cper_monthly_precip <- cper_temp_summaries %>% 
  group_by(month = lubridate::floor_date(as.Date(yday), 'month')) %>%
  summarise(monthly_precip_anoms = sum(precip_anamoly))

#slightly re-do this one
cper_temp_anom_plot_inset <- 
  ggplot(cper_temp_summaries, aes(x = as.Date(yday), y = temp_anamoly)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth=0.25,color='red') +
  xlab("") +
  scale_x_date(date_labels = "%b", breaks = "month") +
  ylab(expression('Daily anomaly ('*~degree*C*')')) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1, name = "Monthly anomaly (mm)")) +
  geom_line(data = cper_monthly_precip,aes(x=as.Date(month),y = monthly_precip_anoms),
            color='blue',linewidth=0.25) +
  geom_point(data = cper_monthly_precip,aes(x=as.Date(month),y = monthly_precip_anoms),
             size=.5,pch=19) +
  theme(
    axis.text.x = element_text(color = 'black', size = 5,angle = 30),
    axis.text.y = element_text(color = 'black', size = 5),
    axis.title.y.left = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    axis.title.y = element_text(size = 10),
    legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#insert second inset
vp_3 <- grid::viewport(width = 0.32, height = 0.31, x = 0.27,y=0.25)

#executing the inset, you create a function the utlizes all the previous code
full_3 <- function() {
  full_2()
  print(cper_temp_anom_plot_inset , vp = vp_3)
}

full_3()

#save
png(height = 1700,width=2100,res=300,'figures/sgs_drought_impact_inset.png')

full_3()

dev.off()

#remove
rm(cper_drought_anom_plot,cper_drought_barplot,cper_drought_phenology_plot,
   cper_gcc_data,cper_gcc_data,cper_mean,cper_monthly_precip,cper_npp_data,
   cper_npp_impact,cper_precip_temp_data,cper_temp_anom_plot_inset,
   cper_temp_summaries,cper_weather_2022,dry_normal_years,vp,vp_2,vp_3,
   cper_gcc_plot,cper_gpp_data,full,full_2,full_3)

#-----------------------------------------------------------

#### make plots for entire northern great plains ecoregion ####

#import raster stack
ngp_stack <- terra::rast('data/ngp/ngp_raster_stack.tif')

#convert to spring and summer dataframes for plotting
ngp_spring <- as.data.frame(ngp_stack$percent_change_spring,xy = TRUE)
ngp_spring$season <- 'Spring'
ngp_summer <- as.data.frame(ngp_stack$percent_change_summer,xy = TRUE)
ngp_summer$season <- 'Summer'

#albers conic projection
Albers <-
  terra::crs(
    '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
  )

#try a faceting approach
head(ngp_spring,1)

#get these all into one dataframe with season as a variable
ngp_spring <- ngp_spring %>% rename(percent_change = percent_change_spring)
ngp_summer <- ngp_summer %>% rename(percent_change = percent_change_summer)
ngp_spring_summer <- rbind(ngp_spring,ngp_summer)

#get the geopolitical boundaries for context in the map
na <- geodata::gadm("GADM", country=c('USA','CAN'), level=1,download=TRUE)

na <- na[na$NAME_1 %in% c("Montana","South Dakota","Wyoming",'Nebraska',
                          "North Dakota","Alberta","Saskatchewan"),]

na <- terra::project(na,Albers)


ngp_spring_summer_plot <- ggplot(ngp_spring_summer) +
  geom_raster(aes(x = x, y = y, fill = percent_change)) +
  facet_wrap(~season) +
  tidyterra::geom_spatvector(data=na,fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(crs = Albers,
           xlim = c(-113, -98),
           ylim = c(40.2, 52),
           default_crs = sf::st_crs(4326)) +
  scico::scale_fill_scico(
    name = "Change in gross primary productivity (%)",
    palette = "vik", #distinct blue versus red divergence for negative/positive
    direction = -1,
    limits = c(-100, 100),
    midpoint = 0
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
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
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()) 

#make and save plot
png(height = 1700,width=2100,res=300,'figures/spring_summer_gpp_facets.png')

ngp_spring_summer_plot

dev.off()

#now make bivariate plot

ngp_df <- as.data.frame(ngp_stack,xy=T) %>%
  dplyr::mutate(drought_sens = (((mean_npp - drought_npp)/mean_npp)*100)/(mean_precip - drought_precip)) %>%
  dplyr::mutate(drought_sens_abs = ((mean_npp - drought_npp))/(mean_precip - drought_precip))

#see if abs and rel estimates are correlated
#plot(ngp_df$drought_sens,ngp_df $drought_sens_abs)
#they are very correlated

#see how many pixels have 'increased spring with decreases summer gpp
ngp_df %>%
  dplyr::filter(percent_change_spring > 0 & percent_change_summer < 0) %>%
  summarise(number = length(percent_change_spring))

#50% (49.6) of pixels experience (+spr. -sum.)
51941/104576

#see how many pixels have 'increased spring' with 'increased summed'
ngp_df %>%
  dplyr::filter(percent_change_spring > 0 & percent_change_summer > 0) %>%
  summarise(number = length(percent_change_spring))

#2.5% of pixels experience (+spr. +sum.)
2576/104576

#see how many pixels have 'decreased spring' with 'increased summed'
ngp_df %>%
  dplyr::filter(percent_change_spring < 0 & percent_change_summer > 0) %>%
  summarise(number = length(percent_change_spring))

#.1% of pixels experience (-spr. +sum.)
101/104576

#see how many pixels have 'decreased spring' with 'decreased summer'
ngp_df %>%
  dplyr::filter(percent_change_spring < 0 & percent_change_summer < 0) %>%
  summarise(number = length(percent_change_spring))

#48% of pixels experience (-spr. -sum.)
49958/104576

#see how many pixels have 'increased spring with regardless of summer gpp response
ngp_df %>%
  dplyr::filter(percent_change_spring > 0) %>%
  summarise(number = length(percent_change_spring))

#52% of pixels experiences some degree of increased spring gpp during droughtc
54517/104576

#create data table for inset
comp_inset <-
  
  data.frame(
    
    scenario = c('+spr. -sum.','-spr. -sum.','+spr. +sum.','-spr. +sum.'),
    percent = c(50,48,2.5,0.1)
    
  )

#compare spatial variabilty in spring versus summer responses
sd(ngp_df$percent_change_spring)
sd(ngp_df$percent_change_summer)

#filter to when summer gpp is reduced ot isolate spring effect
compensation_look <- ngp_df %>%
  dplyr::filter(percent_change_summer < 0 & percent_change_spring > 0) %>%
  dplyr::mutate(compensation_abs = ((drought_spring_gpp - mean_spring_gpp) - (drought_summer_gpp - mean_summer_gpp))*.01) %>%
  dplyr::mutate(compensation_rel = percent_change_spring - percent_change_summer) %>%
  dplyr::mutate(compensation_abs_2 = abs(((drought_spring_gpp - mean_spring_gpp)/(drought_summer_gpp - mean_summer_gpp)))) %>%
  dplyr::filter(compensation_abs_2 <= 2) #right-truncate because values go into the thousands 

#find value to truncate to for the figure 
quantile(compensation_look$compensation_abs_2,0.99)

#note differences mean and range of change and there a higher spatial variation in spring responses
summary(ngp_df$percent_change_spring)
summary(ngp_df$percent_change_summer)

sd(ngp_df$percent_change_spring)
sd(ngp_df$percent_change_summer)

# hist(ngp_df$percent_change_spring,col='red')
# hist(ngp_df$percent_change_summer,add=TRUE,col='blue')

#correlation spring gpp change and annual npp change. Truncate to 95th quantile for visualizing
npp_spring_gpp_plot <- 
  ggplot(compensation_look,aes(compensation_abs_2,drought_sens_abs,fill = mean_precip)) +
  geom_point(alpha=.1,pch=21,size=1) +
  scale_fill_scico('MAP (mm)',
                   palette = 'roma',direction = 1,midpoint = 409) +
  geom_smooth(method = 'gam',color='black',size=0.5) +
  #ylab(bquote('Annual drought sensitivity ('*'%'~ mm^-1*')')) +
  ylab(bquote('Annual drought sensitivity ('*'g C'~ m^-2~ mm^-1*')'))  +
  xlab ('Spring compensation') +
  theme(
    axis.text.x = element_text(color = 'black', size = 11),
    axis.text.y = element_text(color = 'black', size = 11),
    axis.title = element_text(color = 'black', size = 13),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.key.width = unit(.33, 'cm'),
    legend.position = c(0.15,0.1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    legend.direction = "horizontal",
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#make barplot of this for inset

#define order
comp_inset$scenario <- factor(comp_inset$scenario, 
                              levels = c('+spr. -sum.','-spr. -sum.','+spr. +sum.','-spr. +sum.'))

ngp_comp_barplot <- 
  ggplot(comp_inset, aes(x = scenario, y = percent)) +
  stat_summary(geom = 'bar',fun = 'mean',color='black') +
  scale_y_continuous(expand=c(0,0),limit = c(0,51)) +
  xlab("") +
  ylab('Ocurrence (% of droughts)') +
  theme(
    axis.text.x = element_text(color = 'black', size = 5),
    axis.text.y = element_text(color = 'black', size = 5),
    axis.title = element_text(color = 'black', size = 7),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#make inset plot
#try to make inset
vp <- grid::viewport(width = 0.44, height = 0.39, x = 0.72,y=0.75)

#executing the inset, you create a function that uses all the previous code
full <- function() {
  print(npp_spring_gpp_plot)
  print(ngp_comp_barplot, vp = vp)
}

ngp_comp_plot <- full()

#save
png(height = 1200,width=1500,res=300,'figures/compensation_plot.png')

full()

dev.off()


#quick look at correlations
# plot(compensation_look$mean_precip,compensation_look$drought_sens)
# cor(compensation_look$mean_precip,compensation_look$drought_sens,method = 'spearman')
# cor(compensation_look$compensation_abs_2,compensation_look$drought_sens,method = 'spearman')
# plot(compensation_look$mean_temp,compensation_look$drought_sens)
# cor(compensation_look$mean_temp,compensation_look$drought_sens,method = 'spearman')

#remove
rm(comp_inset,compensation_look,na,ngp_comp_barplot,ngp_comp_plot,ngp_df,
   ngp_spring,ngp_spring_summer,ngp_spring_summer_plot,
   ngp_stack,ngp_summer,npp_spring_gpp_plot,vp,Albers,full)

#---------------done----------------------------------------
