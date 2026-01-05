
#code for acquiring and formatting data used for analyses 

source('Functions.R')

#libraries needed
library_vector <- c('phenocamr','tidyverse','daymetr','terra','MODISTools')
lapply(library_vector,library,character.only = TRUE)
rm(library_vector)

#---------------code----------------------------------------

# Multi-site phenocam GCC data ####

#notes:
# focusing on 'temperate' sites with a clear spring and summer season
# want least five years of data
# want North American sites in order to use daymet climate data
# want to be able to know the 5th and 95th percentile of precip and temp
# so that we can use them as threshold for identifying drought years

# need a plot showing correlation of % deviation in precip and temp for years analyzed.


#explore sites and save the metadata
sites <- phenocamr::list_sites()
#head(sites,1)

#save to file to use for downloading
write.csv(sites,'data/phenocam/metadata.csv')

#initial list of sites to download data from
sites_filtered <- sites_filtered_function()

#create a site vector to iterate through in downloading and processing
sites_download_vector <- sites_filtered$site

#sites with missing data the led to loop stopping:
#bbc3, bbc4, flagstaff2,goodwaterbau, HF_Vivitek,kelloggcornsoy2,
#kellogghybridpoplar,kelloggswitchgrass, mounthood, mountzirkel,
#NEON.D01.HOPB,NEON.D02.LEWI.DP1.20002,NEON.D02.POSE.DP1.20002,
#NEON.D04.CUPE.DP1.20002,NEON.D04.GUIL.DP1.20002,NEON.D05.LIRO.DP1.20002,
#NEON.D06.MCDI.DP1.20002, ossipeelake,pasayten,sangabriel,spruceT6P16E,
#testcam3,testcamnau, warrenwilson

#download all daily gcc for all relevant sites and their ROIs. Because of 
#missing data in above sites, the loop stops, and have to pick up where it stopped, meaning I
#had to change the starting point along the site vector to resume downloading site data
for(i in sites_download_vector[471:length(sites_download_vector)]){
  
  #import and save to temp folder
  download_phenocam(site = i,
                    frequency = "1", #daily gcc
                    #roi_id = "1000", #specific ROI, it often downloads others
                    outlier_detection = TRUE,
                    smooth = TRUE,
                    contract = TRUE,
                    daymet = TRUE,
                    trim_daymet = T,
                    out_dir = 'data/phenocam/temp/')
  
}


#sites with insufficient data
#flagstaff2_GR_2000_1day,haha_UN_1000_1day.csv,honouliuli_EB_1000_1day.csv,
#juncabalejo_WL_1000_1day.csv,shahariya_EN_1000_1day,shahariya_GR_1000_1day,
#spruceT6P16E_DN_0001_1day,testcamnau_EN_1000_1day, warrenwilson_EN_3000_1day.csv

#get the 1000 ROI, some sites download everything
file <- list.files(path = 'data/phenocam/temp/')

#loop through each site and process each ROI. Because of insfficient data in above
#sites,the loop stops, and have to pick up where it stopped, meaning I
#had to change the starting point along the file vector to resume
for(i in file[777:length(file)]){
  
  #to see which file is being processed at a given time
  print(i)
  
  #extract from the file name the actual vegetation type in the ROI. You have to
  #do this because the primary vegetation type is not always what is in the ROI or file name
  vegetation <- substr(i,str_length(i) - 15,str_length(i) - 14)
  
  #read in file using the built in function
  df <- phenocamr::read_phenocam(paste0('data/phenocam/temp/',i))
  
  #data stored as list
  #str(df)
  
  #extrct main data from the list
  phenology_data <- df$data
  
  #prepare main dataframe
  phenology_data <- phenology_data %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(days = length(doy)) %>%
    dplyr::filter(days >= 365) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_years = max(year) - min(year)) %>%
    dplyr::mutate(lon = df$lon,
                  lat = df$lat,
                  site = df$site)
  
  #first extract 43-yr 1980-2024 climate normals for the location from daymet
  daymet_weather_data <- get_daymet(temp_lat = df$lat, temp_lon = df$lon)
  
  #establish a 5th precip percentile drought threshold to identify extreme years
  drought_threshold <- 
    as.numeric(quantile(daymet_weather_data$annual_precip, 0.05))
  
  #weather data provided by phenocam (still originally from daymet)
  phenocam_weather_data <- phenology_data %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(daily_temp = (tmax..deg.c. + tmin..deg.c.)/2) %>%
    dplyr::summarise(annual_temp = mean(daily_temp),
                     annual_precip = sum(prcp..mm.day.)) %>%
    dplyr::mutate(map = mean(annual_precip),
                  mat = mean(annual_temp)) %>%
    ungroup()
  
  #join daily phenology data with annualized weather data
  phenology_data <- phenology_data %>%
    dplyr::select(site,year,doy,smooth_gcc_90) %>%
    dplyr::left_join(phenocam_weather_data)
  
  #save daily data to file
  write.csv(phenology_data,
            paste0('data/phenocam/daily_data/',
                   i,'_',vegetation,'_','1000_1day_daily.csv'))
  
  #phenology during driest year. Summarize/average in case multiple extremes and rename
  phenology_extreme_drought <- phenology_data %>%
    dplyr::filter(annual_precip < drought_threshold) %>% 
    dplyr::group_by(doy) %>%
    dplyr::summarise(smooth_gcc_90_drought = mean(smooth_gcc_90),
                     drought_years = n()) #in case multiple extreme years
  
  #summarize average phenology for all years except those exceeding the threshold
  phenology_no_exteme_drought <- phenology_data %>%
    dplyr::filter(annual_precip >= drought_threshold) %>%
    dplyr::group_by(doy) %>%
    dplyr::summarise(smooth_gcc_90_mean = mean(smooth_gcc_90),
                     control_years = n())
  
  #combine and calculate the difference to get drought phenology anomoly 
  phenology_data <- phenology_no_exteme_drought %>% 
    dplyr::left_join(phenology_extreme_drought,join_by(doy)) %>%
    dplyr::mutate(drought_impact_absolute = smooth_gcc_90_drought - smooth_gcc_90_mean,
                  drought_impact_percentage = 
                    ((smooth_gcc_90_drought - smooth_gcc_90_mean)/smooth_gcc_90_mean)*100,
                  map = mean(daymet_weather_data$annual_precip),
                  mat = mean(daymet_weather_data$annual_temp),
                  site = df$site,
                  primary_veg = df$veg_type,
                  roi_veg = vegetation) %>%
    dplyr::select(site, doy, smooth_gcc_90_mean, smooth_gcc_90_drought, 
                  drought_impact_absolute, map, mat, control_years, drought_years)
  
  #save to file for analysis
  write.csv(phenology_data,
            paste0('data/phenocam/summarized_data/',
                   i,'_',vegetation,'_','1000_1day_summarized.csv'))
  
}

#test the import and visualization of files before analysis
# files_summarized <- list.files('data/phenocam/summarized_data/', pattern = '.csv')
# 
# #loop through each file 
# temp_list <- list()
# for(i in files_summarized){
#   
#   temp_file <- read.csv(paste0('data/phenocam/summarized_data/',i))
#   temp_file$vegetation <- substr(i,str_length(i) - 26,str_length(i) - 25)
#   temp_list[[i]] <- temp_file
#   
# }
# 
# #combine into one dataframe and filter to grasslands
# drought_phenology_db_gr <- do.call(rbind,temp_list) %>%
#   dplyr::filter(control_years > 3) %>%
#   dplyr::filter(vegetation %in% c("GR")) %>%
#   dplyr::filter(drought_years > 0) %>%
#   na.exclude() %>%
#   dplyr::filter(drought_years > 0) 
# 
# #preview
# head(drought_phenology_db_gr,1)
# 
# # quick look at average curve
# ggplot(drought_phenology_db_gr,aes(doy,smooth_gcc_90_mean, color=vegetation)) +
#   geom_point(size=.1) +
#   facet_wrap(~site) +
#   geom_hline(yintercept = 0)
# 
# # quick look drought response
# ggplot(drought_phenology_db_gr,aes(doy,drought_impact_absolute, color=vegetation)) +
#   stat_summary(fun = 'mean',geom = 'point') +
#   geom_hline(yintercept = 0)

#-----------------------------------------------------------

# MODIS GPP, NPP, and Daymet climate data for north-central, MT, USA location ####

#notes:
#gpp and npp data aggregated to 1 km to match daymet

# American Prairie Sun Prairie in north-central Montana, USA in Missouri Breaks
temp_lat_ap <- 47.768253
temp_lon_ap <- -107.699735

#data is for 2000-2023 (latest full year of Daymet data and beginning of MODIS)

#get annual precip and temp for single location

#import precip
precip_temp_ap <- daymetr::download_daymet(
  lat = temp_lat_ap,
  lon = temp_lon_ap,
  start = 2000,
  end = 2023
) %>%
  
  .$data 


#head(precip_temp_ap,1)

#save to file
write.csv(precip_temp_ap,'data/ap/daymet_precip_temp_ap.csv')

# get annual NPP for single location #

#get NPP data
site_npp_ap <- MODISTools::mt_subset(
  product = "MOD17A3HGF",
  lat = temp_lat_ap,
  lon =  temp_lon_ap,
  band = 'Npp_500m',
  start = "2000-01-01",
  end = "2023-12-31",
  km_lr = 1,
  km_ab = 1,
  internal = TRUE,
  progress = TRUE
)

#get npp in grams
site_npp_ap$npp <- site_npp_ap$value/10

#trim down columns
site_npp_ap <- site_npp_ap %>%
  dplyr::select(latitude,longitude,calendar_date,npp)

#get year column
site_npp_ap$year <- substr(site_npp_ap$calendar_date, 1, 4)

#average values by year
site_npp_ap <- site_npp_ap %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(npp = mean(npp))


#save to file
write.csv(site_npp_ap,'data/ap/ap_modis_npp_2.csv')

# get GPP for single location
site_gpp_ap <- MODISTools::mt_subset(
  product = "MOD17A2HGF",
  lat = temp_lat_ap,
  lon =  temp_lon_ap,
  band = 'Gpp_500m',
  start = "2000-01-01",
  end = "2023-12-31",
  km_lr = 1,
  km_ab = 1,
  internal = TRUE,
  progress = TRUE
)

#filter out bad values, get day of year, take median value for coordinate, and rescale GPP units to g/m^2
site_gpp_ap_2  <- site_gpp_ap  %>%
  dplyr::group_by(calendar_date) %>%
  dplyr::summarize(doy = as.numeric(format(as.Date(calendar_date)[1], "%j")),
                   gpp_mean = median(value * as.double(scale)))

#get gpp in grams
site_gpp_ap_2$gpp_mean <- site_gpp_ap_2$gpp_mean*1000

#get year column
site_gpp_ap_2$year <- substr(site_gpp_ap_2$calendar_date, 1, 4)

#save to file
write.csv(site_gpp_ap_2,'data/ap/ap_modis_gpp_2.csv')

#-----------------------------------------------------------

# MODIS GPP, NPP, and Daymet climate data for northern CO, MT, USA location ####

#This is effectively the same as the prior code fold for MT, USA

# cper coordinates
temp_lat_cper <- 40.8402
temp_lon_cper <- -104.7672

#data is for 2000-2023 (latest full year of Daymet data)

#get annual precip and temp for single location in Nunn,CO where CPER is

#import precip
precip_temp_cper <- daymetr::download_daymet(
  lat = temp_lat_cper,
  lon = temp_lon_cper,
  start = 2000,
  end = 2023
) %>%
  
  .$data 


#head(precip_temp,1)

#save to file
write.csv(precip_temp_cper,'data/sgs/sgs_daymet_precip_temp_2.csv')

#get annual NPP for single location

#get NPP data
site_npp_cper <- MODISTools::mt_subset(
  product = "MOD17A3HGF",
  lat = temp_lat_cper,
  lon =  temp_lon_cper,
  band = 'Npp_500m',
  start = "2000-01-01",
  end = "2023-12-31",
  km_lr = 1,
  km_ab = 1,
  #site_name = loc,
  internal = TRUE,
  progress = TRUE
)

#get npp in grams
site_npp_cper$npp <- site_npp_cper$value/10

#trim down columns
site_npp_cper <- site_npp_cper %>%
  dplyr::select(latitude,longitude,calendar_date,npp)

#get year column
site_npp_cper$year <- substr(site_npp_cper$calendar_date, 1, 4)

#average values by year
site_npp_cper <- site_npp_cper %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(npp = mean(npp))


#save to file
write.csv(site_npp_cper,'data/sgs/sgs_modis_npp_2.csv')

#get GPP for single location

site_gpp_cper <- MODISTools::mt_subset(
  product = "MOD17A2HGF",
  lat = temp_lat_cper,
  lon =  temp_lon_cper,
  band = 'Gpp_500m',
  start = "2000-01-01",
  end = "2023-12-31",
  km_lr = 1,
  km_ab = 1,
  #site_name = loc,
  internal = TRUE,
  progress = TRUE
)

#filter out bad values, get day of year, take median value for coordinate, and rescale GPP units to g/m^2
site_gpp_cper_2  <- site_gpp_cper  %>%
  dplyr::group_by(calendar_date) %>%
  dplyr::summarize(doy = as.numeric(format(as.Date(calendar_date)[1], "%j")),
                   gpp_mean = median(value * as.double(scale)))

#get gpp in grams
site_gpp_cper_2$gpp_mean <- site_gpp_cper_2$gpp_mean*1000

#get year column
site_gpp_cper_2$year <- substr(site_gpp_cper_2$calendar_date, 1, 4)

#save to file
write.csv(site_gpp_cper_2,'data/sgs/sgs_modis_gpp_2.csv')



#-----------------------------------------------------------

# Northern great plains raster stack ----

#land cover data
igbp_grasslands <- readRDS("data/ngp/Filtering_Drought_Set.rds")

#import minimum year df and select to core vars so it is an xyz
minimum_year_df <- readRDS("data/ngp/Minimum_Year_Values.rds") %>%
  dplyr::select(x,y,season) %>%
  dplyr::filter(season == 'spring') #doesn't matter season, just want consistent set of points on a regular grid

#filter to drier portions of the ecoregion 
master_df <- readRDS("data/ngp/Master_Raster (7).rds") %>%
  dplyr::filter(mean_precip < 700)

#variables to loop through
column_vec <- colnames(master_df[3:length(colnames(master_df))])

#list to store raster outputs
temp_raster_list <- list()

#loop
for(i in column_vec){
  
  temp_raster <- create_raster(master_df,i)
  
  temp_raster_list[[i]] <- temp_raster
  
}

#check
#plot(temp_raster_list[[15]])

#stack all rasters in the list into one multi-band raster
ngp_stack <- terra::rast(temp_raster_list)

#save to file
terra::writeRaster(ngp_stack,'data/ngp/ngp_raster_stack.tif')

#remove
rm(igbp_grasslands,master_df,minimum_year_df,ngp_stack,temp_raster,
   temp_raster_list,column_vec,i)


#---------------done----------------------------------------
