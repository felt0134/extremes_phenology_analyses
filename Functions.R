

#functions to use in the analyses

#---------------code----------------------------------------
# Downloading daymet climate data 1994-2023 ####

get_daymet <- function(temp_lat,temp_lon){

#use to get annual precip and temp for a subset of the data ----

precip_temp <- daymetr::download_daymet(
  lat = temp_lat,
  lon = temp_lon,
  start = 1980,
  end = 2023
) %>%
  
  .$data %>%
  
  # #--- get date from day of the year ---#
  # mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
  
  dplyr::mutate(mean_temp = (tmax..deg.c. + tmin..deg.c.)/2) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    annual_precip = sum(prcp..mm.day.),
    annual_temp = mean(mean_temp),
    average_max_temp = mean(tmax..deg.c.),
    average_min_temp = mean(tmin..deg.c.))

#get monthly precip and temperature slope to assess seasonaloty dynamics
monthly_precip_temp_slope <- daymetr::download_daymet(
  lat = temp_lat,
  lon = temp_lon,
  start = 1980,
  end = 2023
) %>%
  
  .$data %>%
  dplyr::mutate(mean_temp = (tmax..deg.c. + tmin..deg.c.)/2) %>%
  dplyr::mutate(month = as.Date(paste(year,yday),format="%Y %j")) %>%
  dplyr::mutate(month = format(month,"%m")) %>%
  dplyr::group_by(year,month) %>%
  dplyr::summarise(
    monthly_precip = sum(prcp..mm.day.),
    monthly_mean_temp = mean(mean_temp),
    monthly_max_temp = mean(tmax..deg.c.),
    monthly_min_temp = min(tmin..deg.c.)) %>%
  dplyr::group_by(year) %>%
  do(model = lm(monthly_precip ~ monthly_mean_temp, data = .)) %>%
  dplyr::mutate(p_t_slope = coef(model)[2]) %>%
  dplyr::select(year,p_t_slope) %>%
  dplyr::right_join(precip_temp,by = join_by(year)) %>%
  dplyr::select(year,annual_precip:average_min_temp,p_t_slope)

return(monthly_precip_temp_slope)

}




#-----------------------------------------------------------
# Get initial sites list ----

sites_filtered_function <- function(){

sites <- read.csv('data/phenocam/metadata.csv')

#filter to sits with at least five years of data (as a first pass)
#round to two decimal points
sites_filtered <- sites %>%
  dplyr::mutate(years = as.numeric(difftime(as.Date(date_end), as.Date(date_start), 
                                            unit="weeks"))/52.25) %>%
  dplyr::mutate(years = round(years,2)) %>%
  dplyr::filter(years >= 5) %>%
  dplyr::filter(MAP_daymet != 'NA') 

}


#-----------------------------------------------------------
# function to create raster stack from large dataframe ####
create_raster <- function(data,var){
  
  #trim to single var so it is an xyz and join to a dataset known to be on a regular grid
  df_trimmed <- master_df %>% select(x,y,var) %>%
    dplyr::right_join(minimum_year_df,join_by(x,y),
                      relationship = "many-to-many") %>%
    dplyr::select(x,y,var) %>%
    na.omit()
  
  #create a spatvector object
  df_trimmed_points <- vect(df_trimmed, geom = c("x", "y"), crs = "EPSG:4326")
  
  # Create empty raster grid (0.023 degrees = ~2.56 km)
  df_trimmed_template <- rast(ext(df_trimmed_points), resolution = 0.023, crs = "EPSG:4326")
  
  # Rasterize each subset
  df_trimmed_raster <- rasterize(df_trimmed_points, df_trimmed_template, field = var, fun = "mean")
  
  #now final mask by land cover
  igbp_points <- vect(igbp_grasslands, geom = c("x", "y"), crs = "EPSG:4326")
  
  # Define resolution (e.g., 0.01 degrees ~ 1km)
  
  # Create empty raster template
  igbp_template <- rast(ext(igbp_points), resolution = 0.023, crs = "EPSG:4326")
  
  # Rasterize the points using the 'value' column
  igbp_grasslands <- rasterize(igbp_points, igbp_template, field = "Ground_Type", fun = "mean")
  
  #filter to just grasslands 
  igbp_grasslands <- ifel(igbp_grasslands$mean == 10,igbp_grasslands,NA)
  
  df_trimmed_raster_resampled <- terra::resample(df_trimmed_raster,igbp_grasslands,'bilinear')
  
  #mask (AKA filter) gpp pixels by which pixels are grasslands
  df_trimmed_raster_resampled_masked <- terra::mask(df_trimmed_raster_resampled,igbp_grasslands)
  
  #albers conic projection
  Albers <-
    crs(
      '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
    )
  
  #reproject gpp raster to albers conic projection
  df_trimmed_raster_resampled_masked <-
    terra::project(df_trimmed_raster_resampled_masked,Albers,'bilinear')
  
  return(df_trimmed_raster_resampled_masked)
  
}
#---------------done----------------------------------------