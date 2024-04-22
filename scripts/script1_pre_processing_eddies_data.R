
############################### Library
library(tidyr)
library(dplyr)
library(geosphere)
library(data.table)
library(ggplot2)
library(ggforce)
library(ncdf4)

########################################################################################
#################################### download data ####################################
########################################################################################

name_fich <- paste0("data_eddy/META3.2_DT_allsat_Cyclonic_long_19930101_20210802.nc") ## these data needed to be dowloaded on AVISO

var_ncdf <- nc_open(name_fich)
time <- ncvar_get(var_ncdf, 'time')

time2 <- as.POSIXct(time*60*60*24, origin = "1950-01-01",tz = "UTC")
range(time2)

index_time <- seq(1, length(time2), 1)

latitude <- ncvar_get(var_ncdf, 'latitude')
longitude <- ncvar_get(var_ncdf, 'longitude')

range(longitude)
longitude[longitude < 0] <- longitude[longitude < 0] + 360
longitude[longitude > 360] <- longitude[longitude > 360] - 360
longitude[longitude > 180] <- longitude[longitude > 180] - 360
range(longitude)

observation_number <- ncvar_get(var_ncdf, "observation_number")

speed_average <- ncvar_get(var_ncdf, "speed_average")
speed_radius <- ncvar_get(var_ncdf, 'speed_radius')
speed_area <- ncvar_get(var_ncdf, 'speed_area')

effective_radius <- ncvar_get(var_ncdf, 'effective_radius')
effective_area <- ncvar_get(var_ncdf, "effective_area")

track <- ncvar_get(var_ncdf, 'track')
amplitude <- ncvar_get(var_ncdf, 'amplitude')


df_eddy1 <- data.frame(index_time = index_time,
                       date = time2,
                       lon = longitude,
                       lat = latitude,
                       cyclonic_type = 'Cyclonic',
                       obs_number = observation_number,
                       speed_average = speed_average,
                       speed_radius = speed_radius,
                       speed_area = speed_area,
                       effective_radius = effective_radius,
                       effective_area = effective_area,
                       amplitude = amplitude,
                       track_num = track)
df_eddy1$year <- year(df_eddy1$date)
dim(df_eddy1)

df_eddy1 <- data.table(df_eddy1)

###########################   anticyclonic
name_fich2 <- paste0("data_eddy/META3.2_DT_allsat_Anticyclonic_long_19930101_20210802.nc")  ## these data needed to be dowloaded on AVISO

var_ncdf2 <- nc_open(name_fich2)

time <- ncvar_get(var_ncdf2, 'time')

time3 <- as.POSIXct(time*60*60*24, origin = "1950-01-01",tz = "UTC")
range(time3)

index_time2 <- seq(1, length(time3), 1)

###### et le reste

latitude <- ncvar_get(var_ncdf2, 'latitude')
longitude <- ncvar_get(var_ncdf2, 'longitude')

range(longitude)
longitude[longitude < 0] <- longitude[longitude < 0] + 360
longitude[longitude > 360] <- longitude[longitude > 360] - 360
longitude[longitude > 180] <- longitude[longitude > 180] - 360
range(longitude)


observation_number <- ncvar_get(var_ncdf2, "observation_number")

speed_average <- ncvar_get(var_ncdf2, "speed_average")
speed_radius <- ncvar_get(var_ncdf2, 'speed_radius')
speed_area <- ncvar_get(var_ncdf2, 'speed_area')

effective_radius <- ncvar_get(var_ncdf2, 'effective_radius')
effective_area <- ncvar_get(var_ncdf2, "effective_area")

track <- ncvar_get(var_ncdf2, 'track')
amplitude <- ncvar_get(var_ncdf2, 'amplitude')

df_eddy2 <- data.frame(index_time = index_time2,
                       date = time3,
                       lon = longitude,
                       lat = latitude,
                       cyclonic_type = 'Anticyclonic',
                       obs_number = observation_number,
                       speed_average = speed_average,
                       speed_radius = speed_radius,
                       speed_area = speed_area,
                       effective_radius = effective_radius,
                       effective_area = effective_area,
                       amplitude = amplitude,
                       track_num = track)

df_eddy2$year <- year(df_eddy2$date)
dim(df_eddy2)

df_eddy2 <- data.table(df_eddy2)

########################### anticylconic and cyclonic

df_eddy_tot <- rbind(df_eddy1,df_eddy2)
df_eddy_tot$index_unique_eddy <- seq(1, dim(df_eddy_tot)[1], 1)

df_eddy_tot <- df_eddy_tot %>%
  group_by(track_num, cyclonic_type) %>%
  mutate( track_num_TRUE = cur_group_id()  )

df_eddy_tot$date_char <- as.character(df_eddy_tot$date)


df_eddy_sampled <- df_eddy_tot %>% 
  dplyr::filter(index_unique_eddy %in% liste_eddy_sampled)

save(file = 'C:/aurore/eddy_micronecton/data/raw/df_eddy_sampled.Rdata',
     df_eddy_sampled)

########################### eddy borders  ########################### 

index_contour <- c(1: 20)

list_variable <- c( 'effective_contour_longitude',  'effective_contour_latitude',
                    'speed_contour_longitude',  'speed_contour_latitude')

# for(rep_variable in list_variable){
#   print(rep_variable)
# 
#   contour <- ncvar_get(var_ncdf, rep_variable)
#   dimnames(contour) <- list(index_contour, index_time)
#   contour2 <- contour[, list_eddy_cyclonic] ## liste of clyclonic eddies really sampled to save a smaller dataset
# 
#   contour_anti <- ncvar_get(var_ncdf2, rep_variable)
#   dimnames(contour_anti) <- list(index_contour, index_time2)
#   contour_anti2 <- contour_anti[, list_eddy_anticyclonic] ## liste of anticlyclonic eddies really sampled to save a smaller dataset
# 
# 
#   save(file = paste0('data_R/2df_', rep_variable,'.Rdata'),
#        contour2)
#   save(file = paste0('data_R/2df_anti_', rep_variable,'.Rdata'),
#        contour_anti2)
# 
# }


for(rep_variable in list_variable){
  print(rep_variable)
  load(file = paste0('data_R/2df_', rep_variable,'.Rdata'))
  df_cont <- as.data.table(reshape2::melt(contour2))
  names(df_cont) <- c('index_contour', 'index_time', rep_variable)
  df_cont$cyclonic_type <- 'Cyclonic'
  
  if(rep_variable == list_variable[1]){
    assign('df_contour_tot_cycl', df_cont, .GlobalEnv) } else {
      df_contour_tot_cycl <- merge(df_contour_tot_cycl, df_cont) }
  
  load(file = paste0('data_R/2df_anti_', rep_variable,'.Rdata'))
  df_cont_anti <- as.data.table(reshape2::melt(contour_anti2))
  names(df_cont_anti) <- c('index_contour', 'index_time', rep_variable)
  df_cont_anti$cyclonic_type <- 'Anticyclonic'
  
  if(rep_variable == list_variable[1]){
    assign('df_contour_tot_anti', df_cont_anti, .GlobalEnv) } else {
      df_contour_tot_anti <- merge(df_contour_tot_anti, df_cont_anti) }
  
  
}

df_contour_tot_anti
df_contour_tot_cycl

df_contour_tot_end <- rbind(df_contour_tot_cycl, df_contour_tot_anti)
df_contour_tot_end <- data.frame(df_contour_tot_end)

range(df_contour_tot_end$effective_contour_longitude)
df_contour_tot_end$effective_contour_longitude[df_contour_tot_end$effective_contour_longitude < 0] <- df_contour_tot_end$effective_contour_longitude[df_contour_tot_end$effective_contour_longitude < 0] + 360
df_contour_tot_end$effective_contour_longitude[df_contour_tot_end$effective_contour_longitude > 360] <- df_contour_tot_end$effective_contour_longitude[df_contour_tot_end$effective_contour_longitude > 360] - 360
df_contour_tot_end$effective_contour_longitude[df_contour_tot_end$effective_contour_longitude > 180] <- df_contour_tot_end$effective_contour_longitude[df_contour_tot_end$effective_contour_longitude > 180] - 360

range(df_contour_tot_end$speed_contour_longitude)
df_contour_tot_end$speed_contour_longitude[df_contour_tot_end$speed_contour_longitude < 0] <- df_contour_tot_end$speed_contour_longitude[df_contour_tot_end$speed_contour_longitude < 0] + 360
df_contour_tot_end$speed_contour_longitude[df_contour_tot_end$speed_contour_longitude > 360] <- df_contour_tot_end$speed_contour_longitude[df_contour_tot_end$speed_contour_longitude > 360] - 360
df_contour_tot_end$speed_contour_longitude[df_contour_tot_end$speed_contour_longitude > 180] <- df_contour_tot_end$speed_contour_longitude[df_contour_tot_end$speed_contour_longitude > 180] - 360

df_contour_tot_end <- merge(data.table(df_contour_tot_end), data.table(df_for_complete_eddy)) 

df_contour_sampled <- df_contour_tot_end %>% 
  dplyr::filter(index_unique_eddy %in% liste_eddy_sampled)

save(file = 'C:/aurore/eddy_micronecton/data/raw/df_contour_sampled.Rdata',
     df_contour_sampled)
