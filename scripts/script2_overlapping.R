############################### Library
library(tidyr)
library(dplyr)
library(geosphere)
library(data.table)
library(ggplot2)
library(ggforce)
library(sp)

############################### Functions
source('scripts/functions.R')

########################################################################################
#################################### download data ####################################
########################################################################################

############################### Acoustic data

load(file = "data/raw/df_acoustic.Rdata")
head(df_acoustic)

############################### Eddies data

load(file = 'data/raw/df_eddy_sampled.Rdata')
head(df_eddy_sampled)


df_for_complete_eddy <- data.frame(df_eddy_sampled) %>%
  dplyr::select(index_unique_eddy, track_num_TRUE, lon,lat,
                index_time, cyclonic_type,
                speed_average,speed_radius , speed_area ,
                effective_radius , effective_area ,
                obs_number,amplitude) %>%
  dplyr::mutate(lon_eddy = lon,
                lat_eddy = lat) %>%
  dplyr::select(-lon, -lat)

############################### Eddies border data

load(file = 'data/raw/df_contour_sampled.Rdata')
head(df_contour_sampled)

#######################################################################################
############### 1st : only based on the eddy center localisation ######################
#######################################################################################

df_index_acoustic  <- df_acoustic %>% 
  dplyr::group_by(source, date,index_acous , date_char, lon, lat) %>% 
  dplyr::summarise(n_depth = n_distinct(depth))
  
liste_date <- unique(df_index_acoustic$date_char)
length(liste_date) 

for(rep_date in liste_date){
  # rep_date = liste_date[2]
  print(which(rep_date == liste_date))
  temp_acou <- df_index_acoustic %>% dplyr::filter(date_char == rep_date)
  temp_eddy <- df_eddy_sampled %>% dplyr::filter(date_char == rep_date)

  if(dim(temp_eddy)[1] != 0){
    temp_acou$distance_center_m <- NA
    temp_acou$index_unique_eddy <- NA


    lapply(FUN = function_in_out_eddy,  X = c(1:dim(temp_acou)[1]),
           rep_data_acou = 'temp_acou', rep_data_eddy = 'temp_eddy' )

    dede_c <- data.table(temp_acou)

    if(rep_date == liste_date[1]){
      assign('df_end_acou2_databse', dede_c, .GlobalEnv)} else {
        df_end_acou2_databse <- rbind(df_end_acou2_databse, dede_c)}}

}

df_end_acou2_databse$in_out_center <- ifelse(df_end_acou2_databse$distance_center_m <=
                                               df_end_acou2_databse$effective_radius,
                                              'IN', 'OUT')


df_end_acou2_databse <- merge(df_end_acou2_databse,df_for_complete_eddy )

list_eddy_cyclonic <- unique(df_end_acou2_databse[df_end_acou2_databse$cyclonic_type == 'Cyclonic',
                                                   'index_time'])$index_time
length(list_eddy_cyclonic)

list_eddy_anticyclonic <- unique(df_end_acou2_databse[df_end_acou2_databse$cyclonic_type == 'Anticyclonic',
                                                   'index_time'])$index_time
length(list_eddy_anticyclonic)

list_track_num_TRUE <- unique(df_end_acou2_databse$track_num_TRUE)
length(list_track_num_TRUE)
list_unique_eddy <- unique(df_end_acou2_databse$index_unique_eddy)
length(list_unique_eddy)

#######################################################################################
############### 2nd : overlap based on the exact eddies contour #######################
#######################################################################################

df_index_acoustic <- df_index_acoustic %>% arrange(source, date)

liste_date <- unique(df_index_acoustic$date_char)
length(liste_date) 

res <- list()
for(rep_date in liste_date){
    print(which(rep_date == liste_date))
  temp_acou <- df_index_acoustic %>% dplyr::filter(date_char == rep_date)
  temp_eddy <- df_eddy_sampled %>% dplyr::filter(date_char == rep_date)

    temp_acou_tot = lapply(FUN = function_find_eddy, 
                           X = c(1:dim(temp_acou)[1]),
                           df_acou = temp_acou, df_eddy = temp_eddy )

    temp_final <- do.call(rbind, temp_acou_tot)
    res <- c(res, list(temp_final))
}

df_eddy_extracted <- do.call(rbind.data.frame, res)

save(file = here::here('data/intermediate/df_eddy_extracted.Rdata'), df_eddy_extracted)

#######################################################################################
######################### 3rd :classify eddies regions ################################
#######################################################################################

load(file = here::here('data/intermediate/df_eddy_extracted.Rdata'))

df_sunangle <- df_acoustic %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(sunangle = mean(sunangle))
df_eddy_extracted <- merge(df_eddy_extracted, df_sunangle)
df_eddy_extracted <- df_eddy_extracted %>% 
  dplyr::mutate(moment = ifelse(sunangle < 0, 'Night', 'Day'))

df_end3 <- merge(df_eddy_extracted, df_for_complete_eddy)
df_end3$distance_center_m <- df_end3$distance_center_m/1000
df_end3$speed_radius  <- df_end3$speed_radius/1000
df_end3$effective_radius  <- df_end3$effective_radius/1000

df_end3$distance_bord_speed   <- df_end3$distance_bord_speed/1000
df_end3$distance_bord_effective   <- df_end3$distance_bord_effective/1000
df_end3$distance_bord_center_speed   <- df_end3$distance_bord_center_speed/1000
df_end3$distance_bord_center_effective   <- df_end3$distance_bord_center_effective/1000

df_end3$effective_area   <- df_end3$effective_area/1000^2
df_end3$speed_area   <- df_end3$speed_area/1000^2

df_end3 <- df_end3 %>% mutate(distance_min = pmin(distance_bord_speed, distance_bord_effective))

df_end3$which_bord <- ifelse(df_end3$distance_min == df_end3$distance_bord_effective,
                             'effective', 'speed')

df_end3$distance_bord_center_effective_total <- ifelse(df_end3$in_out_bord_eff == 'IN',
                                                       df_end3$distance_center_m + df_end3$distance_bord_effective,
                                                       df_end3$distance_center_m - df_end3$distance_bord_effective)

df_end3$distance_center_perc <- df_end3$distance_center_m* 100/ df_end3$distance_bord_center_effective_total                             
df_end3$distance_bord_sp_perc <- df_end3$distance_bord_speed * 100/ df_end3$distance_bord_center_effective_total
df_end3$distance_bord_eff_perc <- df_end3$distance_bord_effective * 100/ df_end3$distance_bord_center_effective_total

df_end3$distance_center_perc_classe <- cut(df_end3$distance_center_perc, 
                                           c(0, 20, 50,80, 120, 150, 200,
                                             500, 1000), include.lowest = TRUE)
df_end3$distance_bord_sp_perc_classe <- cut(df_end3$distance_bord_sp_perc, 
                                            c(0,20, 50, 100, 200, 1000), include.lowest = TRUE)
df_end3$distance_bord_eff_perc_classe <- cut(df_end3$distance_bord_eff_perc, 
                                             c(0,20, 100, 200, 1000), include.lowest = TRUE)


df_end3$distance_class <- ifelse(df_end3$distance_bord_sp_perc_classe == '[0,20]' & 
                                   df_end3$which_bord == 'speed',
                                 '3.border_speed',
                                 ifelse(df_end3$distance_bord_eff_perc_classe == '[0,20]' & 
                                          df_end3$which_bord == 'effective',
                                        '4.border_effective', 
                                        ifelse(df_end3$distance_center_perc_classe == '[0,20]',
                                               '1.core', 
                                               ifelse(df_end3$in_out_bord_speed == 'IN',
                                                      '2.interne',
                                                      ifelse(df_end3$distance_bord_eff_perc_classe %in% c("(20,100]") &
                                                               df_end3$in_out_bord_eff == 'OUT',
                                                             '5.control', 'reste')))))

table(df_end3$distance_class)
df_end3[is.na(df_end3$distance_class), 'distance_class'] <- 'reste'

df_end3_with_vertical <- merge(df_end3, df_acoustic[,c("index_acous",  "depth", "sv")])

save(file = here::here('data/intermediate/df_end3_with_vertical.Rdata'), df_end3_with_vertical)
save(file = here::here('data/intermediate/df_end3.Rdata'), df_end3)


