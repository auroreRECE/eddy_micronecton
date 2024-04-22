
function_in_out_eddy <- function(rep_data_acou, rep_data_eddy, k){
  
  dede_a <- data.frame(get(rep_data_acou))
  dede_e <- data.frame(get(rep_data_eddy))
  
  list_dist <- distGeo(as.matrix(dede_a[k,c('lon', 'lat')]),
                       as.matrix(dede_e[,c('lon', 'lat')]))
  mimi <- min(list_dist)
  
  temp_acou$distance_center_m[k] <- mimi
  temp_acou$index_unique_eddy[k] <- dede_e$index_unique_eddy[list_dist == mimi]
  temp_acou$effective_radius[k] <- dede_e$effective_radius[list_dist == mimi]
  
  assign('temp_acou', temp_acou, .GlobalEnv)
  
}


function_find_eddy <- function(k, df_acou, df_eddy){
  # k = 1
  df_eddy = temp_eddy 
  df_acou = temp_acou
  df_acou <- data.frame(df_acou[k,])
  df_eddy <- data.frame(df_eddy)
  
  list_dist <- distGeo(as.matrix(df_acou[,c('lon', 'lat')]),
                       as.matrix(df_eddy[,c('lon', 'lat')]))
  mimi <- min(list_dist)
  
  df_acou$distance_center_m <- mimi
  df_acou$index_unique_eddy <- df_eddy$index_unique_eddy[list_dist == mimi]
  
  df_contour_temp <- df_contour_sampled[df_contour_sampled$index_time == df_eddy$index_time[list_dist == mimi] & 
                                          df_contour_sampled$cyclonic_type == df_eddy$cyclonic_type[list_dist == mimi], ]
  df_contour_temp <- df_contour_temp %>% arrange(index_contour)
  
  list_bord_sp <- distGeo(as.matrix(df_acou[,c('lon', 'lat')]),
                          as.matrix(df_contour_temp[,c('speed_contour_longitude',
                                                       'speed_contour_latitude')]))
  
  mimi_bord_sp <- min(list_bord_sp)
  df_acou$distance_bord_speed <- mimi_bord_sp
  
  df_acou$distance_bord_center_speed <- distGeo(as.matrix(df_contour_temp[list_bord_sp == mimi_bord_sp,c('lon_eddy', 'lat_eddy')]),
                                                as.matrix(df_contour_temp[list_bord_sp == mimi_bord_sp,c('speed_contour_longitude',
                                                                                                         'speed_contour_latitude')]))[1]
  
  list_bord_eff <- distGeo(as.matrix(df_acou[,c('lon', 'lat')]),
                           as.matrix(df_contour_temp[,c('effective_contour_longitude',
                                                        'effective_contour_latitude')]))
  mimi_bord_eff <- min(list_bord_eff)
  df_acou$distance_bord_effective <- mimi_bord_eff
  
  df_acou$distance_bord_center_effective <- distGeo(as.matrix(df_contour_temp[list_bord_eff == mimi_bord_eff,c('lon_eddy', 'lat_eddy')]),
                                                    as.matrix(df_contour_temp[list_bord_eff == mimi_bord_eff,c('effective_contour_longitude',
                                                                                                               'effective_contour_latitude')]))[1]
  
  sp_point_eff <-   SpatialPoints(df_contour_temp[list_bord_eff == mimi_bord_eff,c('effective_contour_longitude',
                                                                                   'effective_contour_latitude')], 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 
                                    +towgs84=0,0,0 "))  
  
  sp_center <-   SpatialPoints(df_contour_temp[list_bord_sp == mimi_bord_sp,c('lon_eddy', 'lat_eddy')], 
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 
                                    +towgs84=0,0,0 "))  
  
  sp_point <-   SpatialPoints(df_acou[,c('lon', 'lat')], 
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 
                                    +towgs84=0,0,0 "))  
  
  sp_contour2 <- Polygons(list(Polygon(df_contour_temp[,c('effective_contour_longitude',
                                                          'effective_contour_latitude')])),1) 
  sp_contour_speed <- Polygons(list(Polygon(df_contour_temp[,c('speed_contour_longitude',
                                                               'speed_contour_latitude')])),1)
  
  sps = SpatialPolygons(list(sp_contour2))
  sps_speed = SpatialPolygons(list(sp_contour_speed))
  
  proj4string(sps) = CRS("+proj=longlat +datum=WGS84 
                                   +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  
  df_acou$in_out_bord_eff <- ifelse(is.na(raster::extract(sps, sp_point)[2]), 'OUT', 'IN')
  df_acou$in_out_bord_speed <- ifelse(is.na(raster::extract(sps_speed, sp_point)[2]), 'OUT', 'IN')
  
  
  # assign('df_acou', df_acou, .GlobalEnv)
  df_acou
  
  # 
  # plot(sps)
  # plot(sps_speed, add = T, col = 'green')
  # plot(sp_point, add = T)
  # plot(sp_center, add = T)
  # plot(sp_point_eff, add = T, col = 'red')
  
}

