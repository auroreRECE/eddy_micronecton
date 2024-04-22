library(dplyr)

########################################################################################
########################################## Figure 3 #################################### 
########################################################################################
load(file = here::here('data/intermediate/df_end3_with_vertical.Rdata'))

df_end3_with_vertical$distance_class2 <- ifelse(df_end3_with_vertical$in_out_bord_eff == 'IN', 
                                                'Inside', 'Outside')
df_end3_with_vertical$Sv <- 10 * log10(df_end3_with_vertical$sv)
df_end3_with_vertical$nasc <- df_end3_with_vertical$sv * 4 * pi * 1852 * 1852 * 20

df_end3_with_vertical$index_unique_eddy_moment <- paste0(df_end3_with_vertical$index_unique_eddy, '_',
                                                         df_end3_with_vertical$moment)

df_plot2 <- df_end3_with_vertical %>%
  group_by(index_unique_eddy , moment, source, 
           cyclonic_type, distance_class2, depth) %>%
  summarize(nasc_m = mean(nasc, na.rm = TRUE),
            nasc_sd = sd(nasc, na.rm = TRUE),
            sv_m = mean(sv, na.rm = TRUE),
            sv_sd = sd(sv, na.rm = TRUE),
            n_acous = length(unique(index_acous)))

df_for_characteristic <- df_end3_with_vertical %>% 
  group_by(index_unique_eddy, moment, track_num_TRUE, cyclonic_type, 
           speed_average, speed_radius, speed_area, effective_radius, 
           effective_area, obs_number,amplitude, lon_eddy, lat_eddy) %>% 
  summarise(n_p_acous = length(index_acous))


df_plot2$distance_class2 <- as.factor(df_plot2$distance_class2)
df_plot2$index_unique_eddy_moment <- paste0(df_plot2$index_unique_eddy, '_', 
                                            df_plot2$moment)

df_for_rep_tot_raw <- df_plot2 %>% 
  filter(distance_class2 != 'Outside') %>% 
  group_by(index_unique_eddy_moment, index_unique_eddy, moment) %>% 
  summarise(n = length(unique(sv_m)))
df_for_rep_tot_raw$effet_eddy <- NA
df_for_rep_tot_raw$effet_eddy_0_200 <- NA
df_for_rep_tot_raw$effet_eddy_200_700 <- NA
df_for_rep_tot_raw$random_control <- NA

df_for_rep_tot_raw <- data.frame(df_for_rep_tot_raw)

## centre et control
length(unique(df_for_rep_tot_raw$index_unique_eddy_moment))

for(rep_eddy in unique(df_for_rep_tot_raw$index_unique_eddy_moment)){
  print(which(rep_eddy == unique(df_for_rep_tot_raw$index_unique_eddy_moment)))
  # rep_eddy = "28873878_Night"
  
  df_temp <- data.frame(df_plot2) %>% 
    filter(index_unique_eddy_moment == rep_eddy)
  
  df_temp_0_700 <- df_temp 
  df_temp_0_200 <- df_temp_0_700 %>%
    dplyr::filter(depth <= 200)
  df_temp_200_700 <- df_temp_0_700 %>%
    dplyr::filter(depth  >= 201)
  
  if(dim(df_temp_0_700)[1]  != 0){
    
    wil_lay1_core_g = wilcox.test(nasc_m ~ distance_class2, paired = FALSE,alternative = 'greater',
                                  data = df_temp_0_700)
    wil_lay1_core_l = wilcox.test(nasc_m ~ distance_class2, paired = FALSE,alternative = 'less',
                                  data = df_temp_0_700)
    
    rep_lay1_core <- ifelse(wil_lay1_core_g$p.value > 0.05 & wil_lay1_core_l$p.value > 0.05, '0', 
                            ifelse(wil_lay1_core_g$p.value < 0.05, '1', '-1'))
    df_for_rep_tot_raw[ df_for_rep_tot_raw$index_unique_eddy_moment == rep_eddy, 'effet_eddy'] <- rep_lay1_core
    
    ######
    wil_lay1_core_g_depth1 = wilcox.test(nasc_m ~ distance_class2, paired = FALSE,alternative = 'greater',
                                         data = df_temp_0_200)
    wil_lay1_core_l_depth1 = wilcox.test(nasc_m ~ distance_class2, paired = FALSE,alternative = 'less',
                                         data = df_temp_0_200)
    
    rep_lay1_core_depth1 <- ifelse(wil_lay1_core_g_depth1$p.value > 0.05 & wil_lay1_core_l_depth1$p.value > 0.05, '0', 
                                   ifelse(wil_lay1_core_g_depth1$p.value < 0.05, '1', '-1'))
    df_for_rep_tot_raw[ df_for_rep_tot_raw$index_unique_eddy_moment == rep_eddy, 'effet_eddy_0_200'] <- rep_lay1_core_depth1 
    
    ######
    wil_lay1_core_g_depth2 = wilcox.test(nasc_m ~ distance_class2, paired = FALSE,alternative = 'greater',
                                         data = df_temp_200_700)
    wil_lay1_core_l_depth2 = wilcox.test(nasc_m ~ distance_class2, paired = FALSE,alternative = 'less',
                                         data = df_temp_200_700)
    
    rep_lay1_core_depth2 <- ifelse(wil_lay1_core_g_depth2$p.value > 0.05 & wil_lay1_core_l_depth2$p.value > 0.05, '0', 
                                   ifelse(wil_lay1_core_g_depth2$p.value < 0.05, '1', '-1'))
    df_for_rep_tot_raw[ df_for_rep_tot_raw$index_unique_eddy_moment == rep_eddy, 'effet_eddy_200_700'] <- rep_lay1_core_depth2 
    
    ###### test aleatoire
    df_temp_random <- data.frame(df_end3_with_vertical) %>% 
      filter(index_unique_eddy_moment == rep_eddy & 
               distance_class2 == 'Outside')
    
    length(unique(df_temp_random$index_acous))
    
    if(length(unique(df_temp_random$index_acous)) >= 2){
      two_acous_profiles <- sample(unique(df_temp_random$index_acous), 2)
      
      df_temp_random <- df_temp_random %>% 
        filter(index_acous %in% two_acous_profiles)
      
      wil_random_test = wilcox.test(nasc ~ index_acous, paired = FALSE,
                                    data = df_temp_random)
      
      rep_random_test <- wil_random_test$p.value
      df_for_rep_tot_raw[ df_for_rep_tot_raw$index_unique_eddy_moment == rep_eddy,
                          'random_control'] <- rep_random_test }
    
    
  }
  
}


df_for_rep_tot <- merge(df_for_rep_tot_raw, df_for_characteristic)

df_for_rep_tot$effet_eddy <- as.numeric(df_for_rep_tot$effet_eddy)
df_for_rep_tot$effet_eddy_0_200 <- as.numeric(df_for_rep_tot$effet_eddy_0_200)
df_for_rep_tot$effet_eddy_200_700 <- as.numeric(df_for_rep_tot$effet_eddy_200_700)


range(df_for_rep_tot$random_control, na.rm = TRUE)
df_for_rep_tot <- df_for_rep_tot %>% 
  dplyr::filter(df_for_rep_tot$random_control >= 0.05)

################# SST et chlor
load(file = here::here('data/intermediate/df_end3.Rdata'))
head(df_end3)

load(file = "data/raw/df_sst_chloro.Rdata")
head(df_sst_chloro)


df_end3 <- merge(df_end3, df_sst_chloro)
df_end3$distance_class2 <- ifelse(df_end3$in_out_bord_eff == 'IN', 
                                                'Inside', 'Outside')

df_plot2_sst <- df_end3 %>%
  group_by(index_unique_eddy ,index_acous, moment, source,
           cyclonic_type, distance_class2) %>%
  summarize(chloro_m = mean(chloro, na.rm = TRUE),
            chloro_sd = sd(chloro, na.rm = TRUE),
            sst_m = mean(sst, na.rm = TRUE),
            sst_sd = sd(sst, na.rm = TRUE),
            n_acous = length(unique(index_acous)))

df_plot2_sst$distance_class2 <- as.factor(df_plot2_sst$distance_class2)

df_for_rep_tot_raw_sst <- df_plot2_sst %>% 
  filter(distance_class2 != 'Outside') %>% 
  group_by(index_unique_eddy) %>% 
  summarise(n = length(unique(chloro_m)))
df_for_rep_tot_raw_sst$effet_eddy_sst <- NA
df_for_rep_tot_raw_sst$effet_eddy_chloro <- NA

df_for_rep_tot_raw_sst <- data.frame(df_for_rep_tot_raw_sst)

## centre et control
length(unique(df_for_rep_tot_raw_sst$index_unique_eddy))

for(rep_eddy in unique(df_for_rep_tot_raw_sst$index_unique_eddy)){
  print(which(rep_eddy == unique(df_for_rep_tot_raw_sst$index_unique_eddy)))
  # rep_eddy = "28873878"
  
  df_temp <- data.frame(df_plot2_sst) %>% filter(index_unique_eddy == rep_eddy)
  df_temp <- df_temp %>% 
    dplyr::filter(!is.na(sst_m))
  if(dim(df_temp)[1]  != 0){
    
    wil_sst_g = wilcox.test(sst_m ~ distance_class2, paired = FALSE,alternative = 'greater',
                            data = df_temp)
    wil_sst_l = wilcox.test(sst_m ~ distance_class2, paired = FALSE,alternative = 'less',
                            data = df_temp)
    
    rep_sst <- ifelse(wil_sst_g$p.value > 0.05 & wil_sst_l$p.value > 0.05, '0', 
                      ifelse(wil_sst_g$p.value < 0.05, '1', '-1'))
    df_for_rep_tot_raw_sst[df_for_rep_tot_raw_sst$index_unique_eddy == rep_eddy, 'effet_eddy_sst'] <- rep_sst
    
    ######
    wil_chloro_g = wilcox.test(chloro_m ~ distance_class2, paired = FALSE,alternative = 'greater',
                               data = df_temp)
    wil_chloro_l = wilcox.test(chloro_m ~ distance_class2, paired = FALSE,alternative = 'less',
                               data = df_temp)
    
    rep_chloro <- ifelse(wil_chloro_g$p.value > 0.05 & wil_chloro_l$p.value > 0.05, '0', 
                         ifelse(wil_chloro_g$p.value < 0.05, '1', '-1'))
    df_for_rep_tot_raw_sst[df_for_rep_tot_raw_sst$index_unique_eddy == rep_eddy, 'effet_eddy_chloro'] <- rep_chloro 
    
  }
}


df_for_rep_tot_sst_chl <- merge(df_for_rep_tot_raw_sst, df_for_characteristic)

df_for_rep_tot_sst_chl$effet_eddy_sst <- as.numeric(df_for_rep_tot_sst_chl$effet_eddy_sst)
df_for_rep_tot_sst_chl$effet_eddy_chloro <- as.numeric(df_for_rep_tot_sst_chl$effet_eddy_chloro)

df_for_rep_tot_sst_chl2 <- df_for_rep_tot_sst_chl %>% 
  dplyr::select(index_unique_eddy,effet_eddy_sst, effet_eddy_chloro, moment)


################# all together 

df_for_rep_tot22 <- merge(df_for_rep_tot, df_for_rep_tot_sst_chl2)
save(file = here::here('data/intermediate/df_for_rep_tot22.Rdata'), df_for_rep_tot22)

