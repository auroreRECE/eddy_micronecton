library(dplyr)
library(ggplot2)
library(ggpubr)

########################################################################################
######################################## Figure 4 ###################################### 
########################################################################################

########## SST and chlorophyll anomalies  

load(file = "data/raw/df_sst_chloro.Rdata")
head(df_sst_chloro)

load(file = here::here('data/intermediate/df_end3.Rdata'))
head(df_end3)
df_end3 <- merge(df_end3, df_sst_chloro) 
df_end3$distance_class2 <- ifelse(df_end3$in_out_bord_eff == 'IN', 
                                  'Inside', 'Outside')

df_for_anom_chloro <- df_end3 %>%
  dplyr::group_by(index_unique_eddy, cyclonic_type , distance_class2 ) %>%
  dplyr::summarise(chloro_m = mean(chloro, na.rm = TRUE),
                   sst_m = mean(sst, na.rm = TRUE))

df_for_anom_chloro_control <- data.frame(df_for_anom_chloro) %>% 
  dplyr::filter(distance_class2 == 'Outside') %>% 
  dplyr::mutate(chloro_control = chloro_m,
                sst_control = sst_m) %>% 
  dplyr::select(index_unique_eddy, chloro_control, sst_control)

df_for_anom_chloro_others <- data.frame(df_for_anom_chloro) %>% 
  dplyr::filter(distance_class2 != 'Outside') 


df_for_anom_chloro2 <- merge(df_for_anom_chloro_others, df_for_anom_chloro_control)

df_for_anom_chloro2$anom_chloro  <- (df_for_anom_chloro2$chloro_m - df_for_anom_chloro2$chloro_control)*100/ df_for_anom_chloro2$chloro_control
df_for_anom_chloro2$anom_sst2  <- (df_for_anom_chloro2$sst_m - df_for_anom_chloro2$sst_control)


df_for_anom_chloro2$cyclonic_type <- as.factor(df_for_anom_chloro2$cyclonic_type)
levels(df_for_anom_chloro2$cyclonic_type)
levels(df_for_anom_chloro2$cyclonic_type)[levels(df_for_anom_chloro2$cyclonic_type) == "Anticyclonic"] <- "AE"
levels(df_for_anom_chloro2$cyclonic_type)[levels(df_for_anom_chloro2$cyclonic_type) == "Cyclonic"] <- "CE"


########## Figures 

load(file = here::here('data/intermediate/df_for_rep_tot22.Rdata'))
head(df_for_rep_tot22)


df_for_rep_tot_melt <- reshape2::melt(df_for_rep_tot22,
                                      id.vars = c("index_unique_eddy"  ,  "moment" ,
                                                  "index_unique_eddy_moment", 
                                                  "cyclonic_type" ,"speed_average" ,
                                                  "speed_radius" ,"speed_area" ,
                                                  "effective_radius", "effective_area" ,
                                                  "obs_number" , "amplitude" ,
                                                  "lon_eddy","lat_eddy" ),
                                      measure.vars = c('effet_eddy_0_200', 'effet_eddy_200_700')  ,
                                      variable.name = 'vertical_layer')
table(df_for_rep_tot_melt$vertical_layer)
df_for_rep_tot_melt$value <- as.factor(df_for_rep_tot_melt$value)
levels(df_for_rep_tot_melt$vertical_layer) 
levels(df_for_rep_tot_melt$vertical_layer) <- c("0-200m","200-750m")


head(df_for_rep_tot_melt)
df_for_rep_tot_melt$value2 <- df_for_rep_tot_melt$value
levels(df_for_rep_tot_melt$value2) 
levels(df_for_rep_tot_melt$value2) <- c( "Effect",  "No effect",   "Effect" )

unique(df_for_rep_tot_melt$vertical_layer)
unique(df_for_rep_tot_melt$value)
df_for_rep_tot_melt <- df_for_rep_tot_melt %>% 
  dplyr::filter(vertical_layer == "0-200m")
df_for_rep_tot_melt$value <- factor(df_for_rep_tot_melt$value, exclude = T)
levels(df_for_rep_tot_melt$value)
levels(df_for_rep_tot_melt$value) <- c('Decrease', 'No effect', 'Increase')

df_for_rep_tot_melt <- merge(df_for_rep_tot_melt, 
                                 df_for_anom_chloro2[,c( "index_unique_eddy" ,
                                                         "anom_chloro",
                                                         "anom_sst2", 
                                                         "chloro_m", 
                                                         "chloro_control", 
                                                         "sst_m", 
                                                         "sst_control")])

###################################### 
wil_1 <- wilcox.test( paired = FALSE, data = df_for_rep_tot_melt,
                      effective_area ~ value2)
wil_1


###################################### 

plt_amplitude = ggplot(df_for_rep_tot_melt, aes(x = amplitude,y = value,  fill = value) ) + 
  geom_boxplot(outlier.colour = 'grey80', color = 'grey20', width = 0.9) + 
  theme_bw() + xlab("Amplitude (m)") +
  scale_fill_manual(values = c("#D75568",'grey70', "#8DAD37" )) +
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        legend.title =  element_blank(),
        legend.background =  element_blank(),
        legend.position = 'none')

plt_sst = ggplot(df_for_rep_tot_melt, aes(x = anom_sst2,y = value, fill = value) ) + 
  geom_boxplot(outlier.colour = 'grey80', color = 'grey20', width = 0.9) + 
  theme_bw()+ xlab("SST anomaly (°)")+
  scale_fill_manual(values = c("#D75568",'grey70', "#8DAD37" )) +
  theme(axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        legend.title =  element_blank(),
        legend.background =  element_blank(),
        legend.position = 'none')

plt_chloro = ggplot(df_for_rep_tot_melt, aes(x = anom_chloro,y = value, fill = value) ) + 
  geom_boxplot(outlier.colour = 'grey80', color = 'grey20', width = 0.9) + 
  theme_bw() + xlab("Chlorophyll anomaly (%)")+
  scale_fill_manual(values = c("#D75568",'grey70', "#8DAD37" )) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title =  element_blank(),
        legend.background =  element_blank(),
        legend.position = 'none')

plt_surface = ggplot(df_for_rep_tot_melt, aes(x = effective_area,y = value, fill = value) ) + 
  geom_boxplot(outlier.colour = 'grey80', color = 'grey20', width = 0.9) + 
  theme_bw()+ xlab("Surface (km2)")+ 
  scale_fill_manual(values = c("#D75568",'grey70', "#8DAD37" )) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title =  element_blank(),
        legend.background =  element_blank(),
        legend.position = 'none')

plt_age = ggplot(df_for_rep_tot_melt, aes(x = obs_number,y = value, fill = value) ) + 
  geom_boxplot(outlier.colour = 'grey80', color = 'grey20', width = 0.9) + 
  theme_bw()+   xlab("Age (days)")+
  coord_cartesian(xlim = c(0, 250)) +
  scale_fill_manual(values = c("#D75568",'grey70', "#8DAD37" )) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title =  element_blank(),
        legend.background =  element_blank(),
        legend.position = 'none')



plt_all1 <-ggarrange(plt_amplitude, plt_surface,plt_age,
                     widths = c(1.75,1.5,1.5), ncol = 3, nrow = 1)

plt_all2 <- ggarrange(plt_sst, plt_chloro, NULL,
                      widths = c(1.75,1.5,1.5), ncol = 3, nrow = 1)

plt_all <- ggarrange(plt_all1, plt_all2, nrow = 2)

ggsave(file = 'figures/Figure4.jpg',
       plot = plt_all, width = 3.5, height = 1.5,  scale = 3)

