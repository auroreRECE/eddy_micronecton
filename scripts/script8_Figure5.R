library(dplyr)
library(ggplot2)
library(ggpubr)
library(sf)
library(rnaturalearth)
library(ggpubr)

world <- ne_countries(scale = "medium", returnclass = "sf")

mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE))

mp2 <- mp1
mp2$long <- mp2$long + 360
mp2$group <- mp2$group + max(mp2$group) + 1
mp <- rbind(mp1, mp2)



change.res <- function(x, new.res) {
  new.res * round(x/new.res)
}


########################################################################################
######################################## Figure 5 ###################################### 
########################################################################################

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

df_for_rep_tot_melt$value2 <- as.factor(df_for_rep_tot_melt$value)
levels(df_for_rep_tot_melt$value2) 
levels(df_for_rep_tot_melt$value2) <- c( "Effect",  "No effect",   "Effect" )

df_for_rep_tot_melt$lon2 <- change.res(df_for_rep_tot_melt$lon_eddy, 3)
df_for_rep_tot_melt$lat2 <- change.res(df_for_rep_tot_melt$lat_eddy, 3)

dede <- df_for_rep_tot_melt %>% 
  dplyr::group_by(lon2, lat2) %>% 
  dplyr::summarise(n_tot_eddies = n_distinct(index_unique_eddy) )
dede_effect <- df_for_rep_tot_melt %>% 
  dplyr::filter(value2 == "Effect") %>% 
  dplyr::group_by(lon2, lat2) %>% 
  dplyr::summarise(n_tot_effect = n_distinct(index_unique_eddy) )

dede_all <- merge(dede, dede_effect , all = T)
dede_all[is.na(dede_all$n_tot_effect), "n_tot_effect"] <- 0
dede_all$perc <- dede_all$n_tot_effect*100/dede_all$n_tot_eddies
dede_all$lon3 <- ifelse(dede_all$lon2 < 30, dede_all$lon2 + 360, dede_all$lon2)


sf_use_s2(FALSE)


plot_map = ggplot(data = world) +  
  geom_tile(data = dede_all, aes(x = lon3, y = lat2, fill = perc)) +
  scale_fill_viridis_b(n.breaks = 8, name = 'Percentage of\ninfluencing eddies') +
  geom_polygon(data = mp, aes(x = long, y = lat, group = group),
               fill = 'grey60', color = NA ) +
  geom_sf(fill = 'grey60', color = NA) +
  coord_sf(expand = FALSE) +
  geom_rect(aes(xmin = 235, xmax = 285, ymin = 41, ymax = 59.9), fill = 'grey60', color = NA) +
  geom_rect(aes(xmin = 242, xmax = 295, ymin = 45, ymax = 59.9), fill = 'grey60', color = NA) +
  geom_rect(aes(xmin = 228, xmax = 250, ymin = 53, ymax = 59.9), fill = 'grey60', color = NA) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) +
  scale_x_continuous(breaks = seq(0, 380, 60),  limits = c(30, 387)) +
  scale_y_continuous(breaks = seq(-60, 80, 20),  limits = c(-70, 60)) +
  theme_minimal() + theme(axis.title = element_blank(),
                          # axis.text = element_blank(),
                          legend.position = c(0.13, 0.89),
                          legend.background = element_rect(fill = 'white', color = NA),
                          legend.title = element_blank(),
                          legend.direction = 'horizontal')

dede_lat <- df_for_rep_tot_melt %>% 
  dplyr::group_by(lat2) %>% 
  dplyr::summarise(n_tot_eddies = n_distinct(index_unique_eddy) )
dede_lat2 <- df_for_rep_tot_melt %>% 
  dplyr::filter(value2 == "Effect") %>% 
  dplyr::group_by(lat2) %>% 
  dplyr::summarise(n_tot_effect = n_distinct(index_unique_eddy) )

dede_lat3 <- merge(dede_lat, dede_lat2 , all = T)
dede_lat3[is.na(dede_lat3$n_tot_effect), "n_tot_effect"] <- 0
dede_lat3$perc <- dede_lat3$n_tot_effect*100/dede_lat3$n_tot_eddies
dede_lat3[dede_lat3$perc == 0, "perc"] <- 0.1



plot_hist_lat = ggplot(dede_lat3, aes(x = lat2, y = perc))  +
  geom_histogram(stat = 'identity', fill = "#21908C", color = "#21908C") + 
  coord_flip() +
  scale_x_continuous(limits = c(-70, 60),
                     breaks = seq(-60, 60, 20), 
                     labels = c("60°S","40°S", "20°S","0°",
                                "20°N","40°N","60°N"),
                     position = "top" ) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 60)) +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        panel.grid.minor.y = element_blank(),
        # axis.text.y = element_blank(),
        legend.position = 'top')

row1 <- ggarrange(plot_map, plot_hist_lat, ncol = 2, widths = c(3,0.7),
                  labels = c('(A)', '(B)'),
                  font.label = list(size = 12, face = "plain"))
ggsave(file = 'figures/Figure5.jpg',
       plot = row1, width = 3, height = 1.5,  scale = 3)




