my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
                                 "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))

three_cols = c('#21908C','#5E0079', '#F92867')
two_cols = c( '#F22300', '#3C9AB2')

library(sf)
library(rnaturalearth)
library(ggpubr)


world <- ne_countries(scale = "medium", returnclass = "sf")

mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE))

mp2 <- mp1
mp2$long <- mp2$long + 360
mp2$group <- mp2$group + max(mp2$group) + 1
mp <- rbind(mp1, mp2)

#######################################################################################
######################################## Figure 1 ##################################### 
#######################################################################################

load(file = 'data/raw/df_eddy_sampled.Rdata')
head(df_eddy_sampled)

load(file = 'data/raw/df_contour_sampled.Rdata')
head(df_contour_sampled)

load(file = here::here('data/intermediate/df_end3_with_vertical.Rdata'))
head(df_end3_with_vertical)
df_end3_with_vertical$distance_class2 <- ifelse(df_end3_with_vertical$in_out_bord_eff == 'IN', 
                                  'Inside', 'Outside')
df_end3_with_vertical$nasc <- df_end3_with_vertical$sv * 4 * pi * 1852 * 1852 * 20

############################### eddies sampling

df_end3_sampling <- df_end3_with_vertical

df_end3_sampling$cyclonic_type <- as.factor(df_end3_sampling$cyclonic_type)
levels(df_end3_sampling$cyclonic_type)[levels(df_end3_sampling$cyclonic_type) == "Anticyclonic"] <- "AE"
levels(df_end3_sampling$cyclonic_type)[levels(df_end3_sampling$cyclonic_type) == "Cyclonic"] <- "CE"


df_end3_sampling$dist_A <- distGeo(as.matrix(df_end3_sampling[,c('lon', 'lat_eddy')]),
                                   as.matrix(df_end3_sampling[,c('lon_eddy', 'lat_eddy')])) /1000
df_end3_sampling$dist_B <- distGeo(as.matrix(df_end3_sampling[,c('lon_eddy', 'lat')]),
                                   as.matrix(df_end3_sampling[,c('lon_eddy', 'lat_eddy')])) /1000

df_end3_sampling$lon_norm <- df_end3_sampling$dist_A/df_end3_sampling$effective_radius
df_end3_sampling$lat_norm <- df_end3_sampling$dist_B/df_end3_sampling$effective_radius

df_end3_sampling$lon_norm <- ifelse(df_end3_sampling$lon < df_end3_sampling$lon_eddy, 
                                    -df_end3_sampling$lon_norm, df_end3_sampling$lon_norm)
df_end3_sampling$lat_norm <- ifelse(df_end3_sampling$lat < df_end3_sampling$lat_eddy, 
                                    -df_end3_sampling$lat_norm, df_end3_sampling$lat_norm)

df_end3_sampling$lon_norm_round <- round(df_end3_sampling$lon_norm , 1)
df_end3_sampling$lat_norm_round <- round(df_end3_sampling$lat_norm , 1)


df_end3_sampling2 <- df_end3_sampling %>%
  dplyr::group_by(cyclonic_type, lon_norm_round, lat_norm_round) %>% 
  dplyr::summarise(n_acous = n_distinct(index_acous ))

df_circle <- data.frame(expand.grid(cyclonic_type = unique(df_end3_sampling$cyclonic_type)))
df_circle$x0 = 0
df_circle$y0 = 0
df_circle$r = 1

df_number <- df_end3_sampling %>% 
  group_by(cyclonic_type) %>%
  summarise(n_acous = length(unique(index_acous)),
            n_eddy = length(unique(index_unique_eddy)))

dede_text <- data.frame(x = 2.4, y = 2.4, 
                        cyclonic_type = c("AE", 'CE'))
eddy_sampling <- ggplot() +
  geom_tile(data = df_end3_sampling2,
            aes(x =lon_norm_round, y =  lat_norm_round, fill = n_acous)) +
  xlim(-2.6, 2.6) +
  scale_fill_gradientn(colors = my_spectral(50),
                       # limits = c(0, 300),
                       na.value = "#D81F1E") +
  scale_color_manual(values = c(two_cols), name = '') + 
  guides(color = guide_legend(override.aes = list(size = 1, alpha = 1))) +
  facet_grid(cyclonic_type ~.) +  coord_fixed() +theme_classic() +
  geom_circle(data = df_circle, aes(x0 = x0, y0 = y0, r = r),
              col = "grey10", fill  = NA, linetype = 1, size = 1) + 
  geom_text(data = df_number[df_number$cyclonic_type == "AE", ], 
            aes(x = -2.5, y = 2.45,  label = paste0('Ne = ', n_eddy)),
            hjust = 0, size = 3, color =  "#F22300") + 
  geom_text(data = df_number[df_number$cyclonic_type == "AE", ], 
            aes(x = -2.5, y = -2.4, label = paste0('Na = ', n_acous)),
            hjust = 0, size = 3, color =  "#F22300") +
  geom_text(data = dede_text, aes(x = x, y = y, label = cyclonic_type)) +
  geom_text(data = df_number[df_number$cyclonic_type == "CE", ], 
            aes(x = -2.5, y = 2.45,  label = paste0('Ne = ', n_eddy)),
            hjust = 0, size = 3, color =  "#3C9AB2") + 
  geom_text(data = df_number[df_number$cyclonic_type == "CE", ], 
            aes(x = -2.5, y = -2.4, label = paste0('Na = ', n_acous)),
            hjust = 0, size = 3, color =  "#3C9AB2") +
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = NA, color = 'black')) + 
  theme(legend.position = 'none',
        legend.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12, face = 'plain'),
        strip.background = element_blank(),
        strip.text = element_blank(),
        strip.placement = "outside")

############################### spatial maps

sf_use_s2(FALSE)

df_eddies_sampled <- df_end3_with_vertical %>% 
  dplyr::group_by(index_unique_eddy, cyclonic_type) %>% 
  dplyr::summarise(lon_eddy = mean(lon_eddy), 
                   lat_eddy = mean(lat_eddy))
df_eddies_sampled$cyclonic_type <- as.factor(df_eddies_sampled$cyclonic_type)
levels(df_eddies_sampled$cyclonic_type)[levels(df_eddies_sampled$cyclonic_type) == 'Anticyclonic'] <- 'AE'
levels(df_eddies_sampled$cyclonic_type)[levels(df_eddies_sampled$cyclonic_type) == 'Cyclonic'] <- 'CE'

df_end3_with_vertical$in_eddies <- df_end3_with_vertical$in_out_bord_eff
df_end3_with_vertical$in_eddies <- as.factor(df_end3_with_vertical$in_eddies)
levels(df_end3_with_vertical$in_eddies)
levels(df_end3_with_vertical$in_eddies) <- c("In eddies", "Outside")

dede <- df_end3_with_vertical
dede$cyclonic_type <- as.factor(dede$cyclonic_type)
levels(dede$cyclonic_type)[levels(dede$cyclonic_type) == 'Anticyclonic'] <- 'AE'
levels(dede$cyclonic_type)[levels(dede$cyclonic_type) == 'Cyclonic'] <- 'CE'

dede$lon2 <- ifelse(dede$lon < 30, dede$lon + 360, dede$lon)

map_eddies  <- ggplot(world) +
  geom_point(data = dede, aes(x = lon2, y = lat, color = cyclonic_type),
             size = 0.01, alpha = 0.2) +
  scale_color_manual(values = c("#F22300", "#3C9AB2" )) +
  geom_polygon(data = mp, aes(x = long, y = lat, group = group),
               fill = 'grey60', color = NA ) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  geom_sf(fill = 'grey60', color = NA) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(0, 360, 60),  limits = c(30, 364)) +
  scale_y_continuous(breaks =seq(-60, 60, 30),  limits = c(-70, 50)) +
  theme_bw() + theme(axis.title = element_blank(),
                     legend.direction = 'horizontal',
                     legend.background = element_blank(),
                     legend.position = c(0.13, 0.9),
                     legend.title = element_blank()) 

############################### one eddy example
table(df_end3_with_vertical$index_unique_eddy)
rep_index1 <- 63140025       

test <- df_end3_with_vertical %>% dplyr::filter(index_unique_eddy  == rep_index1  )

test_edd <- df_contour_sampled %>%  
  dplyr::filter(index_unique_eddy  == rep_index1) %>%
  dplyr::arrange(index_time , index_contour)

dede2 = test_edd %>% group_by(index_unique_eddy, index_time) %>%
  summarize(lon = mean(lon_eddy), lat = mean(lat_eddy),
            speed_radius = mean(speed_radius)/1000,
            core_radius = speed_radius*20/100)

map_insert = ggplot(data = world) +
  geom_polygon(data = test_edd, aes(x = effective_contour_longitude,
                                    y = effective_contour_latitude),
               fill = 'grey90', alpha=0.9) +
  geom_point(data = test, aes(x = lon, y = lat, col = distance_class2),
             size = 1) +
  
  theme_classic() +  geom_sf(color = NA, fill = NA) + coord_sf(expand = FALSE) + 
  scale_x_continuous(limits = c(-99, -95.5),
                     breaks = c(-98, -96), 
                     labels = function(x) paste0(substr(x,2,3), "°W")  ) +
  scale_y_continuous(limits = c( 2 , 4),
                     breaks = c(2,4),
                     labels = function(x) paste0(x, "°N")) +
  scale_color_manual(values = c('#9A32CD','#29BF12'),
                     name = '', drop = FALSE) +
  theme(axis.title = element_blank(),legend.position = 'none',
        axis.text = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA, color = 'black'),
        strip.background = element_rect(fill = 'grey90'))  

#### echogramme

test2 <- df_end3_with_vertical %>% dplyr::filter(index_unique_eddy  == rep_index1)

echogram <- ggplot(test2,  aes(x = index_acous , y = -depth)) +
  geom_tile(aes(fill = nasc, color = nasc)) +
  geom_vline(xintercept = 247483 , color = 'white' ) +
  geom_text(aes(x = 247400, y = 15, label = 'Inside'), col = '#9A32CD') + 
  geom_text(aes(x = 247495, y = 15, label = 'Outside'), col = '#29BF12') + 
  scale_y_continuous(limits = c(-800, 18),
                     breaks = seq(-800, 0, 200),
                     labels = seq(800, 0, -200)) +
  geom_point(data = test[test$in_out_bord_eff == 'IN', ],
             aes(x = index_acous, y = -1), col = '#9A32CD', size = 0.5) +
  geom_point(data = test[test$in_out_bord_eff == 'OUT', ],
             aes(x = index_acous, y = -1), col = '#29BF12', size = 0.5) +
  guides(fill = guide_colorbar(title.position = 'left', barheight  = 0.2, 
                               barwidth  = 6)) +
  scale_fill_viridis_c(name = 'NASC') +
  scale_color_viridis_c(name = 'NASC') +
  
  xlab('') + ylab('Depth (m)')  + theme_classic() +
  theme(legend.position = c(0.5, 0.039),
        # legend.background = element_rect(fill = NA), 
        legend.direction = 'horizontal',
        # legend.title = element_blank(),
        panel.grid.major.y = element_line(color = 'grey60',
                                          linetype = 2))


dede_p <- test2 %>% 
  dplyr::group_by(in_out_bord_eff, depth) %>% 
  dplyr::summarise(nasc_m = mean(nasc),
                   nasc_sd = sd(nasc),
                   n = n_distinct(index_acous))
dede_p$in_out_bord_eff <- as.factor(dede_p$in_out_bord_eff)
levels(dede_p$in_out_bord_eff)
levels(dede_p$in_out_bord_eff) <- c('Inside', 'Outside')

pl_profil <- ggplot(data = dede_p,
                    aes(x = -depth, y = nasc_m, color = in_out_bord_eff)) +
  geom_ribbon(aes(x = -depth, ymin = nasc_m - 1.96 * nasc_sd / sqrt(n),
                  ymax = nasc_m + 1.96 * nasc_sd / sqrt(n), 
                  group = in_out_bord_eff),
              fill = "grey80", color = NA) + 
  geom_line(size = 1) + 
  scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = seq(-800, 0, 200),
                     labels = seq(800, 0, -200),
                     limits = c(-800, 18)) +
  scale_color_manual(values = c('#9A32CD','#29BF12'),
                     name = '', drop = FALSE) +
  ylab('NASC') + xlab('Depth (m)') + 
  coord_flip() +  theme_classic() + 
  theme(axis.title.y = element_blank(), 
        legend.position = 'none',
        panel.grid.major.y = element_line(color = 'grey60',
                                          linetype = 2)) + 
  annotation_custom( ggplotGrob(map_insert), 
                     xmin = -800, xmax = -300, ymin = 300, ymax = 600)

fig4_1 <- ggarrange(map_eddies, eddy_sampling, ncol = 2, widths = c(2, 0.6),
                    labels = c('(A)', '(B)'),
                    font.label = list(size = 12, face = "plain"),
                    vjust = c(2, 2), hjust = c(-0.5, 0.1))

fig4_2 <- ggarrange( echogram,pl_profil, ncol = 2,
                     labels = c('(C)', '(D)'),
                     widths   = c(2,0.9), align='h',
                     font.label = list(size = 12, face = "plain"),
                     vjust = c(1.7), hjust = c(-1, -0.5))

fig4_all <- ggarrange( fig4_1,fig4_2, nrow = 2,
                       heights = c(1,1))

ggsave(file = 'figures/Figure1.jpg',
       plot = fig4_all, width = 3.5, height = 2.7,  scale = 2.5)
