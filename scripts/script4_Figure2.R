library(dplyr)
library(ggplot2)
library(ggpubr)

two_cols = c( '#F22300', '#3C9AB2')

########################################################################################
######################################## Figure 2 ###################################### 
########################################################################################

########################################## SST and Chlorophyll anomalies

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
levels(df_for_anom_chloro2$cyclonic_type)[levels(df_for_anom_chloro2$cyclonic_type) == "Anticyclonic"] <- "AE"
levels(df_for_anom_chloro2$cyclonic_type)[levels(df_for_anom_chloro2$cyclonic_type) == "Cyclonic"] <- "CE"


df_for_anom_chloro2_small <- df_for_anom_chloro2 %>% 
  dplyr::group_by( cyclonic_type) %>%
  dplyr::summarize(anom_chloro_m = mean(anom_chloro   , na.rm = TRUE),
            anom_chloro_sd = sd(anom_chloro   , na.rm = TRUE),
            anom_sst_m = mean(anom_sst2, na.rm = TRUE),
            anom_sst_sd = sd(anom_sst2, na.rm = TRUE),
            n_eddies = n_distinct(index_unique_eddy))

df_for_anom_chloro2_small$sst_max <- df_for_anom_chloro2_small$anom_sst_m + 
  2 * df_for_anom_chloro2_small$anom_sst_sd/sqrt(df_for_anom_chloro2_small$n_eddies)
df_for_anom_chloro2_small$sst_min <- df_for_anom_chloro2_small$anom_sst_m -
  2 * df_for_anom_chloro2_small$anom_sst_sd/sqrt(df_for_anom_chloro2_small$n_eddies)

plot_sst = ggplot(df_for_anom_chloro2_small, 
                  aes(y = anom_sst_m, x = cyclonic_type,
                      color = cyclonic_type)) +
  geom_hline(yintercept = 0, color = 'grey60') +
  geom_point() +
  geom_pointrange(aes(ymin = sst_min, ymax = sst_max, 
                      x = cyclonic_type)) +
  scale_color_manual(values = c((two_cols)), name = '') +
  theme_classic() +
  xlab('') + ylab('SST anomaly (°)') +
  theme(panel.grid.major = element_line(color = 'grey90'),
        panel.background = element_rect(fill = NA, color = 'black'), 
        legend.position = 'none',
        axis.text = element_text(size = 10, face = 'plain'))


df_for_anom_chloro2_small$chloro_max <- df_for_anom_chloro2_small$anom_chloro_m + 
  2 * df_for_anom_chloro2_small$anom_chloro_sd/sqrt(df_for_anom_chloro2_small$n_eddies)
df_for_anom_chloro2_small$chloro_min <- df_for_anom_chloro2_small$anom_chloro_m -
  2 * df_for_anom_chloro2_small$anom_chloro_sd/sqrt(df_for_anom_chloro2_small$n_eddies)

plot_chloro = ggplot(df_for_anom_chloro2_small, 
                     aes(y = anom_chloro_m, x = cyclonic_type,
                         color = cyclonic_type)) +
  geom_hline(yintercept = 0, color = 'grey60') +
  geom_point() +
  geom_pointrange(aes(ymin = chloro_min, ymax = chloro_max, 
                      x = cyclonic_type)) +
  scale_color_manual(values = c((two_cols)), name = '') +
  theme_classic() +
  xlab('') + ylab('Chlorophyll anomaly (%)') +
  # guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = NA, color = 'black'),
        panel.grid.major = element_line(color = 'grey90')) + 
  theme(legend.position = 'none',
        legend.title = element_blank(),
        # legend.background = element_blank(),
        axis.text = element_text(size = 10, face = 'plain'),
        legend.text = element_text(size = 12, face = 'plain'),
        strip.background = element_blank(),
        strip.text = element_text(face = 'plain', size = 12),
        strip.placement = "outside")


########################
load(file = here::here('data/intermediate/df_end3_with_vertical.Rdata'))
head(df_end3_with_vertical)
df_end3_with_vertical$vertical_layer <- cut(df_end3_with_vertical$depth, c(0, 200, 800))
df_end3_with_vertical$distance_class2 <- ifelse(df_end3_with_vertical$in_out_bord_eff == 'IN', 
                                  'Inside', 'Outside')
df_end3_with_vertical$Sv <- 10 * log10(df_end3_with_vertical$sv)
df_end3_with_vertical$nasc <- df_end3_with_vertical$sv * 4 * pi * 1852 * 1852 * 20

df_plot1_bis <- df_end3_with_vertical %>%
  group_by(index_unique_eddy , source, moment,
           cyclonic_type, distance_class2, depth, vertical_layer) %>%
  summarize(Sv_m = mean(Sv, na.rm = TRUE),
            Sv_sd = sd(Sv, na.rm = TRUE),
            nasc_m = mean(nasc, na.rm = TRUE),
            nasc_sd = sd(nasc, na.rm = TRUE),
            sv_m = mean(sv, na.rm = TRUE),
            sv_sd = sd(sv, na.rm = TRUE),
            n_acous = length(unique(index_acous)))

df_plot1_others_bis <- df_plot1_bis %>% 
  dplyr::filter(distance_class2 == 'Inside')

df_plot1_control_bis <- data.frame(df_plot1_bis) %>% 
  dplyr::filter(distance_class2 == 'Outside') %>% 
  dplyr::select(index_unique_eddy, depth,  nasc_m, sv_m, n_acous)
names(df_plot1_control_bis) <- c("index_unique_eddy", "depth",
                                 "control_nasc","control_sv_m", "control_n_acous")

df_plot1_control_total_bis <- df_plot1_control_bis %>% 
  dplyr::group_by(index_unique_eddy) %>% 
  dplyr::summarise(control_nasc_moyen = mean(control_nasc))
df_plot1_control_bis <- merge(df_plot1_control_bis,df_plot1_control_total_bis )

df_plot1_anomalies_bis <- merge(df_plot1_others_bis, df_plot1_control_bis)

df_plot1_anomalies_bis$delta2 <- (df_plot1_anomalies_bis$nasc_m - 
                                    df_plot1_anomalies_bis$control_nasc)*100/df_plot1_anomalies_bis$control_nasc_moyen

df_plot2_bis <- df_plot1_anomalies_bis %>%
  group_by( cyclonic_type, distance_class2, depth) %>%
  summarize(delta_mean = mean(delta2, na.rm = TRUE),
            delta_sd = sd(delta2, na.rm = TRUE),
            n_acous =  sum(n_acous),
            n_eddies = n_distinct(index_unique_eddy))

df_plot2_bis$inte_max <- df_plot2_bis$delta_mean + 2 * df_plot2_bis$delta_sd/sqrt(df_plot2_bis$n_eddies)
df_plot2_bis$inte_min <- df_plot2_bis$delta_mean - 2 * df_plot2_bis$delta_sd/sqrt(df_plot2_bis$n_eddies)


plot_nasc1 = ggplot(df_plot2_bis) +
  geom_hline(yintercept = 0, color = 'grey60') +
  geom_ribbon(aes(x = -depth, ymax = inte_max, ymin = inte_min,
                  group = interaction(distance_class2, cyclonic_type),
                  fill = cyclonic_type), col = NA, alpha = 0.2) +
  geom_line(aes(x = -depth, y = delta_mean, color = cyclonic_type), size = 1) +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(breaks = seq(-800, 0, 200),
                     labels = seq(800, 0, -200)) +
  scale_color_manual(values = c((two_cols)), name = '') + 
  scale_fill_manual(values = c((two_cols)), name = '') + 
  # facet_nested(~ cluster) +
  coord_flip(ylim = c(-50, 50)) +
  theme_classic() +
  ylab('NASC anomalies (%)') + xlab('Depth (m)') +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(panel.background = element_rect(fill = NA, color = 'black'),
        panel.grid.major = element_line(color = 'grey90')) + 
  theme(legend.position = 'none',
        legend.title = element_blank(),
        # legend.background = element_blank(),
        axis.text = element_text(size = 10, face = 'plain'),
        legend.text = element_text(size = 12, face = 'plain'),
        strip.background = element_blank(),
        strip.text = element_text(face = 'plain', size = 12),
        strip.placement = "outside")


df_plot2_ter <- df_plot1_anomalies_bis %>%
  group_by( cyclonic_type, distance_class2, vertical_layer) %>%
  summarize(delta_mean = mean(delta2, na.rm = TRUE),
            delta_sd = sd(delta2, na.rm = TRUE),
            n_acous =  sum(n_acous),
            n_eddies = n_distinct(index_unique_eddy))

df_plot2_ter$inte_max <- df_plot2_ter$delta_mean + 2 * df_plot2_ter$delta_sd/sqrt(df_plot2_ter$n_eddies)
df_plot2_ter$inte_min <- df_plot2_ter$delta_mean - 2 * df_plot2_ter$delta_sd/sqrt(df_plot2_ter$n_eddies)

df_plot2_ter$vertical_layer <- as.factor(df_plot2_ter$vertical_layer)
levels(df_plot2_ter$vertical_layer)[levels(df_plot2_ter$vertical_layer) == "(0,200]"] <- "0-200m"
levels(df_plot2_ter$vertical_layer)[levels(df_plot2_ter$vertical_layer) == "(200,800]"] <- "200-750m"

plot_nasc_mean = ggplot(df_plot2_ter, aes(y = delta_mean, x = cyclonic_type,
                            color = cyclonic_type)) +
  geom_hline(yintercept = 0, color = 'grey60') +
  geom_point() + facet_grid(~ vertical_layer) +
  geom_pointrange(aes(ymin = inte_max, ymax = inte_min, 
                      x = cyclonic_type)) +
  scale_color_manual(values = c((two_cols)), name = '') +
  theme_classic() +
  xlab('') + ylab('NASC anomaly (%)') +
  theme(panel.background = element_rect(fill = NA, color = 'black'),
        panel.grid.major = element_line(color = 'grey90')) + 
  theme(legend.position = 'none',
        legend.title = element_blank(),
        # legend.background = element_blank(),
        axis.text = element_text(size = 10, face = 'plain'),
        legend.text = element_text(size = 12, face = 'plain'),
        strip.background = element_blank(),
        strip.text = element_text(face = 'plain', size = 12),
        strip.placement = "outside")



plot_gauche1 <- ggarrange(plot_sst, plot_chloro, ncol = 2,
                          common.legend = TRUE, legend = 'bottom',
                          # widths = c(2, 1.9),
                          labels = c('(A)', '(B)'),
                          font.label = list(size = 12, face = "plain"))

plot_gauche <- ggarrange(plot_gauche1, plot_nasc_mean, nrow = 2,
                         labels = c('', '(C)'),
                         align = 'v',
                         font.label = list(size = 12, face = "plain"))

plot_figure2 = ggarrange(plot_gauche, plot_nasc1, 
                labels = c('', '(D)'),
                widths = c(2, 1.8),
                font.label = list(size = 12, face = "plain"))

ggsave(file = 'figures/Figure2.jpg',
       plot = plot_figure2, width = 3, height = 2.2,  scale = 3)

