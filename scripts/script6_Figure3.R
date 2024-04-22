library(dplyr)
library(ggplot2)
library(ggpubr)

two_cols = c( '#F22300', '#3C9AB2')

########################################################################################
######################################## Figure 2 ###################################### 
########################################################################################
load(file = here::here('data/intermediate/df_for_rep_tot22.Rdata'))
head(df_for_rep_tot22)


################# plots 

df_for_rep2_0_200 <- df_for_rep_tot22 %>% 
  dplyr::group_by(cyclonic_type, effet_eddy_0_200) %>% 
  dplyr::summarise(n = length(unique(index_unique_eddy)))
df_for_rep2_tot_0_200 <-  df_for_rep2_0_200 %>% 
  dplyr::group_by(cyclonic_type) %>% 
  dplyr::summarise(n_tot = sum(n))
df_for_rep2_0_200 <- merge(df_for_rep2_0_200, df_for_rep2_tot_0_200)
df_for_rep2_0_200$vertical_layer <- 'NASC,\n0-200m'
names(df_for_rep2_0_200)[2] <- 'effet_eddy'


df_for_rep2_200_700 <- df_for_rep_tot22 %>% 
  dplyr::group_by(cyclonic_type, effet_eddy_200_700) %>% 
  dplyr::summarise(n = length(unique(index_unique_eddy)))
df_for_rep2_tot_200_700 <-  df_for_rep2_200_700 %>% 
  dplyr::group_by(cyclonic_type) %>% 
  dplyr::summarise(n_tot = sum(n))
df_for_rep2_200_700 <- merge(df_for_rep2_200_700, df_for_rep2_tot_200_700)
df_for_rep2_200_700$vertical_layer <- 'NASC,\n200-750m'
names(df_for_rep2_200_700)[2] <- 'effet_eddy'


df_for_rep2_sst <- df_for_rep_tot22 %>% 
  dplyr::filter(!is.na(effet_eddy_sst)) %>% 
  dplyr::group_by(cyclonic_type, effet_eddy_sst) %>% 
  dplyr::summarise(n = length(unique(index_unique_eddy)))
df_for_rep2_sst_tot <-  df_for_rep2_sst %>% 
  dplyr::group_by(cyclonic_type) %>% 
  dplyr::summarise(n_tot = sum(n))
df_for_rep2_sst <- merge(df_for_rep2_sst, df_for_rep2_sst_tot)
df_for_rep2_sst$vertical_layer <- 'SST'
names(df_for_rep2_sst)[2] <- 'effet_eddy'


df_for_rep2_chloro <- df_for_rep_tot22 %>%
  dplyr::filter(!is.na(effet_eddy_chloro)) %>% 
  dplyr::group_by(cyclonic_type, effet_eddy_chloro) %>%
  dplyr::summarise(n = length(unique(index_unique_eddy)))
df_for_rep2_chloro_tot <-  df_for_rep2_chloro %>%
  dplyr::group_by(cyclonic_type) %>%
  dplyr::summarise(n_tot = sum(n))
df_for_rep2_chloro <- merge(df_for_rep2_chloro, df_for_rep2_chloro_tot)
df_for_rep2_chloro$vertical_layer <- 'Chlorophyll'
names(df_for_rep2_chloro)[2] <- 'effet_eddy'


df_for_rep2 <- rbind(df_for_rep2_sst, df_for_rep2_chloro,
                     df_for_rep2_0_200, df_for_rep2_200_700)
df_for_rep2$n_perc <- df_for_rep2$n*100/df_for_rep2$n_tot


df_for_rep2$effet_eddy_fact <- as.factor(df_for_rep2$effet_eddy)
unique(df_for_rep2$effet_eddy_fact) 

levels(df_for_rep2$effet_eddy_fact) 
levels(df_for_rep2$effet_eddy_fact) <- c('Decrease',
                                         'Null', 
                                         'Increase')
df_for_rep2$effet_eddy_fact <- factor(df_for_rep2$effet_eddy_fact, 
                                      levels =  c( 'Increase' ,'Null',
                                                   'Decrease' ))

df_for_rep2$vertical_layer <- as.factor(df_for_rep2$vertical_layer)
levels(df_for_rep2$vertical_layer)
df_for_rep2$vertical_layer <- factor(df_for_rep2$vertical_layer, 
                                     levels =  c("NASC,\n200-750m" ,"NASC,\n0-200m" ,
                                                 "Chlorophyll" , "SST"))

levels(df_for_rep2$effet_eddy_fact) 

df_for_all_levels <- data.frame(expand.grid(effet_eddy_fact = unique(df_for_rep2$effet_eddy_fact),
                                            cyclonic_type = unique(df_for_rep2$cyclonic_type),
                                            # cluster = unique(df_for_rep2$cluster),
                                            vertical_layer = unique(df_for_rep2$vertical_layer)))
df_for_all_levels$nperc2 <- 1
dede = data.frame(effet_eddy_fact = unique(df_for_rep2$effet_eddy_fact))
dede$effet_eddy = c(-1, 0, 1)
df_for_all_levels = merge(df_for_all_levels, dede)

df_for_rep2 <- merge(df_for_rep2, df_for_all_levels, all = TRUE)
df_for_rep2[is.na(df_for_rep2$n_perc), 'n_perc'] <- 0

df_for_rep2$effet_eddy_0_1 = ifelse(df_for_rep2$effet_eddy == 0, 0, 1)

### 

aa = ggplot(df_for_rep2, 
            aes(x = vertical_layer , y =  n_perc, fill = effet_eddy_fact   )) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_classic()  +  ylab('Number of eddies (%)') + xlab('') +
  facet_grid(~ cyclonic_type) + 
  scale_color_manual(values = c('black', 'grey70', 'black'),
                     guide = 'none') +
  scale_fill_manual(values = c( "#8DAD37" ,'grey70',"#D75568"),
                    name = 'Eddy effect', drop = FALSE, ) +
  theme(panel.grid.major.y = element_line(color = 'grey95'),
        legend.spacing.x = unit(0.1, 'cm')) +
  theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(legend.position = 'right',
        legend.background = element_blank(),
        axis.text = element_text(size = 12, face = 'plain'),
        legend.text = element_text(size = 12, face = 'plain'),
        strip.background = element_blank(),
        strip.text = element_text(face = 'plain', size = 12),
        strip.placement = "outside")
aa

ggsave(file = 'figures/Figure3.jpg',
       plot = aa, width = 3, height = 1.6,  scale = 3)

