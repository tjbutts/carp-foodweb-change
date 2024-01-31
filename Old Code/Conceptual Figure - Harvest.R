## Conceptual Figure - Scaled Circles ## 

library(tidyverse)

fish = read_csv('fishharv_exploitation.csv')
fish  

fish.exploit = fish %>% 
  select(lake, year, fish, kg.ha, kg.ha.l95, kg.ha.u95, 
         harvest_kgha, perc.expl, perc.expl.l95, perc.expl.u95)
fish.exploit


# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref = rgb(91, 83, 147, max = 255, alpha = 255)

# null, removal, removal # North Twin, Silver
nrr_col_18 = rgb(43, 73, 112, max = 255, alpha = 100) 
nrr_col_19 = rgb(43, 73, 112, max = 255, alpha = 180)
nrr = rgb(43, 73, 112, max = 255, alpha = 255)

# removal, removal, null # Center, Five Island 
rrn_col_18 = rgb(37, 111, 92, max = 255, alpha = 100) 
rrn_col_19 = rgb(37, 111, 92, max = 255, alpha = 180)
rrn = rgb(37, 111, 92, max = 255, alpha = 255)

buff = fish.exploit %>%
  filter(fish == 'buffalo')
carp = fish.exploit %>% 
  filter(fish == 'carp')

windows(height = 5, width = 8)
ggplot(carp) + 
  geom_point(aes(x = year, y = lake, size = perc.expl, color = fish)) + 
  scale_color_manual( values = rev(c('seagreen', 'dodgerblue'))) + 
  scale_size(range = c(0,10)) +
  theme_bw() + 
  theme ( panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          strip.text.x = element_text(size = 10, face = 'bold.italic' ),
          strip.text.y = element_text(size = 10, face = 'bold.italic' )
          ) + 
  scale_x_continuous(breaks = c(2018.1, 2019.1, 2020.1), 
                    labels = c('2018', '2019', '2020'))

                   