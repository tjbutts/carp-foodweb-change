# Isolate Center 2019 - 2020 for exit seminar # 


# Set working directory to wherever .csv files are # 

# Dataset 1 = fitsdata_zp-miv_long_springsummer.csv
# Datset 2 = shortoutput_zp-miv_springsummer.csv

# Plotting Figure 2 #=====================
#### Size Spectrum analysis of all lakes with slopes ####
# Output graphs with linear fits # 
library(ggplot2)
library(tidyverse)

fits = read_csv('fitsdata_zp-miv_long_springsummer.csv') # summary table of size spectra fits 
short.output = read_csv('shortoutput_zp-miv_springsummer.csv') # plotting data 

output_for_plot = short.output %>% 
  left_join(., fits, by = c('year', 'lake')) # Join makes it easier for ggplot to take the data, of course it creates many rep values 
output_for_plot
output_for_plot$lake = factor(output_for_plot$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))

# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

# null, removal, removal # North Twin, Silver
nrr_col_18 = rgb(43, 73, 112, max = 255, alpha = 100) 
nrr_col_19 = rgb(43, 73, 112, max = 255, alpha = 180)
nrr_col_20 = rgb(43, 73, 112, max = 255, alpha = 255)

# removal, removal, null # Center, Five Island 
rrn_col_18 = rgb(37, 111, 92, max = 255, alpha = 100) 
rrn_col_19 = rgb(37, 111, 92, max = 255, alpha = 180)
rrn_col_20 = rgb(37, 111, 92, max = 255, alpha = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

# Filter down output_for_plot # 
center19.used = filter(output_for_plot, lake == 'Center' & year == 2019 & DATAUSEDINFIT == 'used')
center19.dropped = filter(output_for_plot, lake == 'Center' & year == 2019 & DATAUSEDINFIT == 'dropped')
center20.used = filter(output_for_plot, lake == 'Center' & year == 2020 & DATAUSEDINFIT == 'used')
center20.dropped = filter(output_for_plot, lake == 'Center' & year == 2020 & DATAUSEDINFIT == 'dropped')

windows(height = 4, width = 4)
plot(DENS_LOG~BINMID_LOG, data = center19.used, pch = 20, cex = 2,
     ylim = c(2, 22), xlim = c(-35, 5), col = 'gray50') 
abline(a = unique(center19.used$intcpt), b = unique(center19.used$slope), lwd = 4, col = 'gray50')
#points(DENS_LOG~BINMID_LOG, data = center19.dropped, pch = 1, cex = 2)
points(DENS_LOG~BINMID_LOG, data = center20.used, cex = 2, 
       col = rrn_col_19, pch = 18)
abline(a = unique(center20.used$intcpt), b = unique(center20.used$slope), lwd = 4, col = rrn_col_19)
points(DENS_LOG~BINMID_LOG, data = center20.dropped, cex = 2, 
       col = rrn_col_19, pch = 1)
legend('bottomleft', legend = c('Center 2019', 'Center 2020'), pch = c(20, 18), col = c('gray50', rrn_col_19), bty = 'n', cex = 1)



windows(width = 9, height = 4)
plot_ssa2 = 
  ggplot(output_for_plot, 
         aes(BINMID_LOG, DENS_LOG, shape = DATAUSEDINFIT, color = lake)) + 
  geom_point(size = 1.5) + 
  scale_shape_manual(values = c(1,19)) +
  ylim (2, 22) +
  xlim (-35, 5) +
  facet_grid(year ~ lake) + 
  
  labs( x = expression ( paste ( 'Log'[2], 'Dry Weight Biomass (g)') ), 
        y = expression ( paste ( 'Log'[2], 'Abundance (Individuals/m'^2,')' ) ) ) +
  scale_color_manual(values = c(ref_col_20, ref_col_19, ref_col_18,
                                rrn_col_20, rrn_col_18,
                                nrr_col_20, nrr_col_18)) +
  theme_bw() + 
  theme ( panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          strip.text.x = element_text(size = 10, face = 'bold.italic' ),
          strip.text.y = element_text(size = 10, face = 'bold.italic' ) ) +
  geom_smooth( data = output_for_plot %>% filter(DATAUSEDINFIT == 'used') %>% subset ( fitmax & fitmin), 
               method = lm, se = FALSE)
#+ 
# geom_label(data = output_for_plot, aes(x=-7, y=PLACEMENT, label = format ( round( slope, 2 ), 2) ), show.legend = FALSE)
plot_ssa2