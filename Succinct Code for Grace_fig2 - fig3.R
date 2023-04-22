# Succinct Code for Grace # 

# Set working directory to wherever .csv file is # 

# Dataset 1 = sizespectra_fig2-fits_springsummer.csv
# Datset 2 = fitsdata_summary-table.csv 

# Plotting Figure 2 #=====================
#### Size Spectrum analysis of all lakes with slopes ####
# Output graphs with linear fits # 
library(ggplot2)
library(tidyverse)

output_for_plot = read_csv('sizespectra_output-for-plot.csv') # Data used to plot figure 2 
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


# Plotting Figure 3 #==========================
# Load in data # 
library(tidyverse)
library(lubridate)

# Datasets you'll need # 
fits_dat = read_csv('fitsdata_summary-table.csv') # For Figure 3 - longer version of short output with all fits and slopes 
fits_dat 


# Color # 
# ========= PLOTTING COLORS ===== # 
# Color gradation # 
black18 = rgb(0,0,0, max = 255, alpha = 100)
black19 = rgb(0,0,0, max = 255, alpha = 180)
black20 = rgb(0,0,0, max = 255, alpha = 255)

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

## REFERENCE ##======================
# Window for checking plot 
windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

max(fits_dat$slp_u95ci)
min(fits_dat$slp_l95ci)

# BLUE # 
blue_dat = fits_dat %>% 
  filter(lake == 'Blue') 
blue_dat

plot(slope~year, data = blue_dat, pch = 19, ylim = c(-1.2, -0.10),   xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_20, xaxt = 'n')
arrows(x0=blue_dat$year, y0=blue_dat$slp_l95ci, 
       x1=blue_dat$year, y1=blue_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2, col = ref_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3) 
mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Reference', side = 2, line = 1.8)
mtext('Blue', side = 3, line = 0)

# STORM # 
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm') 
storm.su_dat

plot(slope~year, data = storm.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19, col.axis = transparent, xaxt = 'n')
arrows(x0=storm.su_dat$year, y0=storm.su_dat$slp_l95ci, col = ref_col_19,
       x1=storm.su_dat$year, y1=storm.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3)
mtext(side=3, 'Storm', line = 0)
# storm.sp_dat = fits_dat %>%
#   filter(lake == 'Storm') %>%
#   filter(season == 'spring')
# storm.sp_dat
# 
# points(slope~year, data = storm.sp_dat, pch = 22, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
# arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$slope-storm.sp_dat$slp_se,
#        x1=storm.sp_dat$year, y1=storm.sp_dat$slope+storm.sp_dat$slp_se, code = 3, angle=90, length=0)


# SOUTH TWIN # 
st.su_dat = fits_dat %>%
  filter(lake == 'South.Twin') 
st.su_dat

plot(slope~year,data = st.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.su_dat$year, y0=st.su_dat$slp_l95ci, col = ref_col_18,
       x1=st.su_dat$year, y1=st.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
abline(h = -1, lty =3)
mtext(side = 3, 'South Twin', line = 0)


## RRN ##======================
# CENTER # 
center.su_dat = fits_dat %>%
  filter(lake == 'Center') 
center.su_dat

plot(slope~year, data = center.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n')
arrows(x0=center.su_dat$year, y0=center.su_dat$slp_l95ci, col = rrn_col_20,
       x1=center.su_dat$year, y1=center.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Removal 2018, 2019', side = 2, line = 1.8)
mtext(side =3, 'Center', line = 0 )
abline(h = -1, lty =3)
abline(v = 2017.65)
abline(v = 2018.5)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five.Island') 
five.island.su_dat

plot(slope~year, data = five.island.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$slp_l95ci, col = rrn_col_18,
       x1=five.island.su_dat$year, y1=five.island.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 3, 'Five Island', line = 0)
abline(h = -1, lty =3)
abline(v = 2017.65)
abline(v = 2018.5)


#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
# legend("center", legend =c('summer', 'spring'), pch=c(20,22), pt.cex=3, cex=1.5, bty='n',
#        col = 'gray60')

## NRR ##==========================
# NORTH TWIN # 
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North.Twin') 
north.twin.su_dat

plot(slope~year, data = north.twin.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n')
arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$slp_l95ci, col = nrr_col_20,
       x1=north.twin.su_dat$year, y1=north.twin.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Removal 2019, 2020', side = 2, line = 1.8)
mtext(side =3, 'North Twin', line = 0)

# north.twin.sp_dat = fits_dat %>%
#   filter(lake == 'North.Twin') %>% 
#   filter(season == 'spring')

abline(h = -1, lty =3)
abline(v = 2018.5)
abline(v = 2019.5)

# SILVER # 
silver.su_dat = fits_dat %>%
  filter(lake == 'Silver') 
silver.su_dat

plot(slope~year, data = silver.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=silver.su_dat$year, y0=silver.su_dat$slp_l95ci, col = nrr_col_18,
       x1=silver.su_dat$year, y1=silver.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext(side = 3, 'Silver', line = 0)
mtext(side = 1, 'Year', line = 2)

abline(h = -1, lty =3)
abline(v = 2018.5)
abline(v = 2019.5)

