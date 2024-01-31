## Zooplankton Size Spectra - Slope & Height ## 

rm(list = ls())

# Load in data # 
library(tidyverse)
library(lubridate)

fits_dat = read_csv('fitsdata_zooplankton_long.csv')
fits_dat

max(fits_dat$slope)
min(fits_dat$slope)

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
# Slope through time #===============================
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

# BLUE # 
blue_dat = fits_dat %>% 
  filter(lake == 'Blue')
blue_dat

plot(slope~year, data = blue_dat, pch = 20, ylim = c(-1, 1.5),   xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_20, xaxt = 'n')
arrows(x0=blue_dat$year, y0=blue_dat$slope-blue_dat$slp_se, 
       x1=blue_dat$year, y1=blue_dat$slope+blue_dat$slp_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = 0, lty =3) 


# STORM # 
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm') %>%
  filter(season == 'summer')
storm.su_dat

plot(slope~year, data = storm.su_dat, pch = 20, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19, col.axis = transparent, xaxt = 'n')
arrows(x0=storm.su_dat$year, y0=storm.su_dat$slope-storm.su_dat$slp_se, 
       x1=storm.su_dat$year, y1=storm.su_dat$slope+storm.su_dat$slp_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = 0, lty =3)
storm.sp_dat = fits_dat %>%
  filter(lake == 'Storm') %>%
  filter(season == 'spring')
storm.sp_dat

points(slope~year, data = storm.sp_dat, pch = 22, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$slope-storm.sp_dat$slp_se,
       x1=storm.sp_dat$year, y1=storm.sp_dat$slope+storm.sp_dat$slp_se, code = 3, angle=90, length=0)

# SOUTH TWIN # 
st.su_dat = fits_dat %>%
  filter(lake == 'South.Twin') %>%
  filter(season == 'summer')
st.su_dat

plot(slope~year,data = st.su_dat, pch = 20, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.su_dat$year, y0=st.su_dat$slope-st.su_dat$slp_se, 
       x1=st.su_dat$year, y1=st.su_dat$slope+st.su_dat$slp_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = 0, lty =3)

st.sp_dat = fits_dat %>%
  filter(lake == 'South.Twin') %>%
  filter(season == 'spring')
st.sp_dat

points(slope~year, data = st.sp_dat, pch = 22, ylim = c(-1, 1.5), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18)
arrows(x0=st.sp_dat$year, y0=st.sp_dat$slope-st.sp_dat$slp_se, 
       x1=st.sp_dat$year, y1=st.sp_dat$slope+st.sp_dat$slp_se, code = 3, angle=90, length=0)

## RRN ##======================
# CENTER # 
center.su_dat = fits_dat %>%
  filter(lake == 'Center') %>%
  filter(season == 'summer')
center.su_dat

plot(slope~year, data = center.su_dat, pch = 20, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n')
arrows(x0=center.su_dat$year, y0=center.su_dat$slope-center.su_dat$slp_se, 
       x1=center.su_dat$year, y1=center.su_dat$slope+center.su_dat$slp_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)

center.sp_dat = fits_dat %>%
  filter(lake == 'Center') %>%
  filter(season == 'spring')
center.sp_dat

points(slope~year, data = center.sp_dat, pch = 22, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20)
arrows(x0=center.sp_dat$year, y0=center.sp_dat$slope-center.sp_dat$slp_se,
       x1=center.sp_dat$year, y1=center.sp_dat$slope+center.sp_dat$slp_se, code = 3, angle=90, length=0)
abline(h = 0, lty =3)
abline(v = 2017.65)
abline(v = 2018.5)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five.Island') %>%
  filter(season == 'summer')
five.island.su_dat

plot(slope~year, data = five.island.su_dat, pch = 20, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_19, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$slope-five.island.su_dat$slp_se, 
       x1=five.island.su_dat$year, y1=five.island.su_dat$slope+five.island.su_dat$slp_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)

five.island.sp_dat = fits_dat %>%
  filter(lake == 'Five.Island') %>%
  filter(season == 'spring')
five.island.sp_dat

points(slope~year, data = five.island.sp_dat, pch = 22, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_19)
arrows(x0=five.island.sp_dat$year, y0=five.island.sp_dat$slope-five.island.sp_dat$slp_se,
       x1=five.island.sp_dat$year, y1=five.island.sp_dat$slope+five.island.sp_dat$slp_se, code = 3, angle=90, length=0)
abline(h = 0, lty =3)
abline(v = 2017.65)
abline(v = 2018.5)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
legend("center", legend =c('summer', 'spring'), pch=c(20,22), pt.cex=3, cex=1.5, bty='n',
       col = 'gray60')

## NRR ##==========================
# NORTH TWIN # 
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North.Twin') %>%
  filter(season == 'summer')
north.twin.su_dat

plot(slope~year, data = north.twin.su_dat, pch = 20, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n')
arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$slope-north.twin.su_dat$slp_se, 
       x1=north.twin.su_dat$year, y1=north.twin.su_dat$slope+north.twin.su_dat$slp_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)

north.twin.sp_dat = fits_dat %>%
  filter(lake == 'North.Twin') %>%
  filter(season == 'spring')
north.twin.sp_dat

points(slope~year, data = north.twin.sp_dat, pch = 22, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20)
arrows(x0=north.twin.sp_dat$year, y0=north.twin.sp_dat$slope-north.twin.sp_dat$slp_se,
       x1=north.twin.sp_dat$year, y1=north.twin.sp_dat$slope+north.twin.sp_dat$slp_se, code = 3, angle=90, length=0)
abline(h = 0, lty =3)
abline(v = 2018.5)
abline(v = 2019.5)

# SILVER # 
silver.su_dat = fits_dat %>%
  filter(lake == 'Silver') %>%
  filter(season == 'summer')
silver.su_dat

plot(slope~year, data = silver.su_dat, pch = 20, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_19, xaxt = 'n', col.axis = transparent)
arrows(x0=silver.su_dat$year, y0=silver.su_dat$slope-silver.su_dat$slp_se, 
       x1=silver.su_dat$year, y1=silver.su_dat$slope+silver.su_dat$slp_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)

silver.sp_dat = fits_dat %>%
  filter(lake == 'Silver') %>%
  filter(season == 'spring')
silver.sp_dat

points(slope~year, data = silver.sp_dat, pch = 22, ylim = c(-1, 1.5),  xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_19)
arrows(x0=silver.sp_dat$year, y0=silver.sp_dat$slope-silver.sp_dat$slp_se,
       x1=silver.sp_dat$year, y1=silver.sp_dat$slope+silver.sp_dat$slp_se, code = 3, angle=90, length=0)
abline(h = 0, lty =3)
abline(v = 2018.5)
abline(v = 2019.5)

# Height through time #===============================

max(fits_dat$intcpt)
min(fits_dat$intcpt)

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

# BLUE # 
blue_dat = fits_dat %>% 
  filter(lake == 'Blue')
blue_dat

plot(height~year, data = blue_dat, pch = 20, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_20, xaxt = 'n')
#arrows(x0=blue_dat$year, y0=blue_dat$height-blue_dat$int_se, 
      # x1=blue_dat$year, y1=blue_dat$height+blue_dat$int_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
# STORM # 
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm') %>%
  filter(season == 'summer')
storm.su_dat

plot(height~year, data = storm.su_dat, pch = 20, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19, col.axis = transparent, xaxt = 'n')
#arrows(x0=storm.su_dat$year, y0=storm.su_dat$height-storm.su_dat$int_se, 
      # x1=storm.su_dat$year, y1=storm.su_dat$height+storm.su_dat$int_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)

storm.sp_dat = fits_dat %>%
  filter(lake == 'Storm') %>%
  filter(season == 'spring')
storm.sp_dat

points(height~year, data = storm.sp_dat, pch = 22, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
#arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$height-storm.sp_dat$int_se,
     #  x1=storm.sp_dat$year, y1=storm.sp_dat$height+storm.sp_dat$int_se, code = 3, angle=90, length=0)

# SOUTH TWIN # 
st.su_dat = fits_dat %>%
  filter(lake == 'South.Twin') %>%
  filter(season == 'summer')
st.su_dat

plot(height~year,data = st.su_dat, pch = 20, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
#arrows(x0=st.su_dat$year, y0=st.su_dat$height-st.su_dat$int_se, 
      # x1=st.su_dat$year, y1=st.su_dat$height+st.su_dat$int_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)

st.sp_dat = fits_dat %>%
  filter(lake == 'South.Twin') %>%
  filter(season == 'spring')
st.sp_dat

points(height~year, data = st.sp_dat, pch = 22, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18)
#arrows(x0=st.sp_dat$year, y0=st.sp_dat$height-st.sp_dat$int_se, 
       #x1=st.sp_dat$year, y1=st.sp_dat$height+st.sp_dat$int_se, code = 3, angle=90, length=0)

## RRN ##======================
# CENTER # 
center.su_dat = fits_dat %>%
  filter(lake == 'Center') %>%
  filter(season == 'summer')
center.su_dat

plot(height~year, data = center.su_dat, pch = 20, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n')
#arrows(x0=center.su_dat$year, y0=center.su_dat$height-center.su_dat$int_se, 
      # x1=center.su_dat$year, y1=center.su_dat$height+center.su_dat$int_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(v = 2017.65)
abline(v = 2018.5)

center.sp_dat = fits_dat %>%
  filter(lake == 'Center') %>%
  filter(season == 'spring')
center.sp_dat

points(height~year, data = center.sp_dat, pch = 22, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20)
#arrows(x0=center.sp_dat$year, y0=center.sp_dat$height-center.sp_dat$int_se,
       #x1=center.sp_dat$year, y1=center.sp_dat$height+center.sp_dat$int_se, code = 3, angle=90, length=0)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five.Island') %>%
  filter(season == 'summer')
five.island.su_dat

plot(height~year, data = five.island.su_dat, pch = 20, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_19, xaxt = 'n', col.axis = transparent)
#arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$height-five.island.su_dat$int_se, 
      # x1=five.island.su_dat$year, y1=five.island.su_dat$height+five.island.su_dat$int_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)

five.island.sp_dat = fits_dat %>%
  filter(lake == 'Five.Island') %>%
  filter(season == 'spring')
five.island.sp_dat

points(height~year, data = five.island.sp_dat, pch = 22, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_19)
abline(v = 2017.65)
abline(v = 2018.5)
#arrows(x0=five.island.sp_dat$year, y0=five.island.sp_dat$height-five.island.sp_dat$int_se,
       #x1=five.island.sp_dat$year, y1=five.island.sp_dat$height+five.island.sp_dat$int_se, code = 3, angle=90, length=0)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
legend("center", legend =c('summer', 'spring'), pch=c(20,22), pt.cex=3, cex=1.5, bty='n',
       col = 'gray60')

## NRR ##==========================
# NORTH TWIN # 
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North.Twin') %>%
  filter(season == 'summer')
north.twin.su_dat

plot(height~year, data = north.twin.su_dat, pch = 20, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n')
#arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$height-north.twin.su_dat$int_se, 
       #x1=north.twin.su_dat$year, y1=north.twin.su_dat$height+north.twin.su_dat$int_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)

north.twin.sp_dat = fits_dat %>%
  filter(lake == 'North.Twin') %>%
  filter(season == 'spring')
north.twin.sp_dat

points(height~year, data = north.twin.sp_dat, pch = 22, ylim = c(0, 40), xlim = c(2018.5, 2019.5), cex = 2, col = nrr_col_20)
#arrows(x0=north.twin.sp_dat$year, y0=north.twin.sp_dat$height-north.twin.sp_dat$int_se,
       #x1=north.twin.sp_dat$year, y1=north.twin.sp_dat$height+north.twin.sp_dat$int_se, code = 3, angle=90, length=0)
abline(v = 2018.5)
abline(v = 2019.5)

# SILVER # 
silver.su_dat = fits_dat %>%
  filter(lake == 'Silver') %>%
  filter(season == 'summer')
silver.su_dat

plot(height~year, data = silver.su_dat, pch = 20, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_19, xaxt = 'n', col.axis = transparent)
#arrows(x0=silver.su_dat$year, y0=silver.su_dat$height-silver.su_dat$int_se, 
      # x1=silver.su_dat$year, y1=silver.su_dat$height+silver.su_dat$int_se, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)

silver.sp_dat = fits_dat %>%
  filter(lake == 'Silver') %>%
  filter(season == 'spring')
silver.sp_dat

points(height~year, data = silver.sp_dat, pch = 22, ylim = c(0, 40), xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_19)
#arrows(x0=silver.sp_dat$year, y0=silver.sp_dat$height-silver.sp_dat$int_se,
      # x1=silver.sp_dat$year, y1=silver.sp_dat$height+silver.sp_dat$int_se, code = 3, angle=90, length=0)
abline(v = 2018.5)
abline(v = 2019.5)

# Change in Slope v. kg/ha removed #========================== 
library(tidyverse)
library(lubridate)
fish_harv = read_csv('fish_harvest.csv')
fish_harv

# Create Change in slope column # 
fits_dat = read_csv('fitsdata_zooplankton_long.csv')
fits_dat

fits_summer = fits_dat %>%
  filter(season == 'summer') %>% 
  select(year, lake, season, slope) %>%
  pivot_wider(id_cols = c(lake, season), names_from = year, values_from = slope)
fits_summer

fits_spring = fits_dat %>% 
  filter(season == 'spring') %>%
  select(year, lake, season, slope) %>%
  pivot_wider(id_cols = c(lake, season), names_from = year, values_from = slope) 
fits_spring

chng_summer = fits_summer %>% 
  mutate(delta.1918 = abs(`2019` - `2018`)) %>%
  mutate(delta.2019 = abs(`2020` - `2019`)) %>% 
  select(lake, season, delta.1918, delta.2019) %>%
  pivot_longer(cols = c(delta.1918, delta.2019),names_to = 'delta', values_to = 'slp_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 7), rep('2020', 7))
chng_summer = chng_summer %>% mutate(year = as.double(year))

chng_spring = fits_spring %>% 
  mutate(delta.1918 = abs(`2019` - `2018`)) %>%
  mutate(delta.2019 = abs(`2020` - `2019`)) %>% 
  select(lake, season, delta.1918, delta.2019) %>%
  pivot_longer(cols = c(delta.1918, delta.2019),names_to = 'delta', values_to = 'slp_change') %>% 
  arrange(delta)
chng_spring

chng_spring$year = c(rep('2019', 6), rep('2020', 6))
chng_spring = chng_spring %>% mutate(year = as.double(year))
chng_spring

harv = fish_harv %>%
  filter(year != 2018)
harv



slop_harv_summer = left_join(chng_summer, harv, by = c('year', 'lake'))
slop_harv_summer

slop_harv_spring = left_join(chng_spring, harv, by = c('year', 'lake'))
slop_harv_spring


slop_harv_summer = left_join(chng_summer, harv, by = c('year', 'lake'))
slop_harv_summer

slop_harv_spring = left_join(chng_spring, harv, by = c('year', 'lake'))
slop_harv_spring

## Combined spring - summer plots ##========================
# Join harvest data to slope and height data # 
windows(height = 6, width = 6) 
par(mfrow = c(2,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5)) 
# Set dimensions for figure array # 
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Carp 
plot(slp_change ~ carp_harv, data = slop_harv_summer, cex = 1.5, pch = 20, ylim = c(0, 0.7), col.axis = transparent,  
     ylab = '|Change in slope|', xlab = 'Common Carp Harvested (kg/ha)')
mod1 = lm(slp_change ~ carp_harv, data = slop_harv_summer)
summary(mod1)
abline(mod1)
text(60, 0.3, 'p = 0.23',  cex = 1.5) 

points(slp_change ~ carp_harv, data = slop_harv_spring, cex = 1.5, pch = 18, col = 'gray60',
     ylab = '|Change in slope|', xlab = 'Common Carp Harvested (kg/ha)')
mod2 = lm(slp_change ~ carp_harv, data = slop_harv_spring)
summary(mod2)
abline(mod2, col = 'gray60', lty = 2)
text(60, 0.24, 'p = 0.04*',  cex = 1.5, col = 'gray60')
legend('bottomright', legend = c('summer', 'spring'), col = c('black', 'gray60'), pch = c(20, 18), cex = 1.5, bty = 'n')
#mtext(side = 1, line = 2, 'Common Carp Removed')
mtext(side = 2, line = 2, '|Change in slope|')
axis(side = 2, at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), labels = T)

# Buffalo 
plot(slp_change ~ buff_harv, data = slop_harv_summer, cex = 1.5, pch = 20, ylim = c(0, 0.7), col.axis = transparent,  
     ylab = '|Change in slope|', xlab = 'Bigmouth Buffalo Harvested (kg/ha)')
mod3 = lm(slp_change ~ buff_harv, data = slop_harv_summer)
summary(mod3)
abline(mod3)
text(311, 0.3, 'p = 0.23', cex = 1.5)

points(slp_change ~ buff_harv, data = slop_harv_spring, cex = 1.5, pch = 18, col = 'gray60',
     ylab = '|Change in slope|', xlab = 'Bigmouth Buffalo Harvested (kg/ha)')
mod4 = lm(slp_change ~ buff_harv, data = slop_harv_spring)
summary(mod4)
abline(mod4, col = 'gray60', lty = 2)
text(308, 0.23, 'p = 0.21', font = 2, cex = 1.5, col = 'gray60')
#mtext(side = 1, line = 2, 'Bigmouth Buffalo Removed')

# Change in Height v. kg/ha removed #========================== 

fits_summer = fits_dat %>%
  filter(season == 'summer') %>% 
  select(year, lake, season, height) %>%
  pivot_wider(id_cols = c(lake, season), names_from = year, values_from = height)
fits_summer

fits_spring = fits_dat %>% 
  filter(season == 'spring') %>%
  select(year, lake, season, height) %>%
  pivot_wider(id_cols = c(lake, season), names_from = year, values_from = height) 
fits_spring

chng_summer = fits_summer %>% 
  mutate(delta.1918 = abs(`2019` - `2018`)) %>%
  mutate(delta.2019 = abs(`2020` - `2019`)) %>% 
  select(lake, season, delta.1918, delta.2019) %>%
  pivot_longer(cols = c(delta.1918, delta.2019),names_to = 'delta', values_to = 'hgt_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 7), rep('2020', 7))
chng_summer = chng_summer %>% mutate(year = as.double(year))

chng_spring = fits_spring %>% 
  mutate(delta.1918 = abs(`2019` - `2018`)) %>%
  mutate(delta.2019 = abs(`2020` - `2019`)) %>% 
  select(lake, season, delta.1918, delta.2019) %>%
  pivot_longer(cols = c(delta.1918, delta.2019),names_to = 'delta', values_to = 'hgt_change') %>% 
  arrange(delta)
chng_spring

chng_spring$year = c(rep('2019', 6), rep('2020', 6))
chng_spring = chng_spring %>% mutate(year = as.double(year))
chng_spring

harv = fish_harv %>%
  filter(year != 2018)
harv

height_harv_summer = left_join(chng_summer, harv, by = c('year', 'lake'))
height_harv_summer

height_harv_spring = left_join(chng_spring, harv, by = c('year', 'lake'))
height_harv_spring

# Carp 
plot(hgt_change ~ carp_harv, data = height_harv_summer, cex = 1.5, pch = 20, ylim = c(0, 2.3),  
     ylab = '|Change in height|', xlab = 'Common Carp Harvested (kg/ha)')
mod1 = lm(hgt_change ~ carp_harv, data = height_harv_summer)
summary(mod1)
abline(mod1)
text(60, 2, 'p = 0.48',  cex = 1.5) 

points(hgt_change ~ carp_harv, data = height_harv_spring, cex = 1.5, pch = 18, col = 'gray60',
       ylab = '|Change in height|', xlab = 'Common Carp Harvested (kg/ha)')
mod2 = lm(hgt_change ~ carp_harv, data = height_harv_spring)
summary(mod2)
abline(mod2, col = 'gray60', lty = 2)
text(60, 1.8, 'p = 0.72',  cex = 1.5, col = 'gray60')
#legend(42, 0.2, legend = c('summer', 'spring'), col = c('black', 'gray60'), pch = c(20, 18), cex = 1, bty = 'n')
mtext(side = 1, line = 2, 'Common Carp Removed (kg/ha)')
mtext(side = 2, line = 2, '|Change in height|')

# Buffalo 
plot(hgt_change ~ buff_harv, data = height_harv_summer, cex = 1.5, pch = 20, ylim = c(0, 2.3), col.axis = transparent,
     ylab = '|Change in height|', xlab = 'Bigmouth Buffalo Harvested (kg/ha)')
mod3 = lm(hgt_change ~ buff_harv, data = height_harv_summer)
summary(mod3)
abline(mod3)
text(311, 1.2, 'p = 0.23', cex = 1.5)
axis(side = 1, at = c(0, 100, 200, 300), labels = T)

points(hgt_change ~ buff_harv, data = height_harv_spring, cex = 1.5, pch = 18, col = 'gray60',
       ylab = '|Change in height|', xlab = 'Bigmouth Buffalo Harvested (kg/ha)')
mod4 = lm(hgt_change ~ buff_harv, data = height_harv_spring)
summary(mod4)
abline(mod4, col = 'gray60', lty = 2)
text(311, 1, 'p = 0.78', cex = 1.5, col = 'gray60')
mtext(side = 1, line = 2, 'Bigmouth Buffalo Removed (kg/ha)')

