## Step3_Analyze SSA Slope and Height ##=========================

# Load in libraries 
library(tidyverse)
library(lubridate)
library(car)

# ANCOVA Code - Slope  #============================
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")
setwd("J:/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")

## load in size spectra data  ## 
sizespec = read_csv('shortoutput_zp-miv_springsummer_Blue-rm.csv') %>%
  mutate(year = as.factor(year), 
         lake = as.factor(lake)) %>% 
  filter(DATAUSEDINFIT != 'dropped') %>%
  relocate(DENS_LOG, BINMID_LOG) %>% 
  select(!(DATAUSEDINFIT))
  # Add a dummy variables to assess harvest (No Harvest, Harvest - 1year, Harvest - 2year) 
sizespec



fits_dat = read_csv('fitsdata_zp-miv_long_springsummer_Blue-rm.csv')
fits_dat 

dummy = read_csv('sizespec_dummyvar_Blue-rm.csv') %>%
  mutate(year = as.factor(year),
         lake = as.factor(lake))
dummy


sizespec = left_join(sizespec, dummy, by = c('lake', 'year')) 
sizespec

# Run model selection - don't load in MASS because it'll mask a bunch of tidyverse stuff

# # Uncomment to show stepAIC values and search, running will mask a ton of tidyverse function so commented here 
step.spec = MASS::stepAIC(full.aov, trace = T, direction = 'both', criteria = 'AICc') # forward and backward stepwise regression 
# summary(step.spec) # This will give rundown of all possible combinations 

# then go back from lowest AIC to highest and run AICc to get corrected values 
mod1 = aov(DENS_LOG ~ BINMID_LOG + lake + year + harv2, data = sizespec) # AIC = 701.2
mod2 = aov(DENS_LOG ~ BINMID_LOG + lake + year + harv2 + BINMID_LOG:year, data = sizespec) # AIC = 701.34
mod3 = aov(DENS_LOG ~ BINMID_LOG + lake + year + harv2 + BINMID_LOG*lake +
             BINMID_LOG*year, data = sizespec) # AIC = 702.45 
mod4 = aov(DENS_LOG ~ BINMID_LOG + lake + year + BINMID_LOG*lake + 
             BINMID_LOG*year, data = sizespec) # AIC = 703.96
mod5 = aov(DENS_LOG ~ BINMID_LOG + lake + year + BINMID_LOG*lake + BINMID_LOG*year + 
             lake*year, data = sizespec) # AIC = 715.34
mod6 = aov(DENS_LOG ~ BINMID_LOG + lake + year + BINMID_LOG*lake + BINMID_LOG*year + 
             lake*year + BINMID_LOG*lake*year, data = sizespec) # AIC = 719.65
mod7 = aov(DENS_LOG ~ BINMID_LOG + lake + year + noharv + harv1 + BINMID_LOG*lake + 
             BINMID_LOG*year + lake*year + BINMID_LOG*lake*year, data = sizespec) # AIC = 719.65
mod8 = aov(DENS_LOG ~ BINMID_LOG + lake + year + noharv + harv1 + BINMID_LOG*lake + 
             BINMID_LOG*year + lake*year + BINMID_LOG*lake*year, data = sizespec) # AIC = 719.65
mod9 = aov(DENS_LOG ~ BINMID_LOG + lake + year + noharv + harv1 + harv2 + 
             BINMID_LOG*lake + BINMID_LOG*year + lake*year + BINMID_LOG*lake*year, data = sizespec) # AIC = 719.65
mod10 = aov(DENS_LOG ~ BINMID_LOG + lake + year + noharv + harv1 + harv2 + 
              postharv + BINMID_LOG*lake + BINMID_LOG*year + lake*year + 
              BINMID_LOG*lake*year, data = sizespec) # AIC = 719.65


# Check model AICc # 
library(AICcmodavg)
AICc(mod1, second.ord = T) # AICc = 1739.8
AICc(mod2, second.ord = T) # AICc = 1740.2
AICc(mod3, second.ord = T) # AICc = 1742.3
AICc(mod4, second.ord = T) # AICc = 1743.5
AICc(mod5, second.ord = T) # AICc = 1757.6
AICc(mod6, second.ord = T) # AICc = 1766.1
AICc(mod7, second.ord = T) # AICc = 1766.1
AICc(mod8, second.ord = T) # AICc = 1766.1
AICc(mod9, second.ord = T) # AICc = 1766.1
AICc(mod10, second.ord = T) # AICc = 1766.1


## Model: (log2[Density]~log2[dry weight bin] x year x lake x dummy variables) - full model 
sizespec

# Assess models 
full.aov = aov(DENS_LOG~BINMID_LOG*lake*year*noharv*harv1*harv2*postharv, data = sizespec)
AICc(full.aov, second.ord = TRUE)
reduced = aov(DENS_LOG~BINMID_LOG+lake+year+harv2, data = sizespec)
AICc(reduced, second.ord = TRUE)



# Top 5 models - Adjusted R squared # 
mod1.1 = lm(DENS_LOG~BINMID_LOG+lake+year+harv2, data = sizespec)
summary(mod1.1) # 61.61% error explained 
aov(mod1.1)

sz_model = aov(DENS_LOG~BINMID_LOG + lake + year + harv2, data = sizespec)
summary(sz_model)


# # lake is moderating the relationship between Density and Size 

library(emmeans)
library(rstatix)


# Post hoc differences in lake 
sz_model.emms.lk = emmeans(mod1.1, 'lake')
sz_model.emms.lk

sz_model.emms.lk
pairs(sz_model.emms.lk)


# Regression Code - Height  #============================
heights = fits_dat %>% 
  select(year, lake, height, height.l95ci, height.u95ci, int_se) %>%
  mutate(year = as.numeric(year), 
         lake = as.factor(lake))
heights

# Define weights to use # 
wt.mod1 = aov(height~year*lake, data = heights, weights = (1/int_se^2))
summary(wt.mod1) 

# Run model selection - don't load in MASS because it'll mask a bunch of tidyverse stuff 
# step.spec.height = MASS::stepAIC(wt.mod1, trace = T, direction = 'both') # forward and backward stepwise regression 
# summary(step.spec.height)

wt.mod.fin = lm(height~year+lake, data = heights, weights = (1/int_se^2))
summary(wt.mod.fin)


# Create resid v. fit # 
plot(fitted(wt.mod.fin), resid(wt.mod1), xlab = 'fitted', ylab = 'residuals')
abline(0,0) # Not bad 


# Plotting Slope Dynamics #==========================
# Load in data # 
library(tidyverse)
library(lubridate)



# Color # 
# ========= PLOTTING COLORS ===== # 
# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

# removal, removal, null # Center, Five Island 
rrn_col_18 = rgb(37, 111, 92, max = 255, alpha = 100) 
rrn_col_19 = rgb(37, 111, 92, max = 255, alpha = 180)
rrn_col_20 = rgb(37, 111, 92, max = 255, alpha = 255)

# null, removal, removal # North Twin, Silver
nrr_col_18 = rgb(77, 77, 77, max = 255, alpha = 100) 
nrr_col_19 = rgb(77, 77, 77, max = 255, alpha = 180)
nrr_col_20 = rgb(77, 77, 77, max = 255, alpha = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

## REFERENCE ##======================
# Window for checking plot 
windows(height = 5.5, width = 4) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

max(fits_dat$slp_u95ci)
min(fits_dat$slp_l95ci)

# STORM # 
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm') 
storm.su_dat

plot(slope~year, data = storm.su_dat, pch = 19, ylim = c(-1.2, -0.10),   xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_20, xaxt = 'n')
arrows(x0=storm.su_dat$year, y0=storm.su_dat$slp_l95ci,
       x1=storm.su_dat$year, y1=storm.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2, col = ref_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3)
mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Reference', side = 2, line = 1.8)
mtext('Storm', side = 3, line = 0)
text(2017.53, -0.12, 'A', cex = 1.2)

# STORM # 
# storm.su_dat = fits_dat %>%
#   filter(lake == 'Storm') 
# storm.su_dat
# 
# plot(slope~year, data = storm.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19, col.axis = transparent, xaxt = 'n')
# arrows(x0=storm.su_dat$year, y0=storm.su_dat$slp_l95ci, col = ref_col_19,
#        x1=storm.su_dat$year, y1=storm.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
# axis(side = 1, at = c(2018, 2019, 2020), labels = F)
# abline(h = -1, lty =3)
# mtext(side=3, 'Storm', line = 0)
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
  filter(lake == 'South Twin') 
st.su_dat

plot(slope~year,data = st.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.su_dat$year, y0=st.su_dat$slp_l95ci, col = ref_col_18,
       x1=st.su_dat$year, y1=st.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(h = -1, lty =3)
mtext(side = 3, 'South Twin', line = 0)
text(2017.53, -0.12, 'B', cex = 1.2)

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
text(2017.53, -0.12, 'C', cex = 1.2)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five Island') 
five.island.su_dat

plot(slope~year, data = five.island.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$slp_l95ci, col = rrn_col_18,
       x1=five.island.su_dat$year, y1=five.island.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 3, 'Five Island', line = 0)
abline(h = -1, lty =3)
abline(v = 2017.65)
abline(v = 2018.5)
text(2017.53, -0.12, 'D', cex = 1.2)

# 
# #empty plot 
# plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
#      ylab = "", xlim = c(0, 5), 
#      ylim = c(0, 5), col.axis = transparent, bty = 'n')
# # legend("center", legend =c('summer', 'spring'), pch=c(20,22), pt.cex=3, cex=1.5, bty='n',
# #        col = 'gray60')

## NRR ##==========================
# NORTH TWIN # 
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North Twin') 
north.twin.su_dat

plot(slope~year, data = north.twin.su_dat, pch = 19, ylim = c(-1.2, -0.10),  xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n')
arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$slp_l95ci, col = nrr_col_20,
       x1=north.twin.su_dat$year, y1=north.twin.su_dat$slp_u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext('Size Spectra Slope', side = 2, line = 3)
mtext('Removal 2019, 2020', side = 2, line = 1.8)
mtext(side =3, 'North Twin', line = 0)
mtext(side = 1, 'Year', line = 2)
text(2017.53, -0.12, 'E', cex = 1.2)

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
text(2017.53, -0.12, 'F', cex = 1.2)

# Make sure height is independent of slope #===============================
fits_dat
library(ggpubr)

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
ggscatter(fits_dat, x = 'slope', y = 'height', 
          add = 'reg.line', conf.int = T, 
          xlim = c(-0.75, -0.25),
          cor.coef = T, cor.method = 'pearson', 
          xlab = 'Size Spectra Slope', ylab = 'Size Spectra Height')

# Absolutely no correlation # 

# Plotting Height through time #===============================

max(fits_dat$height.u95ci)
min(fits_dat$height.l95ci)

## REFERENCE ##======================
# Window for checking plot 
windows(height = 5.5, width = 4) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# STORM #
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm')
storm.su_dat

plot(height~year, data = storm.su_dat, pch = 19, ylim = c(10, 16), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_20, xaxt = 'n')
arrows(x0=storm.su_dat$year, y0=storm.su_dat$height.l95ci,col = ref_col_20,
 x1=storm.su_dat$year, y1=storm.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 2, 'Size Spectra Height', line = 3)
mtext('Reference', side = 2, line = 1.8)
mtext(side =3, line = 0, 'Storm')
text(2017.53, 15.9, 'A', cex = 1.2)

# storm.sp_dat = fits_dat %>%
#   filter(lake == 'Storm') %>%
#   filter(season == 'spring')
# storm.sp_dat
# 
# points(height~year, data = storm.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_19)
# #arrows(x0=storm.sp_dat$year, y0=storm.sp_dat$height-storm.sp_dat$int_se,
# #  x1=storm.sp_dat$year, y1=storm.sp_dat$height+storm.sp_dat$int_se, code = 3, angle=90, length=0)

# SOUTH TWIN # 
st.su_dat = fits_dat %>%
  filter(lake == 'South Twin') 
st.su_dat

plot(height~year,data = st.su_dat, pch = 19, ylim = c(10, 16), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.su_dat$year, y0=st.su_dat$height.l95ci, col = ref_col_18, 
 x1=st.su_dat$year, y1=st.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 3, 'South Twin', line = 0)
text(2017.53, 15.9, 'B', cex = 1.2)

# st.sp_dat = fits_dat %>%
#   filter(lake == 'South.Twin') %>%
#   filter(season == 'spring')
# st.sp_dat
# 
# points(height~year, data = st.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18)
# #arrows(x0=st.sp_dat$year, y0=st.sp_dat$height-st.sp_dat$int_se, 
#x1=st.sp_dat$year, y1=st.sp_dat$height+st.sp_dat$int_se, code = 3, angle=90, length=0)

## RRN ##======================
# CENTER # 
center.su_dat = fits_dat %>%
  filter(lake == 'Center') 
center.su_dat

plot(height~year, data = center.su_dat, pch = 19, ylim = c(10, 16), xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n')
arrows(x0=center.su_dat$year, y0=center.su_dat$height.l95ci, col = rrn_col_20,
 x1=center.su_dat$year, y1=center.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(v = 2017.65)
abline(v = 2018.5)
mtext(side = 2, 'Size Spectra Height', line = 3)
mtext('Removal 2018, 2019', side = 2, line = 1.8)
mtext(side =3, 'Center', line = 0)
text(2017.53, 15.9, 'C', cex = 1.2)

# center.sp_dat = fits_dat %>%
#   filter(lake == 'Center') %>%
#   filter(season == 'spring')
# center.sp_dat
# 
# points(height~year, data = center.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2017.5, 2020.5), cex = 3, col = rrn_col_20)
# #arrows(x0=center.sp_dat$year, y0=center.sp_dat$height-center.sp_dat$int_se,
# #x1=center.sp_dat$year, y1=center.sp_dat$height+center.sp_dat$int_se, code = 3, angle=90, length=0)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five Island') 
five.island.su_dat

plot(height~year, data = five.island.su_dat, pch = 19, ylim = c(10, 16), xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$height.l95ci, col = rrn_col_18,
 x1=five.island.su_dat$year, y1=five.island.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side =3, 'Five Island', line = 0 )
text(2017.53, 15.9, 'D', cex = 1.2)
# 
# five.island.sp_dat = fits_dat %>%
#   filter(lake == 'Five.Island') %>%
#   filter(season == 'spring')
# five.island.sp_dat
# 
# points(height~year, data = five.island.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_19)
abline(v = 2017.65)
abline(v = 2018.5)

#arrows(x0=five.island.sp_dat$year, y0=five.island.sp_dat$height-five.island.sp_dat$int_se,
#x1=five.island.sp_dat$year, y1=five.island.sp_dat$height+five.island.sp_dat$int_se, code = 3, angle=90, length=0)

# #empty plot 
# plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
#      ylab = "", xlim = c(0, 5), 
#      ylim = c(0, 5), col.axis = transparent, bty = 'n')
# #legend("center", legend =c('summer', 'spring'), pch=c(20,22), pt.cex=3, cex=1.5, bty='n',
#  #      col = 'gray60')

## NRR ##==========================
# NORTH TWIN # 
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North Twin') 
north.twin.su_dat

plot(height~year, data = north.twin.su_dat, pch = 19, ylim = c(10, 16), xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n')
arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$height.l95ci, col = nrr_col_20,
 x1=north.twin.su_dat$year, y1=north.twin.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext(side = 2, 'Size Spectra Height', line = 3)
mtext('Removal 2019, 2020', side = 2, line = 1.8)
mtext(side =3, 'North Twin', line = 0)
mtext(side = 1, 'Year', line = 2)
text(2017.53, 15.9, 'E', cex = 1.2)

# north.twin.sp_dat = fits_dat %>%
#   filter(lake == 'North.Twin') %>%
#   filter(season == 'spring')
# north.twin.sp_dat
# 
# points(height~year, data = north.twin.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2018.5, 2019.5), cex = 2, col = nrr_col_20)
# #arrows(x0=north.twin.sp_dat$year, y0=north.twin.sp_dat$height-north.twin.sp_dat$int_se,
#x1=north.twin.sp_dat$year, y1=north.twin.sp_dat$height+north.twin.sp_dat$int_se, code = 3, angle=90, length=0)
abline(v = 2018.5)
abline(v = 2019.5)

# SILVER # 
silver.su_dat = fits_dat %>%
  filter(lake == 'Silver') 
silver.su_dat

plot(height~year, data = silver.su_dat, pch = 19, ylim = c(10, 16), xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=silver.su_dat$year, y0=silver.su_dat$height.l95ci, col = nrr_col_18,
 x1=silver.su_dat$year, y1=silver.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext(side =3, 'Silver', line = 0)
mtext(side = 1, 'Year', line = 2)
text(2017.53, 15.9, 'F', cex = 1.2)

# silver.sp_dat = fits_dat %>%
#   filter(lake == 'Silver') %>%
#   filter(season == 'spring')
# silver.sp_dat

# points(height~year, data = silver.sp_dat, pch = 22, ylim = c(5, 15), xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_19)
# #arrows(x0=silver.sp_dat$year, y0=silver.sp_dat$height-silver.sp_dat$int_se,
# x1=silver.sp_dat$year, y1=silver.sp_dat$height+silver.sp_dat$int_se, code = 3, angle=90, length=0)
abline(v = 2018.5)
abline(v = 2019.5)

### Cumulative Harvest  ###=====
#=============================================================#

# Change in Slope v. kg/ha removed #========================== 
library(tidyverse)
library(lubridate)
fish_harv = read_csv('fish_harvest.csv') %>% 
  filter(lake != 'Blue')
fish_harv

# Create Change in slope column # 
fits_dat = read_csv('fitsdata_zp-miv_long_springsummer_Blue-rm.csv')
fits_dat

fits_summer = fits_dat %>%
  select(year, lake, slope) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = slope)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'slp_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 6), rep('2020', 12))
chng_summer = chng_summer %>% mutate(year = as.double(year))
chng_summer

cumulative = fish_harv %>% 
  pivot_longer(cols = c(carp_harv, buff_harv), 
               names_to = 'fish', values_to = 'harvest') %>% 
  pivot_wider(names_from = 'year', 
              values_from = 'harvest') %>% 
  group_by(lake, fish) %>% 
  mutate(`2019` = `2018`+`2019`, 
         `2020` = `2019` + `2020`) %>% 
  pivot_longer(cols = c(`2018`, `2019`, `2020`), 
               names_to = 'year', 
               values_to = 'harvest') %>% 
  pivot_wider(names_from = 'fish', 
              values_from = 'harvest') %>% 
  mutate(year = as.double(year))
cumulative

slop_harv_summer = left_join(chng_summer, cumulative, by = c('year', 'lake'))
slop_harv_summer


## Combined spring - summer plots ##========================
# Join harvest data to slope and height data # 
windows(height = 4, width = 5) 
par(mfrow =c(2,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
carp = rgb(0, 0, 0, alpha = 180, max = 255)
buffalo = rgb(204,204,204, alpha = 180, max = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

max(slop_harv_summer$slp_change)
min(slop_harv_summer$slp_change)

# Carp 
plot(slp_change ~ carp_harv, data = slop_harv_summer, cex = 1.5, pch = 19, ylim = c(-0.4, 0.2), col.axis = transparent,
     ylab = '|Change in slope|', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(slp_change ~ carp_harv, data = slop_harv_summer)
axis(side = 2, at = c(-0.2, -0.1, 0, 0.1, 0.2), labels = T, tick = F)
summary(mod1)
abline(mod1, col = carp)
text(70, 0.18, 'p = 0.271',  cex = 1, col = 'black', font = 1)
mtext(side = 2, line = 1.8, 'Change in slope')
mtext(side = 3, line = 0.1, 'Common Carp Harvest')
text(0, 0.19, 'A', cex = 1, col = 'black')

plot(slp_change ~ buff_harv, data = slop_harv_summer, cex = 1.5, pch = 19, col = buffalo, ylim = c(-0.4, 0.2), col.axis = transparent,
       ylab = '|Change in slope|', xlab = 'Fish Harvested (kg/ha)')
mod2 = lm(slp_change ~ buff_harv, data = slop_harv_summer)
summary(mod2)
abline(mod2, col = 'gray80', lty = 2)
text(550, 0.18, 'p = 0.109',  cex = 1, col = 'gray40', font = 1)
mtext(side = 3, line = 0.1, 'Bigmouth Buffalo Harvest')
text(0, 0.19, 'B', cex = 1, col = 'black')


# Change in Height v. kg/ha removed #========================== 
fits_summer = fits_dat %>%
  select(year, lake, height) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = height)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'hgt_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 6), rep('2020', 12))
chng_summer = chng_summer %>% mutate(year = as.double(year))

height_harv_summer = left_join(chng_summer, cumulative, by = c('year', 'lake'))
height_harv_summer

max(height_harv_summer$hgt_change)
min(height_harv_summer$hgt_change)

# Carp 
plot(hgt_change ~ carp_harv, data = height_harv_summer, cex = 1.5, pch = 19, ylim = c(-2.5, 2), 
     ylab = '|Change in height|', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(hgt_change ~ carp_harv, data = height_harv_summer)
summary(mod1)
abline(mod1, col = carp)
text(70, 1.8, 'p = 0.577',  cex = 1, col = 'black') 
mtext(side = 2, line = 1.8, 'Change in height')
mtext(side = 1, line = 1.8, 'Cumulative Harvest (kg/ha)', cex = 1)
text(0, 1.9, 'C', cex = 1, col = 'black')

plot(hgt_change ~ buff_harv, data = height_harv_summer, cex = 1.5, pch = 19, col = buffalo, col.axis = transparent,
       ylab = 'Change in height', xlab = 'Fish Harvested (kg/ha)', ylim = c(-2.5, 2))
mod2 = lm(hgt_change ~ buff_harv, data = height_harv_summer)
summary(mod2)
abline(mod2, col = buffalo, lty = 2)
text(550, 1.8, 'p = 0.330',  cex = 1, col = 'gray40')
axis(side = 1, at = c(0, 200, 400, 600), labels = T, tick = F)
mtext(side = 1, line = 1.8, 'Cumulative Harvest (kg/ha)', cex = 1)
text(0, 1.9, 'D', cex = 1, col = 'black')


# Change in cumulative harvest combining both fish removed totals # 
# Slope 
library(tidyverse)
library(lubridate)

fish_harv = read_csv('fish_harvest.csv') %>%
  filter(lake != 'Blue')
fish_harv

# Create Change in slope column # 
fits_dat = read_csv('fitsdata_zp-miv_long_springsummer_Blue-rm.csv')
fits_dat

fits_summer = fits_dat %>%
  select(year, lake, slope) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = slope)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'slp_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 6), rep('2020', 12))
chng_summer = chng_summer %>% mutate(year = as.double(year))


cumulative = fish_harv %>% 
  mutate(sum_harv = carp_harv + buff_harv) %>% 
  select(lake, year, sum_harv) %>% 
  pivot_longer(cols = c(sum_harv), 
               names_to = 'fish', values_to = 'harvest') %>% 
  pivot_wider(names_from = 'year', 
              values_from = 'harvest') %>%
  group_by(lake) %>% 
  mutate(`2019` = `2018`+`2019`, 
         `2020` = `2019` + `2020`) %>% 
  pivot_longer(cols = c(`2018`, `2019`, `2020`), 
               names_to = 'year', 
               values_to = 'harvest') %>%
  pivot_wider(names_from = 'fish', 
              values_from = 'harvest') %>% 
  mutate(year = as.double(year))
cumulative

slop_harv_summer = left_join(chng_summer, cumulative, by = c('year', 'lake'))
slop_harv_summer

windows(height = 3, width = 6) 
par(mfrow =c(1,2), mar = c(0.5,1,2,2), oma = c(4,4,.5,4))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
carp = rgb(0, 0, 0, alpha = 180, max = 255)
buffalo = rgb(204,204,204, alpha = 180, max = 255)
both = rgb(0,0,0, alpha = 180, max = 225)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

max(slop_harv_summer$slp_change)
min(slop_harv_summer$slp_change)

# Slope 
plot(slp_change ~ sum_harv, data = slop_harv_summer, cex = 1.5, pch = 19, ylim = c(-0.35, 0.2), xlim = c(-5, 750),
     ylab = 'Change in slope', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(slp_change ~ sum_harv, data = slop_harv_summer)
summary(mod1)
abline(mod1, col = carp)
text(615, 0.18, 'p = 0.10',  cex = 1, col = 'black', font = 1)
mtext(side = 2, line = 1.8, 'Change in slope')
mtext(side = 1, line = 1.8, 'Cumalitive Harvest (kg/ha)') 
text(10, 0.19, 'A')

# Height 
fits_summer = fits_dat %>%
  select(year, lake, height) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = height)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'hgt_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 6), rep('2020', 12))
chng_summer = chng_summer %>% mutate(year = as.double(year))

height_harv_summer = left_join(chng_summer, cumulative, by = c('year', 'lake'))
height_harv_summer

max(height_harv_summer$hgt_change)
min(height_harv_summer$hgt_change)

plot(hgt_change ~ sum_harv, data = height_harv_summer, cex = 1.5, pch = 19, col = buffalo,
     ylab = 'Change in height', xlab = 'Fish Harvested (kg/ha)', ylim = c(-2.5, 2))
mod2 = lm(hgt_change ~ sum_harv, data = height_harv_summer)
summary(mod2)
abline(mod2, col = buffalo, lty = 2)
text(560, 1.8, 'p = 0.39',  cex = 1, col = 'gray40')
mtext(side = 1, line = 1.8, 'Cumulative Harvest (kg/ha)', cex = 1)
mtext(side =2, line =1.8, 'Change in height ')
text(10, 1.9, 'B')

## Slope x change in cumulative Harvest ##==========================
# Change in Slope v. kg/ha removed #========================== 
library(tidyverse)
library(lubridate)
fish_harv = read_csv('fish_harvest.csv') %>% 
  filter(lake != 'Blue')
fish_harv

# Create Change in slope column # 
fits_dat = read_csv('fitsdata_zp-miv_long_springsummer_Blue-rm.csv')
fits_dat

fits_summer = fits_dat %>%
  select(year, lake, slope) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = slope)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'slp_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 6), rep('2020', 12))
chng_summer = chng_summer %>% mutate(year = as.double(year))


cumulative = fish_harv %>% 
  pivot_longer(cols = c(carp_harv, buff_harv), 
               names_to = 'fish', values_to = 'harvest') %>% 
  pivot_wider(names_from = 'year', 
              values_from = 'harvest') %>% 
  group_by(lake, fish) %>% 
  mutate(`2019` = `2018`+`2019`, 
         `2020` = `2019` + `2020`) %>% 
  pivot_longer(cols = c(`2018`, `2019`, `2020`), 
               names_to = 'year', 
               values_to = 'harvest') %>% 
  pivot_wider(names_from = 'fish', 
              values_from = 'harvest') %>% 
  mutate(year = as.double(year))
cumulative

slop_harv_summer = left_join(chng_summer, cumulative, by = c('year', 'lake'))
slop_harv_summer


## Combined spring - summer plots ##========================
# Join harvest data to slope and height data # 
windows(height = 4, width = 5) 
par(mfrow =c(2,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
carp = rgb(0, 0, 0, alpha = 180, max = 255)
buffalo = rgb(204,204,204, alpha = 180, max = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

max(slop_harv_summer$slp_change)
min(slop_harv_summer$slp_change)

# Carp 
plot(slp_change ~ carp_harv, data = slop_harv_summer, cex = 1.5, pch = 19, ylim = c(-0.3, 0.2), col.axis = transparent,
     ylab = '|Change in slope|', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(slp_change ~ carp_harv, data = slop_harv_summer)
axis(side = 2, at = c(-0.2, -0.1, 0, 0.1, 0.2), labels = T, tick = F)
summary(mod1)
abline(mod1, col = carp)
text(70, 0.18, 'p = 0.191',  cex = 1, col = 'black', font = 1)
mtext(side = 2, line = 1.8, 'Change in slope')
mtext(side = 3, line = 0.1, 'Common Carp Harvest')

plot(slp_change ~ buff_harv, data = slop_harv_summer, cex = 1.5, pch = 19, col = buffalo, ylim = c(-0.3, 0.2), col.axis = transparent,
     ylab = '|Change in slope|', xlab = 'Fish Harvested (kg/ha)')
mod2 = lm(slp_change ~ buff_harv, data = slop_harv_summer)
summary(mod2)
abline(mod2, col = 'gray80', lty = 2)
text(550, 0.18, 'p = 0.069',  cex = 1, col = 'gray40', font = 1)
mtext(side = 3, line = 0.1, 'Bigmouth Buffalo Harvest')



# Change in Height v. kg/ha removed #========================== 
fits_summer = fits_dat %>%
  select(year, lake, height) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = height)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'hgt_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 7), rep('2020', 14))
chng_summer = chng_summer %>% mutate(year = as.double(year))

height_harv_summer = left_join(chng_summer, cumulative, by = c('year', 'lake'))
height_harv_summer

max(height_harv_summer$hgt_change)
min(height_harv_summer$hgt_change)

# Carp 
plot(hgt_change ~ carp_harv, data = height_harv_summer, cex = 1.5, pch = 19, ylim = c(-2.5, 2), 
     ylab = '|Change in height|', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(hgt_change ~ carp_harv, data = height_harv_summer)
summary(mod1)
abline(mod1, col = carp)
text(70, 1.8, 'p = 0.723',  cex = 1, col = 'black') 
mtext(side = 2, line = 1.8, 'Change in height')
mtext(side = 1, line = 1.8, 'Cumulative Harvest (kg/ha)', cex = 1)

plot(hgt_change ~ buff_harv, data = height_harv_summer, cex = 1.5, pch = 19, col = buffalo, col.axis = transparent,
     ylab = 'Change in height', xlab = 'Fish Harvested (kg/ha)', ylim = c(-2.5, 2))
mod2 = lm(hgt_change ~ buff_harv, data = height_harv_summer)
summary(mod2)
abline(mod2, col = buffalo, lty = 2)
text(550, 1.8, 'p = 0.239',  cex = 1, col = 'gray40')
axis(side = 1, at = c(0, 200, 400, 600), labels = T, tick = F)
mtext(side = 1, line = 1.8, 'Cumulative Harvest (kg/ha)', cex = 1)

# Change in cumulative harvest combining both fish removed totals # 
# Slope 
library(tidyverse)
library(lubridate)
fish_harv = read_csv('fish_harvest.csv')
fish_harv

# Create Change in slope column # 
fits_dat = read_csv('fitsdata_zp-miv_long_springsummer.csv')
fits_dat

fits_summer = fits_dat %>%
  select(year, lake, slope) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = slope)
fits_summer

slps = fits_summer %>% 
  rename(slp.18 = `2018`, 
         slp.19 = `2019`, 
         slp.20 = `2020`) %>% 
  pivot_longer(cols = c(slp.18, slp.19, slp.20), names_to = 'slope', values_to = 'slope.value') %>% 
  arrange(slope)
slps$year = c(rep('2018', 7), rep('2019', 7), rep('2020', 7))
slps = mutate(slps, year = as.double(year))

cumulative = fish_harv %>% 
  pivot_longer(cols = c(carp_harv, buff_harv), 
               names_to = 'fish', values_to = 'harvest') %>%
  pivot_wider(names_from = 'year', 
              values_from = 'harvest') %>% 
  arrange(fish) %>% 
  group_by(lake) %>% 
  mutate(`2018` = `2018`, 
         `2019` = `2018` + `2019`) %>% 
  mutate(`2020` = `2019` + `2020`) %>%
  ungroup() %>% 
  pivot_longer(cols = c(`2018`, `2019`, `2020`), 
               names_to = 'year', 
               values_to = 'harvest') %>%
  pivot_wider(names_from = 'fish', 
              values_from = 'harvest') %>% 
  mutate(year = as.double(year))
cumulative

slps
slop_harv_summer = left_join(slps, cumulative, by = c('year', 'lake'))
slop_harv_summer

windows(height = 3, width = 6) 
par(mfrow =c(1,2), mar = c(0.5,1,2,2), oma = c(4,4,.5,4))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
carp = rgb(0, 0, 0, alpha = 180, max = 255)
buffalo = rgb(204,204,204, alpha = 180, max = 255)
both = rgb(0,0,0, alpha = 180, max = 225)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

max(slop_harv_summer$slope.value)
min(slop_harv_summer$slope.value)

max(slop_harv_summer$carp_harv)
min(slop_harv_summer$carp_harv)

max(slop_harv_summer$buff_harv)
min(slop_harv_summer$buff_harv)

# Slope 
plot(slope.value ~ carp_harv, data = slop_harv_summer, cex = 1.5, pch = 19, ylim = c(-0.35, -0.76), xlim = c(-5, 100),
     ylab = 'Size Spectra Slope', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(slope.value ~ carp_harv, data = slop_harv_summer)
summary(mod1)
abline(mod1, col = carp)
text(80, -0.75, 'p = 0.71',  cex = 1, col = 'black', font = 1)
mtext(side = 2, line = 1.8, 'Size Spectra Slope')
mtext(side = 1, line = 1.8, 'Carp Harvest (cumul.)') 

plot(slope.value ~ buff_harv, data = slop_harv_summer, cex = 1.5, pch = 19, ylim = c(-0.35, -0.76), xlim = c(-5, 700),
     ylab = '', xlab = '', col = buffalo)
mod2 = lm(slope.value ~ buff_harv, data = slop_harv_summer)
summary(mod2)
abline(mod2, col = buffalo)
text(550, -0.75, 'p = 0.598',  cex = 1, col = buffalo, font = 2)
mtext(side = 2, line = 1.8, 'Size Spectra Slope')
mtext(side = 1, line = 1.8, 'Buffalo Harvest (cumul.)') 

# Slope (not cumulative) # 
slps
fish_harv

slop_harv_summer2 = left_join(slps, fish_harv, by = c('lake', 'year'))
slop_harv_summer2

windows(height = 3, width = 6) 
par(mfrow =c(1,2), mar = c(0.5,1,2,2), oma = c(4,4,.5,4))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
carp = rgb(0, 0, 0, alpha = 180, max = 255)
buffalo = rgb(204,204,204, alpha = 180, max = 255)
both = rgb(0,0,0, alpha = 180, max = 225)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

max(slop_harv_summer2$slope.value)
min(slop_harv_summer2$slope.value)

max(slop_harv_summer2$carp_harv)
min(slop_harv_summer2$carp_harv)

max(slop_harv_summer2$buff_harv)
min(slop_harv_summer2$buff_harv)

# Slope 
plot(slope.value ~ carp_harv, data = slop_harv_summer2, cex = 1.5, pch = 19, ylim = c(-0.35, -0.76), xlim = c(-5, 90),
     ylab = 'Size Spectra Slope', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(slope.value ~ carp_harv, data = slop_harv_summer2)
summary(mod1)
abline(mod1, col = carp)
text(70, -0.75, 'p = 0.606',  cex = 1, col = 'black', font = 1)
mtext(side = 2, line = 1.8, 'Size Spectra Slope')
mtext(side = 1, line = 1.8, 'Carp Harvest (kg/ha)') 

plot(slope.value ~ buff_harv, data = slop_harv_summer2, cex = 1.5, pch = 19, ylim = c(-0.35, -0.76), xlim = c(-5, 400),
     ylab = '', xlab = '', col = buffalo)
mod2 = lm(slope.value ~ buff_harv, data = slop_harv_summer2)
summary(mod2)
abline(mod2, col = buffalo)
text(300, -0.75, 'p = 0.189',  cex = 1, col = buffalo, font = 2)
mtext(side = 2, line = 1.8, 'Size Spectra Slope')
mtext(side = 1, line = 1.8, 'Buffalo Harvest (kg/ha)') 


# Height 
fits_summer = fits_dat %>%
  select(year, lake, height) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = height)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'hgt_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 7), rep('2020', 14))
chng_summer = chng_summer %>% mutate(year = as.double(year))

height_harv_summer = left_join(chng_summer, cumulative, by = c('year', 'lake'))
height_harv_summer

max(height_harv_summer$hgt_change)
min(height_harv_summer$hgt_change)

plot(hgt_change ~ sum_harv, data = height_harv_summer, cex = 1.5, pch = 19, col = buffalo,
     ylab = 'Change in height', xlab = 'Fish Harvested (kg/ha)', ylim = c(-2.5, 2))
mod2 = lm(hgt_change ~ sum_harv, data = height_harv_summer)
summary(mod2)
abline(mod2, col = buffalo, lty = 2)
text(575, 1.8, 'p = 0.288',  cex = 1, col = 'gray40')
mtext(side = 1, line = 1.8, 'Cumulative Harvest (kg/ha)', cex = 1)
mtext(side =2, line =1.8, 'Change in height ')

# Change in slope - fish removed (no cumulative) #==============================

# Change in Slope v. kg/ha removed #========================== 
library(tidyverse)
library(lubridate)
fish_harv = read_csv('fish_harvest.csv')
fish_harv

# Create Change in slope column # 
fits_dat = read_csv('fitsdata_zp-miv_long_springsummer.csv')
fits_dat

fits_summer = fits_dat %>%
  select(year, lake, slope) %>%
  pivot_wider(id_cols = c(lake), names_from = year, values_from = slope)
fits_summer

chng_summer = fits_summer %>% 
  mutate(delta.1918 = `2019` - `2018`) %>%
  mutate(delta.2019 = `2020` - `2019`) %>% 
  mutate(delta.2018 = `2020` - `2018`) %>%
  select(lake, delta.1918, delta.2019, delta.2018) %>%
  pivot_longer(cols = c(delta.1918, delta.2019, delta.2018),names_to = 'delta', values_to = 'slp_change') %>% 
  arrange(delta)
chng_summer

chng_summer$year = c(rep('2019', 7), rep('2020', 14))
chng_summer = chng_summer %>% mutate(year = as.double(year))
chng_summer 
fish_harv

slop_harv_summer = left_join(chng_summer, fish_harv, by = c('year', 'lake'))
slop_harv_summer





## Combined spring - summer plots ##========================
# Join harvest data to slope and height data # 
windows(height = 4, width = 5) 
par(mfrow =c(2,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Output graphs with linear fits # 
# Reference - no removal # Blue, South Twin, Storm
carp = rgb(0, 0, 0, alpha = 180, max = 255)
buffalo = rgb(204,204,204, alpha = 180, max = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

max(slop_harv_summer$slp_change)
min(slop_harv_summer$slp_change)

# Carp 
plot(slp_change ~ carp_harv, data = slop_harv_summer, cex = 1.5, pch = 19, ylim = c(-0.3, 0.2),
     ylab = '|Change in slope|', xlab = 'Fish Harvested (kg/ha)', col = carp)
mod1 = lm(slp_change ~ carp_harv, data = slop_harv_summer)
axis(side = 2, at = c(-0.2, -0.1, 0, 0.1, 0.2), labels = T, tick = F)
summary(mod1)
abline(mod1, col = carp)
text(60, 0.18, 'p = 0.876',  cex = 1, col = 'black', font = 1)
mtext(side = 2, line = 1.8, 'Change in slope')
mtext(side = 3, line = 0.1, 'Common Carp Harvest')
mtext(side =1, line = 1.8, 'Carp removed year prior (kg/ha)')

plot(slp_change ~ buff_harv, data = slop_harv_summer, cex = 1.5, pch = 19, col = buffalo, ylim = c(-0.3, 0.2),
     ylab = '|Change in slope|', xlab = 'Fish Harvested (kg/ha)')
mod2 = lm(slp_change ~ buff_harv, data = slop_harv_summer)
summary(mod2)
abline(mod2, col = 'gray80', lty = 2)
text(250, 0.18, 'p = 0.914',  cex = 1, col = 'gray40', font = 1)
mtext(side = 3, line = 0.1, 'Bigmouth Buffalo Harvest')
mtext(side = 1, line = 1.8, 'Buff removed year prior')


