##Step 1 _ Fish Harvest & Exploitation ## ===========================

library(tidyverse)
library(lubridate)

# Data # 
harv_abund = read_csv('fishharv_exploitation.csv') %>% 
  rename(lake = lake...1)
harv_abund

# Trim data to year-to-year biomass estimates # 

fish = harv_abund %>% 
  select(lake, year, fish, biomass_kg.ha, lci_kg.ha, uci_kg.ha, harvest_kgha)
fish

carp = fish %>% 
  filter(fish == 'carp')
buff = fish %>%
  filter(fish == 'buffalo') 

windows(height = 5, width = 8) 


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

# Set dimensions for figure array # 
par(mfrow =c(2,4), mar = c(0.5,1,1,0.5), oma = c(8,4,.5,1.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Carp year-to-year #===============================
# Harvest 18 - 19 #
max(carp$biomass_kg.ha)
min(carp$biomass_kg.ha)

center = carp %>% 
  filter(lake == 'Center') 
center  


plot(biomass_kg.ha~year, data = center, pch = 19, ylim = c(0, 300), 
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n', type = 'o', lwd = 2)
arrows(x0=center$year, y0=center$lci_kg.ha,
       x1=center$year, y1=center$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = rrn_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext('Common Carp', side = 2, line = 3)
mtext('Biomass (kg/ha)', side = 2, line = 1.8)
mtext(side =3, 'Center', line = 0 )
abline(v = 2017.65)
abline(v = 2018.5)

fiveisland = carp %>% 
  filter(lake == 'Five.Island')
plot(biomass_kg.ha~year, data = fiveisland, pch = 19, ylim = c(0,300), 
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', type = 'o', lwd = 2)
arrows(x0=fiveisland$year, y0=fiveisland$lci_kg.ha,
       x1=fiveisland$year, y1=fiveisland$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = rrn_col_18)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side =3, 'Five Island', line = 0 )
abline(v = 2017.65)
abline(v = 2018.5)

# Removal 19 - 20 # 
northtwin = carp %>%
  filter(lake == 'North.Twin')
northtwin
plot(biomass_kg.ha~year, data = northtwin, pch = 19, ylim = c(0,300), 
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n', type = 'o', lwd = 2)
arrows(x0=northtwin$year, y0=northtwin$lci_kg.ha,
       x1=northtwin$year, y1=northtwin$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = nrr_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(v = 2018.5)
abline(v = 2019.5)
mtext(side = 3, 'North Twin', line = 0)



silver = carp %>% 
  filter(lake == 'Silver')
silver

plot(biomass_kg.ha~year, data = silver, pch = 19, ylim = c(0,300), 
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt = 'n', type = 'o', lwd = 2)
arrows(x0=silver$year, y0=silver$lci_kg.ha,
       x1=silver$year, y1=silver$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = nrr_col_18)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(v = 2018.5)
abline(v = 2019.5)
mtext(side = 3, 'Silver', line = 0)


# Buffalo #============================


# Buffalo year-to-year #===============================
# Harvest 18 - 19 #
max(buff$biomass_kg.ha)
min(buff$biomass_kg.ha)

center = buff %>% 
  filter(lake == 'Center') 
center  


plot(biomass_kg.ha~year, data = center, pch = 19, ylim = c(0, 250), 
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n', type = 'o', lwd = 2)
arrows(x0=center$year, y0=center$lci_kg.ha,
       x1=center$year, y1=center$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = rrn_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
mtext('Bigmouth Buffalo', side = 2, line = 3)
mtext('Biomass (kg/ha)', side = 2, line = 1.8)
# mtext(side =3, 'Center', line = 0 )
abline(v = 2017.65)
abline(v = 2018.5)
mtext(side = 1, 'Year', line =2)

fiveisland = buff %>% 
  filter(lake == 'Five.Island')
plot(biomass_kg.ha~year, data = fiveisland, pch = 19, ylim = c(0,250), 
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', type = 'o', lwd = 2)
arrows(x0=fiveisland$year, y0=fiveisland$lci_kg.ha,
       x1=fiveisland$year, y1=fiveisland$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = rrn_col_18)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
# mtext(side =3, 'Five Island', line = 0 )
abline(v = 2017.65)
abline(v = 2018.5)
mtext(side = 1, 'Year', line =2)

# Removal 19 - 20 # 
northtwin = buff %>%
  filter(lake == 'North.Twin')
northtwin
plot(biomass_kg.ha~year, data = northtwin, pch = 19, ylim = c(0,1000), 
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20,
     xaxt = 'n', type = 'o', lwd = 2)
axis(2, col.axis = 'maroon')
arrows(x0=northtwin$year, y0=northtwin$lci_kg.ha,
       x1=northtwin$year, y1=northtwin$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = nrr_col_20)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
abline(v = 2018.5)
abline(v = 2019.5)
# mtext(side = 3, 'North Twin', line = 0)
mtext(side =1, 'Year', line = 2)

silver = buff %>% 
  filter(lake == 'Silver')
silver

plot(biomass_kg.ha~year, data = silver, pch = 19, ylim = c(0,250), 
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt = 'n', type = 'o', lwd = 2)
arrows(x0=silver$year, y0=silver$lci_kg.ha,
       x1=silver$year, y1=silver$uci_kg.ha, code = 3, angle=90, 
       length=0, lwd = 2, col = nrr_col_18)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)
abline(v = 2018.5)
abline(v = 2019.5)
# mtext(side = 3, 'Silver', line = 0)
mtext(side = 1, 'Year', line = 2)
