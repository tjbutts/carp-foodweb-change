## Step 7_Supplemental Information ##==============================

# Load in library # 
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

## Fish Biomass Estimates ## ===========================

library(tidyverse)
library(lubridate)

# Data # 
fish_schnab = get_data("edi.1926.1", filenum = 6)
fish_schnab

# Trim data to year-to-year biomass estimates # 

fish = fish_schnab %>% 
  select(lake, year, fish, biomass_kg.ha, lci_kg.ha, uci_kg.ha)
fish

carp = fish %>% 
  filter(fish == 'carp')
buff = fish %>%
  filter(fish == 'buffalo') 

windows(height = 5, width = 8) 


# ========= PLOTTING COLORS ===== # 
# Output graphs with linear fits # 
# Reference - no removal  South Twin, Storm
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


# Height v. Chlorophyll Value #============================================
# Pull data from EDI # 
# Package ID: edi.1251.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Summer water chemistry; sediment phosphorus fluxes and sorption capacity; sedimentation and sediment resuspension dynamics; water column thermal structure; and zooplankton, macroinvertebrate, and macrophyte communities in eight shallow lakes in northwest Iowa, USA (2018-2020).
# Data set creator: Dr. Ellen Albright - University of Wisconsin-Madison 
# Data set creator: Dr. Grace Wilkinson - University of Wisconsin-Madison 
# Data set creator:  Tyler Butts - University of Wisconsin-Madison 
# Data set creator:  Quin Shingai - Dartmouth College 
# Metadata Provider: Dr. Ellen Albright - University of Wisconsin-Madison 
# Contact: Dr. Grace Wilkinson -  University of Wisconsin-Madison  - gwilkinson@wisc.edu
# Contact: Dr. Michelle Balmer -  Iowa Department of Natural Resources  - michelle.balmer@dnr.iowa.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1251/3/22a1c209f19f303271ddab8aaa70402d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year",     
                 "sample_id",     
                 "zoop_id",     
                 "program",     
                 "lake_name",     
                 "lake_id",     
                 "depth",     
                 "doy",     
                 "year_frac",     
                 "secchi_m",     
                 "secchi_flag",     
                 "tss_mgL",     
                 "tss_flag",     
                 "vss_mgL",     
                 "vss_flag",     
                 "iss_mgL",     
                 "iss_flag",     
                 "tp_ugL",     
                 "tp_flag",     
                 "srp_ugL",     
                 "srp_flag",     
                 "tn_mgL",     
                 "tn_flag",     
                 "nox_mgL",     
                 "nox_flag",     
                 "chl_ugL",     
                 "chl_flag",     
                 "phyco_ugL",     
                 "phyco_flag"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$sample_id)!="factor") dt1$sample_id<- as.factor(dt1$sample_id)
if (class(dt1$zoop_id)!="factor") dt1$zoop_id<- as.factor(dt1$zoop_id)
if (class(dt1$program)!="factor") dt1$program<- as.factor(dt1$program)
if (class(dt1$lake_name)!="factor") dt1$lake_name<- as.factor(dt1$lake_name)
if (class(dt1$lake_id)!="factor") dt1$lake_id<- as.factor(dt1$lake_id)
if (class(dt1$depth)!="factor") dt1$depth<- as.factor(dt1$depth)
if (class(dt1$doy)=="factor") dt1$doy <-as.numeric(levels(dt1$doy))[as.integer(dt1$doy) ]               
if (class(dt1$doy)=="character") dt1$doy <-as.numeric(dt1$doy)
if (class(dt1$year_frac)=="factor") dt1$year_frac <-as.numeric(levels(dt1$year_frac))[as.integer(dt1$year_frac) ]               
if (class(dt1$year_frac)=="character") dt1$year_frac <-as.numeric(dt1$year_frac)
if (class(dt1$secchi_m)=="factor") dt1$secchi_m <-as.numeric(levels(dt1$secchi_m))[as.integer(dt1$secchi_m) ]               
if (class(dt1$secchi_m)=="character") dt1$secchi_m <-as.numeric(dt1$secchi_m)
if (class(dt1$secchi_flag)!="factor") dt1$secchi_flag<- as.factor(dt1$secchi_flag)
if (class(dt1$tss_mgL)=="factor") dt1$tss_mgL <-as.numeric(levels(dt1$tss_mgL))[as.integer(dt1$tss_mgL) ]               
if (class(dt1$tss_mgL)=="character") dt1$tss_mgL <-as.numeric(dt1$tss_mgL)
if (class(dt1$tss_flag)!="factor") dt1$tss_flag<- as.factor(dt1$tss_flag)
if (class(dt1$vss_mgL)=="factor") dt1$vss_mgL <-as.numeric(levels(dt1$vss_mgL))[as.integer(dt1$vss_mgL) ]               
if (class(dt1$vss_mgL)=="character") dt1$vss_mgL <-as.numeric(dt1$vss_mgL)
if (class(dt1$vss_flag)!="factor") dt1$vss_flag<- as.factor(dt1$vss_flag)
if (class(dt1$iss_mgL)=="factor") dt1$iss_mgL <-as.numeric(levels(dt1$iss_mgL))[as.integer(dt1$iss_mgL) ]               
if (class(dt1$iss_mgL)=="character") dt1$iss_mgL <-as.numeric(dt1$iss_mgL)
if (class(dt1$iss_flag)!="factor") dt1$iss_flag<- as.factor(dt1$iss_flag)
if (class(dt1$tp_ugL)=="factor") dt1$tp_ugL <-as.numeric(levels(dt1$tp_ugL))[as.integer(dt1$tp_ugL) ]               
if (class(dt1$tp_ugL)=="character") dt1$tp_ugL <-as.numeric(dt1$tp_ugL)
if (class(dt1$tp_flag)!="factor") dt1$tp_flag<- as.factor(dt1$tp_flag)
if (class(dt1$srp_ugL)=="factor") dt1$srp_ugL <-as.numeric(levels(dt1$srp_ugL))[as.integer(dt1$srp_ugL) ]               
if (class(dt1$srp_ugL)=="character") dt1$srp_ugL <-as.numeric(dt1$srp_ugL)
if (class(dt1$srp_flag)!="factor") dt1$srp_flag<- as.factor(dt1$srp_flag)
if (class(dt1$tn_mgL)=="factor") dt1$tn_mgL <-as.numeric(levels(dt1$tn_mgL))[as.integer(dt1$tn_mgL) ]               
if (class(dt1$tn_mgL)=="character") dt1$tn_mgL <-as.numeric(dt1$tn_mgL)
if (class(dt1$tn_flag)!="factor") dt1$tn_flag<- as.factor(dt1$tn_flag)
if (class(dt1$nox_mgL)=="factor") dt1$nox_mgL <-as.numeric(levels(dt1$nox_mgL))[as.integer(dt1$nox_mgL) ]               
if (class(dt1$nox_mgL)=="character") dt1$nox_mgL <-as.numeric(dt1$nox_mgL)
if (class(dt1$nox_flag)!="factor") dt1$nox_flag<- as.factor(dt1$nox_flag)
if (class(dt1$chl_ugL)=="factor") dt1$chl_ugL <-as.numeric(levels(dt1$chl_ugL))[as.integer(dt1$chl_ugL) ]               
if (class(dt1$chl_ugL)=="character") dt1$chl_ugL <-as.numeric(dt1$chl_ugL)
if (class(dt1$chl_flag)!="factor") dt1$chl_flag<- as.factor(dt1$chl_flag)
if (class(dt1$phyco_ugL)=="factor") dt1$phyco_ugL <-as.numeric(levels(dt1$phyco_ugL))[as.integer(dt1$phyco_ugL) ]               
if (class(dt1$phyco_ugL)=="character") dt1$phyco_ugL <-as.numeric(dt1$phyco_ugL)
if (class(dt1$phyco_flag)!="factor") dt1$phyco_flag<- as.factor(dt1$phyco_flag)

# Convert Missing Values to NA for non-dates

dt1$secchi_m <- ifelse((trimws(as.character(dt1$secchi_m))==trimws("NA")),NA,dt1$secchi_m)               
suppressWarnings(dt1$secchi_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$secchi_m))==as.character(as.numeric("NA"))),NA,dt1$secchi_m))
dt1$tss_mgL <- ifelse((trimws(as.character(dt1$tss_mgL))==trimws("NA")),NA,dt1$tss_mgL)               
suppressWarnings(dt1$tss_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tss_mgL))==as.character(as.numeric("NA"))),NA,dt1$tss_mgL))
dt1$vss_mgL <- ifelse((trimws(as.character(dt1$vss_mgL))==trimws("NA")),NA,dt1$vss_mgL)               
suppressWarnings(dt1$vss_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$vss_mgL))==as.character(as.numeric("NA"))),NA,dt1$vss_mgL))
dt1$iss_mgL <- ifelse((trimws(as.character(dt1$iss_mgL))==trimws("NA")),NA,dt1$iss_mgL)               
suppressWarnings(dt1$iss_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$iss_mgL))==as.character(as.numeric("NA"))),NA,dt1$iss_mgL))
dt1$tp_ugL <- ifelse((trimws(as.character(dt1$tp_ugL))==trimws("NA")),NA,dt1$tp_ugL)               
suppressWarnings(dt1$tp_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tp_ugL))==as.character(as.numeric("NA"))),NA,dt1$tp_ugL))
dt1$tn_mgL <- ifelse((trimws(as.character(dt1$tn_mgL))==trimws("NA")),NA,dt1$tn_mgL)               
suppressWarnings(dt1$tn_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tn_mgL))==as.character(as.numeric("NA"))),NA,dt1$tn_mgL))
dt1$nox_mgL <- ifelse((trimws(as.character(dt1$nox_mgL))==trimws("NA")),NA,dt1$nox_mgL)               
suppressWarnings(dt1$nox_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$nox_mgL))==as.character(as.numeric("NA"))),NA,dt1$nox_mgL))
dt1$chl_ugL <- ifelse((trimws(as.character(dt1$chl_ugL))==trimws("NA")),NA,dt1$chl_ugL)               
suppressWarnings(dt1$chl_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$chl_ugL))==as.character(as.numeric("NA"))),NA,dt1$chl_ugL))
dt1$phyco_ugL <- ifelse((trimws(as.character(dt1$phyco_ugL))==trimws("NA")),NA,dt1$phyco_ugL)               
suppressWarnings(dt1$phyco_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$phyco_ugL))==as.character(as.numeric("NA"))),NA,dt1$phyco_ugL))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year)
summary(sample_id)
summary(zoop_id)
summary(program)
summary(lake_name)
summary(lake_id)
summary(depth)
summary(doy)
summary(year_frac)
summary(secchi_m)
summary(secchi_flag)
summary(tss_mgL)
summary(tss_flag)
summary(vss_mgL)
summary(vss_flag)
summary(iss_mgL)
summary(iss_flag)
summary(tp_ugL)
summary(tp_flag)
summary(srp_ugL)
summary(srp_flag)
summary(tn_mgL)
summary(tn_flag)
summary(nox_mgL)
summary(nox_flag)
summary(chl_ugL)
summary(chl_flag)
summary(phyco_ugL)
summary(phyco_flag) 
detach(dt1)    

carp.wq = as_tibble(dt1) %>% 
  select(year, sample_id, program, lake_name, lake_id, depth, doy, secchi_m, secchi_flag, chl_ugL, chl_flag, phyco_ugL, phyco_flag)
carp.wq

# Chlorophyll #======================
chla = carp.wq %>% 
  select(lake_name, year, doy, chl_ugL, chl_flag) %>% 
  mutate(lake = as.character(lake_name)) %>%
  filter(chl_flag != 'm' & chl_flag != 'a') # Remove data points where chlorophyll wasn't collected
chla

# Window for checking plot 
windows(height = 6.5, width = 4.5) 

max(chla$chl_ugL) # 160
min(chla$chl_ugL) # 0.4 

# ========= PLOTTING COLORS ===== # 
# Output graphs with linear fits # 
# Reference - no removal #  South Twin, Storm
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

# Set dimensions for figure array # 
par(mfrow =c(3,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,2))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

## Reference HEIGHT ##==========================

fits_dat 

# STORM #
storm.su_dat = fits_dat %>%
  filter(lake == 'Storm')
storm.su_dat$year = c(2017.9, 2018.9, 2019.9)
storm.su_dat

plot(height~year, data = storm.su_dat, pch = 15, ylim = c(10, 16), xlim = c(2017.5, 2020.5), 
     cex = 2, col = ref_col_20, xaxt = 'n', ylab = '')
arrows(x0=storm.su_dat$year, y0=storm.su_dat$height.l95ci,col = ref_col_20,
 x1=storm.su_dat$year, y1=storm.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext(side = 3, 'Size Spectra Height', line = 0)
mtext('Height', side = 2, line = 1.8)
mtext('Reference', side = 2, line = 3)
#mtext(side =3, line = 0, 'Storm')
text(2017.55, 15.9, 'A', cex = 1.2)

# SOUTH TWIN # 
st.su_dat = fits_dat %>%
  filter(lake == 'South Twin') 
st.su_dat$year = c(2018.1, 2019.1, 2020.1)

points(height~year,data = st.su_dat, pch = 15, ylim = c(10, 16),
       xlim = c(2017.5, 2020.5), cex = 2, col = ref_col_18, xaxt='n', col.axis = transparent)
arrows(x0=st.su_dat$year, y0=st.su_dat$height.l95ci, col = ref_col_18, 
 x1=st.su_dat$year, y1=st.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)

## REFERENCE CHLOROPHYLL ##==================================

# STORM & SOUTH TWIN # 
ref = chla %>%
  filter(lake == 'Storm' | lake == 'South Twin') %>% 
  mutate(year = as.factor(year), 
         lake = as.factor(lake)) %>% 
  select(lake, year, chl_ugL) %>%
  as.data.frame()
ref
ref$lake = factor(ref$lake, levels = c('Storm', 'South Twin'))

boxplot(log(chl_ugL) ~ lake + year, data = ref,
        col = c(ref_col_20, ref_col_18),          # Custom fill colors
        at = c(1,2,3,4,5,6), 
        border = "black",                       # Border color for the boxes
        ylim = c(log(0.1), log(175)), 
        yaxt = 'n', xaxt = 'n',
        ylab = '', xlab = '') 
stripchart(log(chl_ugL) ~ lake + year, data = ref, vertical = T,
           yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = .8, 
           ylim = c(log(0.1), log(175)))
axis(side = 1, at = c(1.5, 3.5, 5.5), labels = F)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '',
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','200'),
     las=0)

mtext(expression(Chlorophyll*`-`*italic(a)), side = 2, line = .2, cex = 0.9)
mtext(expression(Chl-a~`(`*mu*g~L^-1*`)`), side =3, line = -0.2, cex = 0.9)
text(0.6, log(160), 'B', cex = 1.2)
# Legend - Storm & South Twin # 
legend('bottomright', legend = c('Storm', 'South Twin'), col = c(ref_col_20, ref_col_18), pch = 15, bty = 'n', cex = 1.2)

## RRN HEIGHT ##===========================
# CENTER # 
center.su_dat = fits_dat %>%
  filter(lake == 'Center') 
center.su_dat$year = c(2017.9, 2018.9, 2019.9)

plot(height~year, data = center.su_dat, pch = 15, ylim = c(10, 16), 
     xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_20, xaxt = 'n', ylab = '')
arrows(x0=center.su_dat$year, y0=center.su_dat$height.l95ci, col = rrn_col_20,
 x1=center.su_dat$year, y1=center.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
abline(v = 2017.65)
abline(v = 2018.5)

text(2017.53, 15.9, 'C', cex = 1.2)

# FIVE ISLAND # 
five.island.su_dat = fits_dat %>%
  filter(lake == 'Five Island') 
five.island.su_dat$year =  c(2018.1, 2019.1, 2020.1)

points(height~year, data = five.island.su_dat, pch = 15, ylim = c(10, 16),
       xlim = c(2017.5, 2020.5), cex = 2, col = rrn_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=five.island.su_dat$year, y0=five.island.su_dat$height.l95ci, col = rrn_col_18,
 x1=five.island.su_dat$year, y1=five.island.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = F)
mtext('Height', side = 2, line = 1.8)
mtext('Removal 2018, 2019', side = 2, line = 3)

# 
abline(v = 2017.65)
abline(v = 2018.5)

## RRN CHLOROPHYLL ## ==============================
# CENTER & FIVE ISLAND # 
rrn = chla %>%
  filter(lake == 'Center' | lake == 'Five Island') %>% 
  mutate(year = as.factor(year), 
         lake = as.factor(lake)) %>% 
  select(lake, year, chl_ugL) %>%
  as.data.frame()
rrn
rrn$lake = factor(rrn$lake, levels = c('Center', 'Five Island'))
rrn

boxplot(log(chl_ugL) ~ lake + year, data = rrn,
        col = c(rrn_col_20, rrn_col_18),          # Custom fill colors
        at = c(1,2,3,4,5,6), 
        border = "black",                       # Border color for the boxes
        ylim = c(log(0.1), log(175)), 
        yaxt = 'n', xaxt = 'n',
        ylab = '', xlab = '') 
stripchart(log(chl_ugL) ~ lake + year, data = rrn, vertical = T,
           yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 0.8, 
           ylim = c(log(0.1), log(175)))
axis(side = 1, at = c(1.5, 3.5, 5.5), labels = F)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '',
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','200'),
     las=0)

mtext(expression(Chlorophyll*`-`*italic(a)), side = 2, line = .2, cex = 0.9)

text(0.6, log(160), 'D', cex = 1.2)
legend('bottomright', legend = c('Center', 'Five Island'), col = c(rrn_col_20, rrn_col_18), pch = 15, bty = 'n', cex = 1.2)

## NRR HEIGHT ##=====================================
north.twin.su_dat = fits_dat %>%
  filter(lake == 'North Twin') 
north.twin.su_dat$year = c(2017.9, 2018.9, 2019.9)

plot(height~year, data = north.twin.su_dat, pch = 15, ylim = c(10, 16), 
     xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_20, xaxt = 'n', ylab = '')
arrows(x0=north.twin.su_dat$year, y0=north.twin.su_dat$height.l95ci, col = nrr_col_20,
 x1=north.twin.su_dat$year, y1=north.twin.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)

mtext(side =2, 'Height', line = 1.8)
mtext(side = 2, 'Removal 2019,2020', line = 3)
mtext(side = 1, 'Year', line = 2)
text(2017.53, 15.9, 'E', cex = 1.2)

abline(v = 2018.5)
abline(v = 2019.5)

# SILVER # 
silver.su_dat = fits_dat %>%
  filter(lake == 'Silver') 
silver.su_dat$year = c(2018.1, 2019.1, 2020.1)

points(height~year, data = silver.su_dat, pch = 15, ylim = c(10, 16), xlim = c(2017.5, 2020.5), cex = 2, col = nrr_col_18, xaxt = 'n', col.axis = transparent)
arrows(x0=silver.su_dat$year, y0=silver.su_dat$height.l95ci, col = nrr_col_18,
 x1=silver.su_dat$year, y1=silver.su_dat$height.u95ci, code = 3, angle=90, length=0, lwd = 2)
axis(side = 1, at = c(2018, 2019, 2020), labels = T)


## NRR CHLOROPHYLL ##===============================
nrr = chla %>%
  filter(lake == 'North Twin' | lake == 'Silver') %>% 
  mutate(year = as.factor(year), 
         lake = as.factor(lake)) %>% 
  select(lake, year, chl_ugL) %>%
  as.data.frame()
nrr
nrr$lake = factor(nrr$lake, levels = c('North Twin', 'Silver'))
rrn

# NORTH TWIN & SILVER #
boxplot(log(chl_ugL) ~ lake + year, data = nrr,
        col = c(nrr_col_20, nrr_col_18),          # Custom fill colors
        at = c(1,2,3,4,5,6), 
        border = "black",                       # Border color for the boxes
        ylim = c(log(0.1), log(175)), 
        yaxt = 'n', xaxt = 'n',
        ylab = '', xlab = '') 
stripchart(log(chl_ugL) ~ lake + year, data = nrr, vertical = T,
           yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 0.8, 
           ylim = c(log(0.1), log(175)))
axis(side = 1, at = c(1.5, 3.5, 5.5), labels = c(2018, 2019, 2020))
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '',
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','200'),
     las=0)

mtext(expression(Chlorophyll*`-`*italic(a)), side = 2, line = .2, cex = 0.9)

text(0.6, log(160), 'F', cex = 1.2)
mtext(side = 1, 'Year', line = 2)

legend('bottomleft', legend = c('North Twin', 'Silver'), col = c(nrr_col_20, nrr_col_18), pch = 15, cex = 1.2, bty = 'n')

