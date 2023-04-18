# Carp boxplots # 

# Load in library # 
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

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
windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

max(chla$chl_ugL) # 160
min(chla$chl_ugL) # 0.4 

# Plot # 
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

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


# BLUE # 
blue_dat = chla %>% 
  filter(lake == 'Blue') 
blue_dat

boxplot(log(chl_ugL)~year, data = blue_dat, col = ref_col_20, ylim = c(log(0.1), log(175)), yaxt = 'n', xaxt = 'n')
stripchart(log(chl_ugL) ~ year, vertical = TRUE, data = blue_dat, ylim = c(log(0.1), log(175)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', 
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','200'),
     las=0)
axis(side=1, 
     at=c(1,2,3), 
     labels = F)
mtext(expression(Chlorophyll~`-`~italic(a)), side = 2, line = 3)
mtext(expression(Concentration~`(`~mu~g~L^-1~`)`), side =2, line = 1.8)
mtext('Blue', side = 3, line = 0)

# STORM # 
storm.su_dat = chla %>%
  filter(lake == 'Storm') 
storm.su_dat

boxplot(log(chl_ugL)~year, data = storm.su_dat, col = ref_col_19, ylim = c(log(0.1), log(175)), yaxt = 'n', xaxt = 'n')
stripchart(log(chl_ugL) ~ year, vertical = TRUE, data = storm.su_dat, ylim = c(log(0.1), log(175)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200)), #Where the tick marks should be drawn
     labels = F,
     las=0)
axis(side = 1, 
     at = c(1,2,3), 
     labels = F)
mtext('Storm', side = 3, line = 0)

# SOUTH TWIN # 
south.twin_dat = chla %>%
  filter(lake == 'South Twin') 
south.twin_dat

boxplot(log(chl_ugL)~year, data = south.twin_dat, col = ref_col_18, ylim = c(log(0.1), log(175)), yaxt = 'n')
stripchart(log(chl_ugL) ~ year, vertical = TRUE, data = south.twin_dat, ylim = c(log(0.1), log(175)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200)), #Where the tick marks should be drawn
     labels = F,
     las=0)
mtext('South Twin', side = 3, line = 0)

# CENTER # 
center.su_dat = chla %>%
  filter(lake == 'Center') 
center.su_dat

boxplot(log(chl_ugL)~year, data = center.su_dat, col = rrn_col_20, ylim = c(log(0.1), log(175)), yaxt = 'n', xaxt = 'n')
stripchart(log(chl_ugL) ~ year, vertical = TRUE, data = center.su_dat, ylim = c(log(0.1), log(175)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', 
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','200'),
     las=0)
axis(side=1, 
     at=c(1,2,3), 
     labels = F)
mtext(expression(Chlorophyll~`-`~italic(a)), side = 2, line = 3)
mtext(expression(Concentration~`(`~mu~g~L^-1~`)`), side =2, line = 1.8)
mtext('Center', side = 3, line = 0)
abline(v = 0.5)
abline(v = 1.5)

# FIVE ISLAND # 
fivisl_dat = chla %>%
  filter(lake == 'Five Island') 
fivisl_dat

boxplot(log(chl_ugL)~year, data = fivisl_dat, col = rrn_col_18, ylim = c(log(0.1), log(175)), yaxt = 'n', xaxt = 'n')
stripchart(log(chl_ugL) ~ year, vertical = TRUE, data = fivisl_dat, ylim = c(log(0.1), log(175)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200)), #Where the tick marks should be drawn
     labels = F,
     las=0)
axis(side = 1, 
     at = c(1,2,3), 
     labels = F)
mtext('Five Island', side = 3, line = 0)
abline(v = 0.5)
abline(v = 1.5)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
# legend("center", legend =c('summer', 'spring'), pch=c(20,22), pt.cex=3, cex=1.5, bty='n',
#        col = 'gray60')

# NORTH TWIN # 
ntwin_dat = chla %>%
  filter(lake == 'North Twin') 
ntwin_dat

boxplot(log(chl_ugL)~year, data = ntwin_dat, col = nrr_col_20, ylim = c(log(0.1), log(175)), yaxt = 'n', xaxt = 'n')
stripchart(log(chl_ugL) ~ year, vertical = TRUE, data = ntwin_dat, ylim = c(log(0.1), log(175)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', 
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','200'),
     las=0)
axis(side=1, 
     at=c(1,2,3), 
     labels = c(2018, 2019, 2020))
mtext(expression(Chlorophyll~`-`~italic(a)), side = 2, line = 3)
mtext(expression(Concentration~`(`~mu~g~L^-1~`)`), side =2, line = 1.8)
mtext('North Twin', side = 3, line = 0)
abline(v = 1.5)
abline(v = 2.5)

# SILVER # 
silver.su_dat = chla %>%
  filter(lake == 'Silver') 
silver.su_dat

boxplot(log(chl_ugL)~year, data = silver.su_dat, col = nrr_col_18, ylim = c(log(0.1), log(175)), yaxt = 'n', xaxt = 'n')
stripchart(log(chl_ugL) ~ year, vertical = TRUE, data = silver.su_dat, ylim = c(log(0.1), log(175)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200)), #Where the tick marks should be drawn
     labels = F,
     las=0)
axis(side=1, 
     at=c(1,2,3), 
     labels = c(2018, 2019, 2020))
mtext('Silver', side = 3, line = 0)
mtext('Year', side = 1, line = 2)
abline(v = 1.5)
abline(v = 2.5)


# Secchi #=======================================
secchi = carp.wq %>% 
  select(year, doy, lake_name, secchi_m, secchi_flag) %>% 
  mutate(lake = as.character(lake_name)) %>% 
  filter(secchi_flag != 'm')
secchi

max(secchi$secchi_m) # 2.8
min(secchi$secchi_m) # 0.1

# Window for checking plot 
windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Plot # 
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

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


# BLUE # 
blue_dat = secchi %>% 
  filter(lake == 'Blue') 
blue_dat

boxplot(secchi_m~year, data = blue_dat, col = ref_col_20, ylim = rev(c(0, 3)), xaxt = 'n')
stripchart(secchi_m ~ year, vertical = TRUE, data = blue_dat, ylim = rev(c(0, 3)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=1, 
     at=c(1,2,3), 
     labels = F)
mtext(expression(Secchi~Depth~`(`~m~`)`), side = 2, line = 2)
mtext('Blue', side = 3, line = 0)

# STORM # 
storm.su_dat = secchi %>%
  filter(lake == 'Storm') 
storm.su_dat

boxplot(secchi_m~year, data = storm.su_dat, col = ref_col_19, ylim = rev(c(0, 3)), yaxt = 'n', xaxt = 'n')
stripchart(secchi_m ~ year, vertical = TRUE, data = storm.su_dat, ylim = rev(c(0, 3)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=rev(c(0, 0.5, 1, 1.5, 2, 2.5, 3)), #Where the tick marks should be drawn
     labels = F,
     las=0)
axis(side = 1, 
     at = c(1,2,3), 
     labels = F)
mtext('Storm', side = 3, line = 0)

# SOUTH TWIN # 
south.twin_dat = secchi %>%
  filter(lake == 'South Twin') 
south.twin_dat

boxplot(secchi_m~year, data = south.twin_dat, col = ref_col_18, ylim = rev(c(0, 3)), yaxt = 'n')
stripchart(secchi_m ~ year, vertical = TRUE, data = south.twin_dat, ylim = rev(c(0, 3)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=rev(c(0, 0.5, 1, 1.5, 2, 2.5, 3)), #Where the tick marks should be drawn
     labels = F,
     las=0)
mtext('South Twin', side = 3, line = 0)

# CENTER # 
center.su_dat = secchi %>%
  filter(lake == 'Center') 
center.su_dat

boxplot(secchi_m~year, data = center.su_dat, col = rrn_col_20, ylim = rev(c(0, 3)), xaxt = 'n')
stripchart(secchi_m ~ year, vertical = TRUE, data = center.su_dat, ylim = rev(c(0, 3)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=1, 
     at=c(1,2,3), 
     labels = F)
mtext(expression(Secchi~Depth~`(`~m~`)`), side = 2, line = 2)
mtext('Center', side = 3, line = 0)
abline(v = 0.5)
abline(v = 1.5)

# FIVE ISLAND # 
fivisl_dat = secchi %>%
  filter(lake == 'Five Island') 
fivisl_dat

boxplot(secchi_m~year, data = fivisl_dat, col = rrn_col_18, ylim = rev(c(0,3)), yaxt = 'n', xaxt = 'n')
stripchart(secchi_m ~ year, vertical = TRUE, data = fivisl_dat, ylim = rev(c(0,3)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=rev(c(0, 0.5, 1, 1.5, 2, 2.5, 3)), #Where the tick marks should be drawn
     labels = F,
     las=0)
axis(side = 1, 
     at = c(1,2,3), 
     labels = F)
mtext('Five Island', side = 3, line = 0)
abline(v = 0.5)
abline(v = 1.5)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
# legend("center", legend =c('summer', 'spring'), pch=c(20,22), pt.cex=3, cex=1.5, bty='n',
#        col = 'gray60')

# NORTH TWIN # 
ntwin_dat = secchi %>%
  filter(lake == 'North Twin') 
ntwin_dat

boxplot(secchi_m~year, data = ntwin_dat, col = nrr_col_20, ylim = rev(c(0,3)))
stripchart(secchi_m ~ year, vertical = TRUE, data = ntwin_dat, ylim = rev(c(0,3)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
mtext(expression(Secchi~Depth~`(`~m~`)`), side = 2, line = 2)
mtext('North Twin', side = 3, line = 0)
abline(v = 1.5)
abline(v = 2.5)

# SILVER # 
silver.su_dat = secchi %>%
  filter(lake == 'Silver') 
silver.su_dat

boxplot(secchi_m~year, data = silver.su_dat, col = nrr_col_18, ylim = rev(c(0,3)), yaxt = 'n', xaxt = 'n')
stripchart(secchi_m ~ year, vertical = TRUE, data = silver.su_dat, ylim = rev(c(0,3)), yaxt = 'n',
           method = "jitter", add = TRUE, pch = 21, bg = 'gray80', col = 'black', cex = 1.5)
axis(side=2,
     at=rev(c(0,0.5,1,1.5,2,2.5,3)), #Where the tick marks should be drawn
     labels = F,
     las=0)
axis(side=1, 
     at=c(1,2,3), 
     labels = c(2018, 2019, 2020))
mtext('Silver', side = 3, line = 0)
mtext('Year', side = 1, line = 2)
abline(v = 1.5)
abline(v = 2.5)


# Zooplankton #============================== 
# Load in data # 
full_zoops = read_csv('carpzoops_final.csv') %>%
  mutate(group = case_when(.$taxon %in% c("Alona",
                                          "Alonella",
                                          "Pleuroxus", 
                                          "Camptocercus", 
                                          "Graptoleberis") ~ "Chydorid",
                           .$taxon %in% c("Bosmina") ~ 'Bosmina',
                           .$taxon %in% c("Chydorus") ~ 'Chydorid',
                           .$taxon %in% c("Daphnia.lumholtzi",
                                          "Diaphanosoma",
                                          "Leptodora",
                                          "Moina",
                                          "Scapholeberis") ~ "Lg.Cladocera",
                           .$taxon %in% c("Ceriodaphnia") ~ 'Daphnia', 
                           .$taxon %in% c("Daphnia") ~ 'Daphnia',
                           .$taxon %in% c('Simocephalus') ~ 'Lg.Cladocera',
                           .$taxon %in% c("Anuraeopsis",
                                          "Ascomorpha",
                                          "Asplanchna",
                                          "Asplanchnopus",
                                          "Brachionus",
                                          "Conochilus",
                                          "Euchlanis",
                                          "Filinia",
                                          "Gastropus",
                                          "Hexarthra",
                                          "Kellicottia",
                                          "Keratella.cochlearis",
                                          "Keratella.quadrata",
                                          "Lecane", 
                                          "Lepadella",
                                          "Macrochaetus",
                                          "Monostyla",
                                          "Mytilina",
                                          "Notholca",
                                          "Platyias",
                                          "Ploesoma",
                                          "Polyarthra",
                                          "Pompholyx",
                                          "Synchaeta",
                                          "Testudinella",
                                          "Trichotria", 
                                          "Trichocerca") ~ "Rotifer",
                           .$taxon %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$taxon %in% c("Calanoida") ~ "Calanoid",
                           .$taxon %in% c("Nauplii") ~ "Nauplii")) %>%
  mutate(group = as.factor(group))
full_zoops

# samples per lake per year 
n.zoop = full_zoops %>% 
  group_by(lake, doy, year) %>% 
  summarize(biomass = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(lake, year) %>% 
  summarize(n = length(biomass)) %>% 
  ungroup()
n.zoop

median(n.zoop$n)
mean(n.zoop$n)
