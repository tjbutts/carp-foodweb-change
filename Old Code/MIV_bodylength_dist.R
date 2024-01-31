## Carp Lakes Length Data Exploration ## 
setwd("J:/Box Sync/Iowa Data/Biology Data/Macroinvertebrates/18-20_Carp Lakes Macroinvertebrates")
library(tidyverse)
library(lubridate)

dat = read_csv('MIV_lengthdat.csv')
dat

bodylength = dat %>% 
  filter(measurement == 'BL' | measurement == 'SL')
bodylength

clean_dat = dat %>%
  pivot_longer(cols = c(x1:x43), 
               names_to = 'column', 
               values_to = 'bodylength') %>%
  drop_na('bodylength') %>% 
  select(!c(column, count))
clean_dat

# break out sampleid to useful information # 
clean_dat$year = substr(clean_dat$sampleid, 2,3)
clean_dat

clean_dat$lakeid = substr(clean_dat$sampleid, 4,6)
clean_dat

clean_dat$doy = substr(clean_dat$sampleid, 7,9)
clean_dat = clean_dat %>%
  select(sampleid, year, lakeid, doy, taxa, measurement, bodylength)

# Cleaned data set # 
clean_dat

# Overlapping Histogram per lake # 
set.seed(42)
# Blue lake = 12; Center Lake = 19; 
#Five Island = 36; North Twin Lake = 90; 
# Silver Lake = 105; Storm = 113; South Twin = 406

# Blue Lake # 
blue18 = clean_dat %>% filter(lakeid == '012' & year == '18')
blue19 = clean_dat %>% filter(lakeid == '012' & year == '19')
blue20 = clean_dat %>% filter(lakeid == '012' & year == '20')

bl_p1 = hist(blue18$bodylength, breaks = 15, main = '', xlab = 'Length (mm)')
bl_p2 = hist(blue19$bodylength, breaks = 15, main = '', xlab = 'Length (mm)')
bl_p3 = hist(blue20$bodylength, breaks = 15)

col1 = rgb(74, 166, 81, max = 255, alpha = 128) 
col2 = rgb(44, 127, 184, max = 255, alpha = 128)
col3 = rgb(8, 29, 88, max = 255, alpha = 128)

# Storm - 113 # 
storm18 = clean_dat %>% filter(lakeid == '113' & year == '18')
storm19 = clean_dat %>% filter(lakeid == '113' & year == '19')
storm20 = clean_dat %>% filter(lakeid == '113' & year == '20')

sto_p1 = hist(storm18$bodylength, breaks = 15, main = '', xlab = 'Length (mm)')
sto_p2 = hist(storm19$bodylength, breaks = 15)
sto_p3 = hist(storm20$bodylength, breaks = 15)

col1 = rgb(74, 166, 81, max = 255, alpha = 128) 
col2 = rgb(44, 127, 184, max = 255, alpha = 128)
col3 = rgb(8, 29, 88, max = 255, alpha = 128)

# South Twin - 406 # 
sotwin18 = clean_dat %>% filter(lakeid == '406' & year == '18')
sotwin19 = clean_dat %>% filter(lakeid == '406' & year == '19')
sotwin20 = clean_dat %>% filter(lakeid == '406' & year == '20')

st_p1 = hist(sotwin18$bodylength, breaks = 15, main = '', xlab = 'Length (mm)')
st_p2 = hist(sotwin19$bodylength, breaks = 15)
st_p3 = hist(sotwin20$bodylength, breaks = 15)

col1 = rgb(74, 166, 81, max = 255, alpha = 128) 
col2 = rgb(44, 127, 184, max = 255, alpha = 128)
col3 = rgb(8, 29, 88, max = 255, alpha = 128)

windows(height=4, width=8) 
par(mfrow=c(1,3))

plot(bl_p1, col=col1, xlim = c(0,40), ylim=c(0,80), xlab = 'Length (mm)', main = 'Blue Lake', cex.lab = 1.5, cex.axis = 1.5)
plot(bl_p2, col=col2, xlim = c(0,40), add=T)
plot(bl_p3, col=col3, xlim = c(0,40), add=T)
#legend('topright', legend = c('18', '19', '20'), col=c(col1, col2, col3), pch=15, cex=2)

plot(sto_p1, col=col1, xlim = c(0,40), ylim=c(0,80), xlab = 'Length (mm)', main = 'Storm Lake', cex.lab = 1.5, cex.axis = 1.5)
plot(sto_p2, col=col2, xlim = c(0,40), add=T)
plot(sto_p3, col=col3, xlim = c(0,40), add=T)
#legend('topright', legend = c('18', '19', '20'), col=c(col1, col2, col3), pch=15, cex=2)

plot(st_p1, col=col1, xlim = c(0,40), ylim=c(0,80), xlab = 'Length (mm)', main = 'South Twin', cex.lab = 1.5, cex.axis = 1.5)
plot(st_p2, col=col2, xlim = c(0,40), add=T)
plot(st_p3, col=col3, xlim = c(0,40), add=T)
legend('topright', legend = c('18', '19', '20'), col=c(col1, col2, col3), pch=15, cex=2)

#============================================================================================#

# North Twin Lake # 
nortwin18 = clean_dat %>% filter(lakeid == '090' & year == '18')
nortwin19 = clean_dat %>% filter(lakeid == '090' & year == '19')
nortwin20 = clean_dat %>% filter(lakeid == '090' & year == '20')

nt_p1 = hist(nortwin18$bodylength, breaks = 15)
nt_p2 = hist(nortwin19$bodylength, breaks = 15)
nt_p3 = hist(nortwin20$bodylength, breaks = 15)

col1 = rgb(74, 166, 81, max = 255, alpha = 128) 
col2 = rgb(44, 127, 184, max = 255, alpha = 128)
col3 = rgb(8, 29, 88, max = 255, alpha = 128)

# Silver Lake - 105 # 
silver18 = clean_dat %>% filter(lakeid == '105' & year == '18')
silver19 = clean_dat %>% filter(lakeid == '105' & year == '19')
silver20 = clean_dat %>% filter(lakeid == '105' & year == '20')

sil_p1 = hist(silver18$bodylength, breaks = 15)
sil_p2 = hist(silver19$bodylength, breaks = 15)
sil_p3 = hist(silver20$bodylength, breaks = 15)

col1 = rgb(74, 166, 81, max = 255, alpha = 128) 
col2 = rgb(44, 127, 184, max = 255, alpha = 128)
col3 = rgb(8, 29, 88, max = 255, alpha = 128) 

windows(height=4, width=6) 
par(mfrow=c(1,2))

plot(nt_p1, col=col1, xlim = c(0,40), ylim = c(0, 35), xlab = 'Length (mm)', main = 'North Twin')
plot(nt_p2, col=col2, xlim = c(0,40), add=T)
plot(nt_p3, col=col3, xlim = c(0,40), add=T)
#legend('topright', legend = c('18', '19', '20'), col=c(col1, col2, col3), pch=15, cex=2)

plot(sil_p1, col=col1, xlim = c(0,40), ylim=c(0,35), xlab = 'Length (mm)', main = 'Silver Lake ')
plot(sil_p2, col=col2, xlim = c(0,40), add=T)
plot(sil_p3, col=col3, xlim = c(0,40), add=T)
legend('topright', legend = c('18', '19', '20'), col=c(col1, col2, col3), pch=15, cex=1.5)

#============================================================================================#

# Center Lake # 
center18 = clean_dat %>% filter(lakeid == '019' & year == '18')
center19 = clean_dat %>% filter(lakeid == '019' & year == '19')
center20 = clean_dat %>% filter(lakeid == '019' & year == '20')

cl_p1 = hist(center18$bodylength, breaks = 15)
cl_p2 = hist(center19$bodylength, breaks = 15)
cl_p3 = hist(center20$bodylength, breaks = 15)

col1 = rgb(74, 166, 81, max = 255, alpha = 128) 
col2 = rgb(44, 127, 184, max = 255, alpha = 128)
col3 = rgb(8, 29, 88, max = 255, alpha = 128)

# Five Island # 
fivisl18 = clean_dat %>% filter(lakeid == '036' & year == '18')
fivisl19 = clean_dat %>% filter(lakeid == '036' & year == '19')
fivisl20 = clean_dat %>% filter(lakeid == '036' & year == '20')

fi_p1 = hist(fivisl18$bodylength, breaks = 15)
fi_p2 = hist(fivisl19$bodylength, breaks = 15)
fi_p3 = hist(fivisl20$bodylength, breaks = 15)

col1 = rgb(74, 166, 81, max = 255, alpha = 128) 
col2 = rgb(44, 127, 184, max = 255, alpha = 128)
col3 = rgb(8, 29, 88, max = 255, alpha = 128)

windows(height=4, width=6) 
par(mfrow=c(1,2))
plot(cl_p1, col=col1, xlim = c(0,40), main = 'Center Lake', xlab = 'Length (mm)', cex.lab = 1.5, cex.axis = 1.5)
plot(cl_p2, col=col2, xlim = c(0,40), add=T)
plot(cl_p3, col=col3, xlim = c(0,40), add=T)
#legend('topright', legend = c('18', '19', '20'), col=c(col1, col2, col3), pch=15, cex=2)

plot(fi_p1, col=col1, xlim = c(0,40), main = 'Five Island', xlab = 'Length (mm)', cex.lab = 1.5, cex.axis = 1.5)
plot(fi_p2, col=col2, xlim = c(0,40), add=T)
plot(fi_p3, col=col3, xlim = c(0,40), add=T)
legend('topright', legend = c('18', '19', '20'), col=c(col1, col2, col3), pch=15, cex=1)
