# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# Load in data # 
full_miv = read_csv('18_20_carp_mivs.csv')
full_miv

# Macroinvertebrate Density Time-Series #=======================
# Present by lake and year # 

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

# MIV by lake and year # 

# zoop by lake (easier to apply LOESS fit if separated) #
miv_samplesum = full_miv %>%
  group_by(lake, year, sample) %>%
  summarize(sum_dens = sum(density)) %>%
  ungroup() %>%
  mutate(sum_dens = sum_dens+1)
miv_samplesum 

miv_samplesum$lake = as.character(miv_samplesum$lake)
miv_samplesum$lake[miv_samplesum$lake == '12'] <- 'Blue'
miv_samplesum$lake[miv_samplesum$lake == '19'] <- 'Center' 
miv_samplesum$lake[miv_samplesum$lake == '36'] <- 'Five.Island' 
miv_samplesum$lake[miv_samplesum$lake == '90'] <- 'North.Twin'
miv_samplesum$lake[miv_samplesum$lake == '105'] <- 'Silver'
miv_samplesum$lake[miv_samplesum$lake == '113'] <- 'Storm'
miv_samplesum$lake[miv_samplesum$lake == '406'] <- 'South.Twin'
miv_samplesum


blue = miv_samplesum %>%
  filter(lake == 'Blue')
center = miv_samplesum %>%
  filter(lake == 'Center')
five.island = miv_samplesum %>%
  filter(lake == 'Five.Island')
north.twin = miv_samplesum %>%
  filter(lake == 'North.Twin') 
silver = miv_samplesum %>%
  filter(lake == 'Silver')
storm = miv_samplesum %>%
  filter(lake == 'Storm')
south.twin = miv_samplesum %>%
  filter(lake == 'South.Twin')

set.seed(55)

# Combine MIV plots into one array #==========================

# Window for checking plot 
windows(height = 6, width = 8) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

## Zooplankton Final plot ========================

# Reference #==========================
#Blue Lake 
boxplot(log(sum_dens)~year, data = blue, ylim = c(log(1), log(20000)), 
        yaxt = 'n', col=c(ref_col_18, ref_col_19, ref_col_20), 
        ylab = 'MIV Density', col.axis = transparent, 
        cex.axis = 1.5)
#Add data points 
stripchart(log(sum_dens)~year, vertical = TRUE, data = blue, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000),
          log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','','','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','','','','','','','','10000',''),
     las=0) 
mtext(side=2, 'Reference \n Zooplankton Biomass', line = 1.5)
text(0.8, log(3), 'Blue', font=2, cex =2)

#South Twin
boxplot(log(sum_dens)~year, data = south.twin, ylim = c(log(1), log(20000)), 
        yaxt = 'n', col=c(ref_col_18, ref_col_19, ref_col_20), 
        ylab = 'MIV Density', col.axis = transparent, 
        cex.axis = 1.5)
#Add data points 
stripchart(log(sum_dens)~year, vertical = TRUE, data = south.twin, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000),
          log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','','','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','','','','','','','','10000',''),
     las=0) 
text(1.3, log(3), 'South Twin', font=2, cex =2)

#Storm 
boxplot(log(sum_dens)~year, data = storm, ylim = c(log(1), log(20000)), 
        yaxt = 'n', col=c(ref_col_18, ref_col_19, ref_col_20), 
        ylab = '',  
        cex.axis = 1.5)
#Add data points 
stripchart(log(sum_dens)~year, vertical = TRUE, data = storm, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000),
          log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','','','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','','','','','','','','10000',''),
     las=0) 
text(1, log(3), 'Storm', font=2, cex =2)

# Removal, Removal, Null #============================

#Center
boxplot(log(sum_dens)~year, data = center, ylim = c(log(1), log(20000)), 
        yaxt = 'n', col=c(rrn_col_18, rrn_col_19, rrn_col_20), 
        ylab = '', col.axis = transparent, 
        cex.axis = 1.5)
#Add data points 
stripchart(log(sum_dens)~year, vertical = TRUE, data = center, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000),
          log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','','','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','','','','','','','','10000',''),
     las=0) 
text(1, log(3), 'Center', font=2, cex =2)
mtext(side=2, 'Removal 2018-2019 \n Zooplankton Biomass', line = 1.5)
abline(v=2.5, lwd = 2, lty=3)
text(1.9, log(16000), 'Removal', cex=1.5)
text(3, log(8000), 'No \n Removal', cex=1.5)

#Five Island
boxplot(log(sum_dens)~year, data = five.island, ylim = c(log(1), log(20000)), 
        yaxt = 'n', col=c(rrn_col_18, rrn_col_19, rrn_col_20), 
        ylab = '', col.axis = transparent, 
        cex.axis = 1.5)
#Add data points 
stripchart(log(sum_dens)~year, vertical = TRUE, data = five.island, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000),
          log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','','','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','','','','','','','','10000',''),
     las=0) 
text(1.3, log(3), 'Five Island', font=2, cex =2)
abline(v=2.5, lwd = 2, lty=3)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
legend('bottom', legend = c('Reference',  'Removal 18-19', 'Removal 19-20'), pch=20, pt.cex=3, 
       cex=1.5, bty='n', col=c(ref_col_20, rrn_col_20, nrr_col_20))

# Null, Removal, Removal #============================

#North Twin
boxplot(log(sum_dens)~year, data = north.twin, ylim = c(log(1), log(20000)), 
        yaxt = 'n', col=c(nrr_col_18, nrr_col_19, nrr_col_20), 
        ylab = '',  
        cex.axis = 1.5)
#Add data points 
stripchart(log(sum_dens)~year, vertical = TRUE, data = north.twin, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000),
          log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','','','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','','','','','','','','10000',''),
     las=0) 
text(2.8, log(3), 'North Twin', font=2, cex =2)
abline(v=1.5, lwd = 2, lty=3)
mtext(side=2, 'Removal 2019-2020 \n Zooplankton Biomass', line = 1.5)
text(0.9, log(3), 'No \n Removal', cex=1.5)
text(2, log(16000), 'Removal', cex=1.5)

#Silver
boxplot(log(sum_dens)~year, data = silver, ylim = c(log(1), log(20000)), 
        yaxt = 'n', col=c(nrr_col_18, nrr_col_19, nrr_col_20), 
        ylab = '',  
        cex.axis = 1.5)
#Add data points 
stripchart(log(sum_dens)~year, vertical = TRUE, data = silver, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000),
          log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','','','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','','','','','','','','10000',''),
     las=0) 
text(3, log(3), 'Silver', font=2, cex =2)
abline(v=1.5, lwd = 2, lty=3)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')

# Macroinvertebrate Community Composition #===========================
miv_avg = full_miv %>% 
  group_by(lake, year, taxa) %>%
  summarize(samp_avg = mean(density)) %>% 
  ungroup()
miv_avg  

# Create a wide data frame
miv_ord = miv_avg %>% 
  select(lake,  year, taxa, samp_avg) %>%
  pivot_wider(id_cols = c(lake, year), 
              names_from = taxa, 
              values_from = samp_avg)
miv_ord

miv_ord$lake = as.character(miv_ord$lake)
miv_ord$lake[miv_ord$lake == '12'] <- 'Blue'
miv_ord$lake[miv_ord$lake == '19'] <- 'Center' 
miv_ord$lake[miv_ord$lake == '36'] <- 'Five.Island' 
miv_ord$lake[miv_ord$lake == '90'] <- 'North.Twin'
miv_ord$lake[miv_ord$lake == '105'] <- 'Silver'
miv_ord$lake[miv_ord$lake == '113'] <- 'Storm'
miv_ord$lake[miv_ord$lake == '406'] <- 'South.Twin'
miv_ord

# Replace NAs with 0 # 
miv_ord[is.na(miv_ord)] <- 0
miv_ord

# Remove columns that sum to zero 
miv_ord_select = miv_ord[, 3:26] %>% select_if(colSums(.) !=0)
miv_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
miv_ord_m = as.matrix(miv_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
miv_hell = decostand(miv_ord_m, method = 'hellinger')

graphics.off()

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
miv_nmds = metaMDS(miv_hell, k=2,trymax=100, distance='bray') 
miv_nmds # Stress is 0.13, decent
stressplot(miv_nmds) # Pretty low scatter, good fit  
plot(miv_nmds) # Check it out

ordiplot(miv_nmds, type='n')
orditorp(miv_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(miv_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = miv_ord$lake # pull pond ID identifier
data.scores$year = miv_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

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

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(shape=year,color=lake)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c(ref_col_20, ref_col_19, ref_col_18, rrn_col_20, rrn_col_19, nrr_col_20, nrr_col_19)) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
xx
windows(height=6,width=6)
xx # plot the figure 

# MIV Community Composition - by lake #========================= 

## Don't Average by Sample ## 
 
full_miv

# Create a wide data frame
miv_ord = full_miv %>% 
  select(lake,  year, taxa, sample, density) %>%
  pivot_wider(id_cols = c(lake, year, sample), 
              names_from = taxa, 
              values_from = density)
miv_ord

miv_ord$lake = as.character(miv_ord$lake)
miv_ord$lake[miv_ord$lake == '12'] <- 'Blue'
miv_ord$lake[miv_ord$lake == '19'] <- 'Center' 
miv_ord$lake[miv_ord$lake == '36'] <- 'Five.Island' 
miv_ord$lake[miv_ord$lake == '90'] <- 'North.Twin'
miv_ord$lake[miv_ord$lake == '105'] <- 'Silver'
miv_ord$lake[miv_ord$lake == '113'] <- 'Storm'
miv_ord$lake[miv_ord$lake == '406'] <- 'South.Twin'
miv_ord

# Replace NAs with 0 # 
miv_ord[is.na(miv_ord)] <- 0
miv_ord

# Split by Lake 
## Reference ##
blue_ord = miv_ord %>%
  filter(lake == 'Blue')
south.twin_ord = miv_ord %>%
  filter(lake == 'South.Twin')
storm_ord = miv_ord %>%
  filter(lake == 'Storm')
## RRN ## 
center_ord = miv_ord %>%
  filter(lake == 'Center')
five.island_ord = miv_ord %>%
  filter(lake == 'Five.Island')
## NRR ##
north.twin_ord = miv_ord %>%
  filter(lake == 'North.Twin')
silver_ord = miv_ord %>%
  filter(lake == 'Silver')

# Blue # 
# Remove columns that sum to zero 
blue_ord_select = blue_ord[, 4:27] %>% select_if(colSums(.) !=0)
blue_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
blue_ord_m = as.matrix(blue_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
blue_hell = decostand(blue_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
blue_nmds = metaMDS(blue_hell, k=2,trymax=1000, distance='bray') 
blue_nmds # Stress is 0.13, decent

stressplot(blue_nmds) # Pretty low scatter, good fit  
plot(blue_nmds) # Check it out

ordiplot(blue_nmds, type='n')
orditorp(blue_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(blue_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = blue_ord$lake # pull pond ID identifier
data.scores$season = blue_ord$season # pull season
data.scores$year = blue_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

blue_xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(color=year)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c(ref_col_20, ref_col_18, 'gray60')) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
blue_xx

# South Twin # 
# Remove columns that sum to zero 
south.twin_ord_select = south.twin_ord[, 5:49] %>% select_if(colSums(.) !=0)
south.twin_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
south.twin_ord_m = as.matrix(south.twin_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
south.twin_hell = decostand(south.twin_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
south.twin_nmds = metaMDS(south.twin_hell, k=2,trymax=100, distance='bray') 
south.twin_nmds # Stress is 0.13, decent
stressplot(south.twin_nmds) # Pretty low scatter, good fit  
plot(south.twin_nmds) # Check it out

ordiplot(south.twin_nmds, type='n')
orditorp(south.twin_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(south.twin_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = south.twin_ord$lake # pull pond ID identifier
data.scores$season = south.twin_ord$season # pull season
data.scores$year = south.twin_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

south.twin_xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(shape=season,color=year)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c(ref_col_20, ref_col_18, 'gray60')) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
south.twin_xx

# Storm # 
# Remove columns that sum to zero 
storm_ord_select = storm_ord[, 5:49] %>% select_if(colSums(.) !=0)
storm_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
storm_ord_m = as.matrix(storm_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
storm_hell = decostand(storm_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
storm_nmds = metaMDS(storm_hell, k=2,trymax=100, distance='bray') 
storm_nmds # Stress is 0.13, decent
stressplot(storm_nmds) # Pretty low scatter, good fit  
plot(storm_nmds) # Check it out

ordiplot(storm_nmds, type='n')
orditorp(storm_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(storm_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = storm_ord$lake # pull pond ID identifier
data.scores$season = storm_ord$season # pull season
data.scores$year = storm_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

storm_xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(shape=season,color=year)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c(ref_col_20, ref_col_18, 'gray60')) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
storm_xx

# Center # 
# Remove columns that sum to zero 
center_ord_select = center_ord[, 5:49] %>% select_if(colSums(.) !=0)
center_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
center_ord_m = as.matrix(center_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
center_hell = decostand(center_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
center_nmds = metaMDS(center_hell, k=2,trymax=100, distance='bray') 
center_nmds # Stress is 0.13, decent
stressplot(center_nmds) # Pretty low scatter, good fit  
plot(center_nmds) # Check it out

ordiplot(center_nmds, type='n')
orditorp(center_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(center_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = center_ord$lake # pull pond ID identifier
data.scores$season = center_ord$season # pull season
data.scores$year = center_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

center_xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(shape=season,color=year)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c(rrn_col_20, rrn_col_19, 'gray60')) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
center_xx

# Five Island # 
# Remove columns that sum to zero 
five.island_ord_select = five.island_ord[, 5:49] %>% select_if(colSums(.) !=0)
five.island_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
five.island_ord_m = as.matrix(five.island_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
five.island_hell = decostand(five.island_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
five.island_nmds = metaMDS(five.island_hell, k=2,trymax=100, distance='bray') 
five.island_nmds # Stress is 0.13, decent
stressplot(five.island_nmds) # Pretty low scatter, good fit  
plot(five.island_nmds) # Check it out

ordiplot(five.island_nmds, type='n')
orditorp(five.island_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(five.island_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = five.island_ord$lake # pull pond ID identifier
data.scores$season = five.island_ord$season # pull season
data.scores$year = five.island_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

five.island_xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(shape=season,color=year)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c(rrn_col_20, rrn_col_19, 'gray60')) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
five.island_xx

# North Twin # 
# Remove columns that sum to zero 
north.twin_ord_select = north.twin_ord[, 5:49] %>% select_if(colSums(.) !=0)
north.twin_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
north.twin_ord_m = as.matrix(north.twin_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
north.twin_hell = decostand(north.twin_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
north.twin_nmds = metaMDS(north.twin_hell, k=2,trymax=100, distance='bray') 
north.twin_nmds # Stress is 0.13, decent
stressplot(north.twin_nmds) # Pretty low scatter, good fit  
plot(north.twin_nmds) # Check it out

ordiplot(north.twin_nmds, type='n')
orditorp(north.twin_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(north.twin_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = north.twin_ord$lake # pull pond ID identifier
data.scores$season = north.twin_ord$season # pull season
data.scores$year = north.twin_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

north.twin_xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(shape=season,color=year)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c('gray60', nrr_col_19,  nrr_col_20)) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
north.twin_xx

# Silver # 
# Remove columns that sum to zero 
silver_ord_select = silver_ord[, 5:49] %>% select_if(colSums(.) !=0)
silver_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
silver_ord_m = as.matrix(silver_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
silver_hell = decostand(silver_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
silver_nmds = metaMDS(silver_hell, k=2,trymax=100, distance='bray') 
silver_nmds # Stress is 0.13, decent
stressplot(silver_nmds) # Pretty low scatter, good fit  
plot(silver_nmds) # Check it out

ordiplot(silver_nmds, type='n')
orditorp(silver_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(silver_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = silver_ord$lake # pull pond ID identifier
data.scores$season = silver_ord$season # pull season
data.scores$year = silver_ord$year # pull year
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

silver_xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(shape=season,color=year)) + # plot points 
  #geom_text(x=0.75,y=0.9, label='Stress = 0.152', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  scale_color_manual(values = c('gray60', nrr_col_19,  nrr_col_20)) + 
  labs(x='NMDS1', shape='year', y='NMDS2') 
silver_xx

# Ordination Array #==========================
if (!require(ggpubr)) install.packages('ggpubr')
library('ggpubr')
windows(height=4, width=5)
blue_xx
south.twin_xx
storm_xx

center_xx
five.island_xx

north.twin_xx
silver_xx



