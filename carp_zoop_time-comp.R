## Zooplantkon Biomass & Community Composition ## 

# Combine zooplankton biomass plots into one array #==========================

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
plot(y=log(blue18$tot_biomass), x=blue18$doy ,type = 'o', yaxt='n', pch = 20, col.axis = transparent, 
     cex = 1.5, xlab = '',  ylim=c(log(1), log(4000)),
     ylab = '', xlim=c(125, 300), col=ref_col_18, lwd = 2, lty=2)
axis(side=2,
     at=c(log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','',''),
     las=0) 
lines(y=log(blue19$tot_biomass), x=blue19$doy, type='o', col=ref_col_19, lwd=2, pch = 18, cex=1.5, lty=2)
lines(y=log(blue20$tot_biomass), x=blue20$doy, type='o', col=ref_col_20, lwd=2, pch = 15, cex=1.5, lty=2)
mtext(side=2, 'Reference \n Zooplankton Biomass', line = 1.5)
text(145, log(3), 'Blue', font=2, cex =2)

#South Twin
plot(y=log(south.twin18$tot_biomass), x=south.twin18$doy ,type = 'o', yaxt='n', pch = 20, col.axis = transparent, 
     cex = 1.5, xlab = '',  ylim=c(log(1), log(4000)),
     ylab = '', xlim=c(125, 300), col=ref_col_18, lwd = 2, lty=2)
axis(side=2,
     at=c( log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','',''),
     las=0) 
lines(y=log(south.twin19$tot_biomass), x=south.twin19$doy, type='o', col=ref_col_19, lwd=2, pch = 18, cex=1.5, lty=2)
lines(y=log(south.twin20$tot_biomass), x=south.twin20$doy, type='o', col=ref_col_20, lwd=2, pch = 15, cex=1.5, lty=2)
text(175, log(3), 'South Twin', font=2, cex =2)

#Storm 
plot(y=log(storm18$tot_biomass), x=storm18$doy ,type = 'o', yaxt='n', pch = 20, 
     cex = 1.5, xlab = '',  ylim=c(log(1), log(4000)), cex.axis = 1.4,
     ylab = '', xlim=c(125, 300), col=ref_col_18, lwd = 2,lty=2)
axis(side=2,
     at=c( log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
          log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
          log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','',''),
     las=0) 
lines(y=log(storm19$tot_biomass), x=storm19$doy, type='o', col=ref_col_19, lwd=2, pch = 18, cex=1.5,lty=2)
lines(y=log(storm20$tot_biomass), x=storm20$doy, type='o', col=ref_col_20, lwd=2, pch = 15, cex=1.5,lty=2)
text(150, log(3), 'Storm', font=2, cex =2)

# Removal, Removal, Null #============================

#Center
plot(y=log(center18$tot_biomass), x=center18$doy ,type = 'o', yaxt='n', pch = 20, col.axis = transparent, 
     cex = 1.5, xlab = '',  ylim=c(log(1), log(4000)),
     ylab = '', xlim=c(125, 300), col=rrn_col_18, lwd = 2)
axis(side=2,
     at=c( log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
           log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
           log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
           log(2000), log(3000), log(4000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','',''),
     las=0) 
lines(y=log(center19$tot_biomass), x=center19$doy, type='o', col=rrn_col_19, lwd=2, pch = 18, cex=1.5)
lines(y=log(center20$tot_biomass), x=center20$doy, type='o', col=rrn_col_20, lwd=2, pch = 15, cex=1.5, lty=2)
mtext(side=2, 'Removal 2018-2019 \n Zooplankton Biomass', line = 1.5)
text(155, log(3), 'Center', font=2, cex =2)

#Five Island
plot(y=log(five.island18$tot_biomass), x=five.island18$doy ,type = 'o', yaxt='n', pch = 20, col.axis = transparent, 
     cex = 1.5, xlab = '',  ylim=c(log(1), log(4000)),
     ylab = '', xlim=c(125, 300), col=rrn_col_18, lwd = 2)
axis(side=2,
     at=c( log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
           log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
           log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
           log(2000), log(3000), log(4000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','',''),
     las=0) 
lines(y=log(five.island19$tot_biomass), x=five.island19$doy, type='o', col=rrn_col_19, lwd=2, pch = 18, cex=1.5)
lines(y=log(five.island20$tot_biomass), x=five.island20$doy, type='o', col=rrn_col_20, lwd=2, pch = 15, cex=1.5, lty = 2)
text(175, log(3), 'Five Island', font=2, cex =2)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
legend("bottom", legend =c('2018', '2019', '2020'), pch=c(20,18,15), pt.cex=3, cex=1.5, bty='n',
       col = c(black18, black19, black20))
par(new=T)
legend(x=0.75, y=4.8, legend = c('No removal', 'Removal'), lty = c(2,1), bty='n', cex = 1.5)

# Null, Removal, Removal #============================

#North Twin
plot(y=log(north.twin18$tot_biomass), x=north.twin18$doy ,type = 'o', yaxt='n', pch = 20, cex.axis = 1.4,
     cex = 1.5, xlab = '',  ylim=c(log(1), log(4000)),
     ylab = '', xlim=c(125, 300), col=nrr_col_18, lwd = 2, lty=2)
axis(side=2,
     at=c( log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
           log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
           log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
           log(2000), log(3000), log(4000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','',''),
     las=0) 
lines(y=log(north.twin19$tot_biomass), x=north.twin19$doy, type='o', col=nrr_col_19, lwd=2, pch = 18, cex=1.5)
lines(y=log(north.twin20$tot_biomass), x=north.twin20$doy, type='o', col=nrr_col_20, lwd=2, pch = 15, cex=1.5)
mtext(side=2, 'Removal 2019-20 \n Zooplankton Biomass', line = 1.5)
text(175, log(3), 'North Twin', font=2, cex =2)

#Silver
plot(y=log(silver18$tot_biomass), x=silver18$doy ,type = 'o', yaxt='n', pch = 20, cex.axis = 1.4, lty = 2,
     cex = 1.5, xlab = '',  ylim=c(log(1), log(4000)),
     ylab = '', xlim=c(125, 300), col=nrr_col_18, lwd = 2)
axis(side=2,
     at=c( log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10), 
           log(20), log(30), log(40),log(50),log(60),log(70),log(80),log(90), log(100), 
           log(200), log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
           log(2000), log(3000), log(4000)), #Where the tick marks should be drawn
     labels = c('','','','','','', '','10','','','','','','','','', '100',
                '','','','','','','','', '1000','','',''),
     las=0) 
lines(y=log(silver19$tot_biomass), x=silver19$doy, type='o', col=nrr_col_19, lwd=2, pch = 18, cex=1.5)
lines(y=log(silver20$tot_biomass), x=silver20$doy, type='o', col=nrr_col_20, lwd=2, pch = 15, cex=1.5)
mtext(side = 1, 'Day of Year', line = 2)
text(150, log(3), 'Silver', font=2, cex =2)

#empty plot 
plot(1, type = "n", xlab = "", yaxt = 'n', xaxt='n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
legend('top', legend = c('Reference',  'Removal 18-19', 'Removal 19-20'), pch=20, pt.cex=3, 
       cex=1.5, bty='n', col=c(ref_col_20, rrn_col_20, nrr_col_20))


# Zooplankton Community Composition - Combined Figure #=========================


# The goal of NMDS is to represent the original position of communities in
# multidimensional space as accurately as possible using a reduced number 
# of dimensions that can be easily plotted and visualized 

# NMDS does not use the absolute abundances of species in communities, but
# rather their RANK ORDER!
# The use of ranks omits some of the issues associated with using absolute
# distance (e.g., sensitivity to transformation), and as a result is much 
# more flexible technique that accepts a variety of types of data
# (It is also where the "non-metric" part of the name comes from)

# Required Libraries for analysis and visualization
if (!require(vegan)) install.packages('vegan')
library(vegan)

# Colors - Each plot will be a removal type # 
# Separate by year (shade), season (point) 
full_zoops

# ALM Duplicates need to be removed so that I can turn this into a wide dataset. Dates removed have data from carp synoptic sampling.  
zp_nodups = full_zoops %>%
  filter(sampleID != 'A18019150001') %>%
  filter(sampleID != 'A19036204007') %>%
  filter(sampleID != 'A18105151005')
zp_nodups 

# Create a wide data frame
zp_ord = zp_nodups %>% 
  select(lake, doy, year, season,  taxon, biomass) %>%
  pivot_wider(id_cols = c(lake, year, season, doy), 
              names_from = taxon, 
              values_from = biomass)
zp_ord

# Replace NAs with 0 # 
zp_ord[is.na(zp_ord)] <- 0
zp_ord

# Remove columns that sum to zero 
zp_ord_select = zp_ord[, 5:33] %>% select_if(colSums(.) !=0)
zp_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
zp_ord_m = as.matrix(zp_ord_select[,5:22])

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
zp_hell = decostand(zp_ord_m, method = 'hellinger')

graphics.off()

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
zp_nmds = metaMDS(zp_hell, k=2,trymax=100, distance='bray') 
zp_nmds # Stress is 0.13, decent
stressplot(zp_nmds) # Pretty low scatter, good fit  
plot(zp_nmds) # Check it out

ordiplot(zp_nmds, type='n')
orditorp(zp_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(zp_nmds)
data.scores = as.data.frame(dat$sites)

# Add columns to the data frame from original data 
data.scores$lake = zp_ord$lake # pull pond ID identifier
data.scores$season = zp_ord$season # pull season
data.scores$year = zp_ord$year # pull year
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

# Zooplankton Community Composition - by lake #=========================
# Colors - Each plot will be a removal type # 
library(vegan)

clad = adjustcolor(col = '#dd4444', alpha.f = 0.6)
bos = adjustcolor(col = '#f48080', alpha.f = 0.6)
chy = adjustcolor(col = '#ffdcdc', alpha.f = 0.6)
dap = adjustcolor(col = '#540101', alpha.f = 0.6)
rot = adjustcolor(col = 'gray99', alpha.f = 0.6)
cyc = adjustcolor(col = '#2d676f', alpha.f = 0.4)
cal = adjustcolor(col = '#194b4f', alpha.f = 0.8)
nau = adjustcolor(col = '#00afbf', alpha.f = 0.6)

colors = c(clad, bos, chy, dap, rot, cyc, cal, nau)

# Separate by year (shade), season (point) 
full_zoops

# ALM Duplicates need to be removed so that I can turn this into a wide dataset. Dates removed have data from carp synoptic sampling.  
zp_nodups = full_zoops %>%
  filter(sampleID != 'A18019150001') %>%
  filter(sampleID != 'A19036204007') %>%
  filter(sampleID != 'A18105151005')
zp_nodups 

# Create a wide data frame
zp_ord = zp_nodups %>% 
  select(lake, doy, year, season,  taxon, biomass) %>%
  pivot_wider(id_cols = c(lake, year, season, doy), 
              names_from = taxon, 
              values_from = biomass) %>%
  filter(season == 'summer')
zp_ord

# Replace NAs with 0 # 
zp_ord[is.na(zp_ord)] <- 0
zp_ord

# Split by Lake 
## Reference ##
blue_ord = zp_ord %>%
  filter(lake == 'Blue')
south.twin_ord = zp_ord %>%
  filter(lake == 'South.Twin')
storm_ord = zp_ord %>%
  filter(lake == 'Storm')

## RRN ## 
center_ord = zp_ord %>%
  filter(lake == 'Center')
five.island_ord = zp_ord %>%
  filter(lake == 'Five.Island')

## NRR ##
north.twin_ord = zp_ord %>%
  filter(lake == 'North.Twin')
silver_ord = zp_ord %>%
  filter(lake == 'Silver')

# Blue # 
# Remove columns that sum to zero 
blue_ord_select = blue_ord[, 5:49] %>% select_if(colSums(.) !=0)
blue_ord_select

# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
blue_ord_m = as.matrix(blue_ord_select)

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
blue_hell = decostand(blue_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation
blue_nmds = metaMDS(blue_hell, k=2,trymax=100, distance='bray') 
blue_nmds # Stress is 0.13, decent
stressplot(blue_nmds) # Pretty low scatter, good fit  
plot(blue_nmds) # Check it out

ordiplot(blue_nmds, type='n')
orditorp(blue_nmds, display='species') # Remember Calanoids and Large Cladocera are on the right 

# Extract nMDS scores (x and y coordinates, for better plotting)
dat = scores(blue_nmds)
data.scores = as.data.frame(dat$sites) 
species = as.data.frame(blue_nmds$species) %>%
  rownames_to_column('taxon') %>% 
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
                           .$taxon %in% c("Ceriodaphnia") ~ 'Ceriodaphnia', 
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
                           .$taxon %in% c("Nauplii") ~ "Nauplii")) 
species 


# Add columns to the data frame from original data 
data.scores$lake = blue_ord$lake # pull pond ID identifier
data.scores$season = blue_ord$season # pull season
data.scores$year = blue_ord$year # pull year 
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
blue18 = data.scores %>%
  filter(year == 2018) %>% 
  as.data.frame()
plot(y = blue18$NMDS2, x = blue18$NMDS1, pch = 19, col = ref_col_20,
     cex = 2, xlim = c(-0.7, 1.1), ylim = c(-1.5, 1), 
     ylab = 'NMDS2', xlab = 'NMDS1')
blue19 = data.scores %>%
  filter(year == 2019)
points(y = blue19$NMDS2, x = blue19$NMDS1, pch = 15, col = ref_col_19, cex = 2)
blue20 = data.scores %>%
  filter(year == 2020)
points(y = blue20$NMDS2, x = blue20$NMDS1, pch = 18, col = ref_col_18, cex = 2)

points(y = species$MDS2, x = species$MDS1, pch = 3, col = transparent)

points(y=species$MDS1, x = species$MDS2, pch = 20, 
     labels = species$taxon, cex = 0.6, pos = 1)
legend('topright', legend = c('2018', '2019', '2020'), bty = 'n', pch = c(19, 15, 18), 
       cex = 1, col = c(ref_col_20, ref_col_19, ref_col_18))

# South Twin # 
# south.twin # 
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
species = as.data.frame(south.twin_nmds$species) %>%
  rownames_to_column('taxon') %>% 
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
                           .$taxon %in% c("Ceriodaphnia") ~ 'Ceriodaphnia', 
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
                           .$taxon %in% c("Nauplii") ~ "Nauplii")) 
species 


# Add columns to the data frame from original data 
data.scores$lake = south.twin_ord$lake # pull pond ID identifier
data.scores$season = south.twin_ord$season # pull season
data.scores$year = south.twin_ord$year # pull year 
head(data.scores)

# Create a factor with levels 
data.scores$lake = factor(data.scores$lake, levels = c('Blue', 'South.Twin', 'Storm', 'Center', 'Five.Island', 
                                                       'North.Twin', 'Silver')) 
data.scores$year = factor(data.scores$year, levels = c('2018', '2019', '2020'))

windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
south.twin18 = data.scores %>%
  filter(year == 2018) %>% 
  as.data.frame()
plot(y = south.twin18$NMDS2, x = south.twin18$NMDS1, pch = 19, col = ref_col_20,
     cex = 2, xlim = c(-0.7, 1.1), ylim = c(-1.5, 1), 
     ylab = 'NMDS2', xlab = 'NMDS1')
south.twin19 = data.scores %>%
  filter(year == 2019)
points(y = south.twin19$NMDS2, x = south.twin19$NMDS1, pch = 15, col = ref_col_19, cex = 2)
south.twin20 = data.scores %>%
  filter(year == 2020)
points(y = south.twin20$NMDS2, x = south.twin20$NMDS1, pch = 18, col = ref_col_18, cex = 2)

points(y = species$MDS2, x = species$MDS1, pch = 3, col = transparent)

points(y=species$MDS1, x = species$MDS2, pch = 20, 
       labels = species$taxon, cex = 0.6, pos = 1)
legend('topright', legend = c('2018', '2019', '2020'), bty = 'n', pch = c(19, 15, 18), 
       cex = 1, col = c(ref_col_20, ref_col_19, ref_col_18))

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

