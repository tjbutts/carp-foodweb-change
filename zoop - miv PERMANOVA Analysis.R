# Carp_Zoop_PERMANOVA # 


# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
# pairwise 
if(!require(spaa)) install.packages('spaa')
library(spaa)
if(!require(EcolUtils)) devtools::install_github("GuillemSalazar/EcolUtils")
library(EcolUtils)
if(!require(vegan)) install.packages('vegan')
library(vegan)

# Load in function to perform pairwise analysis of adonis function in vegan # 
adonis.pair<-function(dist.mat,Factor,nper=999,corr.method="fdr"){
  require(vegan)
  as.factor(Factor)
  comb.fact<-combn(levels(Factor),2)
  pv<-NULL
  R2<-NULL
  SS<-NULL
  MeanSqs<-NULL
  F.Model<-NULL
  for (i in 1:dim(comb.fact)[2]){
    model.temp<-adonis(as.dist(as.matrix(dist.mat)[Factor==comb.fact[1,i] | Factor==comb.fact[2,i],Factor==comb.fact[1,i] | Factor==comb.fact[2,i]])~Factor[Factor==comb.fact[1,i] | Factor==comb.fact[2,i]],permutations=nper)
    pv<-c(pv,model.temp$aov.tab[[6]][1])
    R2<-c(R2,model.temp$aov.tab$R2[1])
    SS<-c(SS,model.temp$aov.tab[[2]][1])
    MeanSqs<-c(MeanSqs,model.temp$aov.tab[[3]][1])
    F.Model<-c(F.Model,model.temp$aov.tab[[4]][1])
  }
  pv.corr<-p.adjust(pv,method=corr.method)
  data.frame(combination=paste(comb.fact[1,],comb.fact[2,],sep=" <-> "),SumsOfSqs=SS,MeanSqs=MeanSqs,F.Model=F.Model,R2=R2,P.value=pv,P.value.corrected=pv.corr)}


# Load in data #==================
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

taxa = full_zoops %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year, season), 
              names_from = 'taxon', 
              values_from = 'biomass')
taxa[is.na(taxa)] <- 0
taxa.raw = taxa[,6:ncol(taxa)]
taxa.list = taxa.raw[,colSums(taxa.raw) > 0]
taxa.list$sampleID = taxa$sampleID
taxa.long = taxa.list %>% 
  pivot_longer(cols = c(!(sampleID)), 
               names_to = 'taxon', 
               values_to = 'biomass')
grouping = full_zoops %>% 
  select(taxon, group)

taxa.unique = left_join(taxa.long, grouping, by = 'taxon') %>% 
  distinct() 

taxa.unique

## Summer Zooplankton Community ## =================================
# Separate by lake # 
storm = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
storm
storm[is.na(storm)] <- 0
storm.raw = storm[,5:ncol(storm)]
storm.raw
storm.wide = storm.raw[,colSums(storm.raw) > 0]


center = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
center[is.na(center)] <- 0
center.raw = center[,5:ncol(center)]
center.raw
center.wide = center.raw[,colSums(center.raw) > 0]

### PERMANOVA of zooplankton communities by year # =============================
set.seed(55)

# Blue # 
blue = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
blue[is.na(blue)] <- 0
blue.raw = blue[,5:ncol(blue)]
blue.raw
blue.wide = blue.raw[,colSums(blue.raw) > 0]

blue.taxa = decostand(as.matrix(blue.wide), method = 'hellinger') # Hellinger transform data # 
blue.dist = vegdist(blue.taxa, method = 'bray')
blue.group = blue[,1:4] %>% 
  mutate(year = as.factor(year)) 

blue.group
blue.dist



# PERMANOVA # 
adonis2(blue.dist~year, data = blue.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(blue.taxa), blue.group$year)


# Storm # 
storm.taxa = decostand(as.matrix(storm.wide), method = 'hellinger') # Hellinger transform data # 
storm.dist = vegdist(storm.taxa, method = 'bray')
storm.group = storm[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(storm.dist~year, data = storm.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(storm.taxa), storm.group$year)

# South Twin # 
south.twin = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
south.twin[is.na(south.twin)] <- 0
south.twin.raw = south.twin[,5:ncol(south.twin)]
south.twin.raw
south.twin.wide = south.twin.raw[,colSums(south.twin.raw) > 0]

south.twin.taxa = decostand(as.matrix(south.twin.wide), method = 'hellinger') # Hellinger transform data # 
south.twin.dist = vegdist(south.twin.taxa, method = 'bray')
south.twin.group = south.twin[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(south.twin.dist~year, data = south.twin.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(south.twin.taxa), south.twin.group$year)

# Center # 
center.taxa = decostand(as.matrix(center.wide), method = 'hellinger') # Hellinger transform data # 
center.dist = vegdist(center.taxa, method = 'bray')
center.group = center[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(center.dist~year, data = center.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(center.taxa), center.group$year)

# Five Island # *****
five.island = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass') 
five.island[is.na(five.island)] <- 0
five.island.raw = five.island[,5:ncol(five.island)]
five.island.raw
five.island.wide = five.island.raw[,colSums(five.island.raw) > 0]

five.island.taxa = decostand(as.matrix(five.island.wide), method = 'hellinger') # Hellinger transform data # 
five.island.dist = vegdist(five.island.taxa, method = 'bray')
five.island.group = five.island[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(five.island.dist~year, data = five.island.group, permutations = 999, method = 'bray')

# Pairwise 
set.seed(55)
adonis.pair(vegdist(five.island.taxa), five.island.group$year)

# North Twin # ****
north.twin = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(season == 'summer')  %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass') 
north.twin[is.na(north.twin)] <- 0
north.twin.raw = north.twin[,5:ncol(north.twin)]
north.twin.raw
north.twin.wide = north.twin.raw[,colSums(north.twin.raw) > 0]

north.twin.taxa = decostand(as.matrix(north.twin.wide), method = 'hellinger') # Hellinger transform data # 
north.twin.dist = vegdist(north.twin.taxa, method = 'bray')
north.twin.group = north.twin[,1:4] %>% 
  mutate(year = as.factor(year)) 
set.seed(55)
# PERMANOVA # 
adonis2(north.twin.dist~year, data = north.twin.group, permutations = 999, method = 'bray')

# Pairwise 

adonis.pair(vegdist(north.twin.taxa), north.twin.group$year)

# Silver # 
silver = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(season == 'summer')  %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass') 
silver[is.na(silver)] <- 0
silver.raw = silver[,5:ncol(silver)]
silver.raw
silver.wide = silver.raw[,colSums(silver.raw) > 0]

silver.taxa = decostand(as.matrix(silver.wide), method = 'hellinger') # Hellinger transform data # 
silver.dist = vegdist(silver.taxa, method = 'bray')
silver.group = silver[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
set.seed(55)
adonis2(silver.dist~year, data = silver.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(silver.taxa), silver.group$year)

## Spring Zooplankton Community ## ======================================
# Separate by lake # 
blue = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(season == 'spring') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass') 
blue[is.na(blue)] <- 0
blue.raw = blue[,5:ncol(blue)]
blue.raw
blue.wide = blue.raw[,colSums(blue.raw) > 0]

silver = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(season == 'spring')  %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
silver[is.na(silver)] <- 0
silver.raw = silver[,5:ncol(silver)]
silver.raw
silver.wide = silver.raw[,colSums(silver.raw) > 0]

center = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(season == 'spring') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
center[is.na(center)] <- 0
center.raw = center[,5:ncol(center)]
center.raw
center.wide = center.raw[,colSums(center.raw) > 0]

five.island = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(season == 'spring') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
five.island[is.na(five.island)] <- 0
five.island.raw = five.island[,5:ncol(five.island)]
five.island.raw
five.island.wide = five.island.raw[,colSums(five.island.raw) > 0]


### PERMANOVA of zooplankton communities by year # =============================
# Blue # (unable to do comparison - two few data) 

# Storm # 
storm = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(season == 'spring') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
storm
storm[is.na(storm)] <- 0
storm.raw = storm[,5:ncol(storm)]
storm.raw
storm.wide = storm.raw[,colSums(storm.raw) > 0]

storm.taxa = decostand(as.matrix(storm.wide), method = 'hellinger') # Hellinger transform data # 
storm.dist = vegdist(storm.taxa, method = 'bray')
storm.group = storm[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(storm.dist~year, data = storm.group, permutations = 999, method = 'bray')

# Pairwise 
set.seed(55)
adonis.pair(vegdist(storm.taxa), storm.group$year)

# South Twin # 
south.twin = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(season == 'spring') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass') 
south.twin[is.na(south.twin)] <- 0
south.twin.raw = south.twin[,5:ncol(south.twin)]
south.twin.raw
south.twin.wide = south.twin.raw[,colSums(south.twin.raw) > 0]

south.twin.taxa = decostand(as.matrix(south.twin.wide), method = 'hellinger') # Hellinger transform data # 
south.twin.dist = vegdist(south.twin.taxa, method = 'bray')
south.twin.group = south.twin[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(south.twin.dist~year, data = south.twin.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(south.twin.taxa), south.twin.group$year)

# Center # 
center.taxa = decostand(as.matrix(center.wide), method = 'hellinger') # Hellinger transform data # 
center.dist = vegdist(center.taxa, method = 'bray')
center.group = center[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(center.dist~year, data = center.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(center.taxa), center.group$year)

# Five Island # 
five.island.taxa = decostand(as.matrix(five.island.wide), method = 'hellinger') # Hellinger transform data # 
five.island.dist = vegdist(five.island.taxa, method = 'bray')
five.island.group = five.island[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(five.island.dist~year, data = five.island.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(five.island.taxa), five.island.group$year)

# North Twin # ****
north.twin = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(season == 'spring')  %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
north.twin[is.na(north.twin)] <- 0
north.twin.raw = north.twin[,5:ncol(north.twin)]
north.twin.raw
north.twin.wide = north.twin.raw[,colSums(north.twin.raw) > 0]

north.twin.taxa = decostand(as.matrix(north.twin.wide), method = 'hellinger') # Hellinger transform data # 
north.twin.dist = vegdist(north.twin.taxa, method = 'bray')
north.twin.group = north.twin[,1:4] %>% 
  mutate(year = as.factor(year)) 

set.seed(55)
# PERMANOVA # 
adonis2(north.twin.dist~year, data = north.twin.group, permutations = 999, method = 'bray')

# Pairwise 
set.seed(55)
adonis.pair(vegdist(north.twin.taxa), north.twin.group$year)

# Silver # 
silver.taxa = decostand(as.matrix(silver.wide), method = 'hellinger') # Hellinger transform data # 
silver.dist = vegdist(silver.taxa, method = 'bray')
silver.group = silver[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(silver.dist~year, data = silver.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(silver.taxa), silver.group$year)

## Summer Zooplankton Community ## =================================
# Separate by lake # 
blue = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass') 
blue[is.na(blue)] <- 0
blue.raw = blue[,5:ncol(blue)]
blue.raw
blue.wide = blue.raw[,colSums(blue.raw) > 0]


south.twin = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
south.twin[is.na(south.twin)] <- 0
south.twin.raw = south.twin[,5:ncol(south.twin)]
south.twin.raw
south.twin.wide = south.twin.raw[,colSums(south.twin.raw) > 0]

storm = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
storm
storm[is.na(storm)] <- 0
storm.raw = storm[,5:ncol(storm)]
storm.raw
storm.wide = storm.raw[,colSums(storm.raw) > 0]

north.twin = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(season == 'summer')  %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
north.twin[is.na(north.twin)] <- 0
north.twin.raw = north.twin[,5:ncol(north.twin)]
north.twin.raw
north.twin.wide = north.twin.raw[,colSums(north.twin.raw) > 0]

silver = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(season == 'summer')  %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
silver[is.na(silver)] <- 0
silver.raw = silver[,5:ncol(silver)]
silver.raw
silver.wide = silver.raw[,colSums(silver.raw) > 0]

center = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
center[is.na(center)] <- 0
center.raw = center[,5:ncol(center)]
center.raw
center.wide = center.raw[,colSums(center.raw) > 0]

five.island = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(season == 'summer') %>% 
  pivot_wider(id_cols = c(sampleID, lake, doy, year), 
              names_from = 'taxon', 
              values_from = 'biomass')
five.island[is.na(five.island)] <- 0
five.island.raw = five.island[,5:ncol(five.island)]
five.island.raw
five.island.wide = five.island.raw[,colSums(five.island.raw) > 0]

### PERMANOVA of zooplankton communities by year # =============================
# Blue # 
blue.taxa = decostand(as.matrix(blue.wide), method = 'hellinger') # Hellinger transform data # 
blue.dist = vegdist(blue.taxa, method = 'bray')
blue.group = blue[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(blue.dist~year, data = blue.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(blue.taxa), blue.group$year)

# Storm # 
storm.taxa = decostand(as.matrix(storm.wide), method = 'hellinger') # Hellinger transform data # 
storm.dist = vegdist(storm.taxa, method = 'bray')
storm.group = storm[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(storm.dist~year, data = storm.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(storm.taxa), storm.group$year)

# South Twin # 
south.twin.taxa = decostand(as.matrix(south.twin.wide), method = 'hellinger') # Hellinger transform data # 
south.twin.dist = vegdist(south.twin.taxa, method = 'bray')
south.twin.group = south.twin[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(south.twin.dist~year, data = south.twin.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(south.twin.taxa), south.twin.group$year)

# Center # 
center.taxa = decostand(as.matrix(center.wide), method = 'hellinger') # Hellinger transform data # 
center.dist = vegdist(center.taxa, method = 'bray')
center.group = center[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(center.dist~year, data = center.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(center.taxa), center.group$year)

# Five Island # *****
five.island.taxa = decostand(as.matrix(five.island.wide), method = 'hellinger') # Hellinger transform data # 
five.island.dist = vegdist(five.island.taxa, method = 'bray')
five.island.group = five.island[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(five.island.dist~year, data = five.island.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(five.island.taxa), five.island.group$year)

# North Twin # ****
north.twin.taxa = decostand(as.matrix(north.twin.wide), method = 'hellinger') # Hellinger transform data # 
north.twin.dist = vegdist(north.twin.taxa, method = 'bray')
north.twin.group = north.twin[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(north.twin.dist~year, data = north.twin.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(north.twin.taxa), north.twin.group$year)

# Silver # 
silver.taxa = decostand(as.matrix(silver.wide), method = 'hellinger') # Hellinger transform data # 
silver.dist = vegdist(silver.taxa, method = 'bray')
silver.group = silver[,1:4] %>% 
  mutate(year = as.factor(year)) 

# PERMANOVA # 
adonis2(silver.dist~year, data = silver.group, permutations = 999, method = 'bray')

# Pairwise 
adonis.pair(vegdist(silver.taxa), silver.group$year)
