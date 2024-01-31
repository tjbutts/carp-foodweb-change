## Size Spectra Analysis - Diagnostics ## 
## Pelagic Size Spectra ## 
# Calculate Pelagic Size Spectra # 

rm(list = ls())

# Allocate all organisms based on their Wet mass (ug) into log-binned size classes 
# Average the abundance (individuals*m^2) of organisms in each size bin for each DOY in each each size bin

# Slope of the abundance size spectra was calculated with maximum likelihood regression 
## Between the log of density and midpoint of each log2 size class 

# Used Cook's distance to evaluate and remove points with high leverage; points with a Cook's 
## Distance > 1 were discarded from the regression

# Install sizeSpectra package from Edwards et al. 2017 # 
if (!require(devtools)) install.packages('devtools')
library(devtools)

install.packages('sizeSpectra 1.0.0.0.tar.gz', repos = NULL)
library(sizeSpectra)

library(tidyverse)
#browseVignettes("sizeSpectra")

# Current Process # 
# 1. Calculate zooplantkon average weight 
# 2. Calculate zooplankton areal density 
# 3. Don't do anything with density data 

## Zooplankton Average Weight ##=======================
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files/Count Files")
setwd("J:/Box Sync/Carp Zooplankton Files/Count Files")

# Purpose: this script is designed to convert the length and count data for zooplankton samples in to biomass data, based on the allometric equations provided by the Downing Lab
# Script originally developed by Dr. Eric Moody - June 2017
# Updated by Dr. Grace Wilkinson - Feb 2021

# # Removed Trichocerca from list of taxa - only appeared in one lake and the calculations were getting messed up by it 

# STEP 1: Place the files in a working directory
# Place all of the count files into a folder, no other files should be in this folder
# DO NOT put the sample log into this folder, only count files
# Make sure the files are saved as .csv
# Make sure the file names are consistent (they SHOULD just be the 12 character sample ID)
# Set the working directory in R to this folder

# Have to remove the multiplication by file count so average biomass is not weighted by density - density is incorporated later 

# STEP 2: Set up empty vectors that will be filled using a for loop
filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
YEAR <- c()
DOY <- c()
TAXON <- c()
BIOMASS.UG <- c()
#BIOMASS.UG.SD <- c()


# STEP 3: Use a for loop to fill the vectors above based on the information in the count files
# Note that this is set up to work with the exact column headers and row order for the
# ZooCountTemplate file used in the lab - another format will not work with this script

############ NOTE: RUN THE ENTIRE FOR-LOOP AT ONCE, LINES 27-296
#DRY2WET = 0.2 
#DRY2WET.ROTI = 0.1
#DRY2WET.ASPLANCH = 0.039
i<-1
for (i in c(1:numfiles)){  
  filenames[i] <- paste(filenames[i],sep="")  
  file<- read.csv(filenames[i])
  
  #This is the list of taxa that we identify and the order that they are in for the rows
  #in the ZooCountTemplate; there are 44 taxa in this list
  Taxon<-c("Alona","Alonella","Bosmina","Camptocercus","Ceriodaphnia","Chydorus",
           "Daphnia","Daphnia.lumholtzi","Diaphanosoma","Graptoleberis","Leptodora",
           "Moina","Pleuroxus","Scapholeberis","Simocephalus","Calanoida","Cyclopoida",
           "Nauplii","Anuraeopsis","Ascomorpha","Asplanchna","Asplanchnopus",
           "Brachionus","Conochilus","Euchlanis","Filinia","Gastropus","Hexarthra",
           "Kellicottia","Keratella.cochlearis","Keratella.quadrata","Lecane",
           "Lepadella","Macrochaetus","Monostyla","Mytilina","Notholca","Platyias",
           "Ploesoma","Polyarthra","Pompholyx","Synchaeta","Testudinella",
           "Trichotria")
  
  #Calculate the biomass in micrograms per liter by converting the length and counts
  #for each taxa using the allometric equations below
  #Note that the equations are specific to each taxa, and reference the indexed list of 
  #taxon above (n=1-44)
  
  z<-c()
  y<-c()
  x<-c()
  BiomassSubsample<-c()
  #BiomassSD<-c()
  
  #Alona 
  BiomassSubsample[1]<-mean(as.numeric(
    (15.92*(file[1,5:length(names(file))]/1000)^3.84)),na.rm=TRUE)
  #BiomassSD[1]<-sd(as.numeric(
  #(15.92*(file[1,5:length(names(file))]/1000)^3.84)))*file$Count[1]
  
  #Alonella
  BiomassSubsample[2]<-mean(as.numeric(
    (15.92*(file[2,5:length(names(file))]/1000)^3.84)),na.rm=T)
  #BiomassSD[2]<-sd(as.numeric(
  #(15.92*(file[2,5:length(names(file))]/1000)^3.84)))*file$Count[2]
  
  #Bosmina
  BiomassSubsample[3]<-mean(as.numeric(
    (26.6*(file[3,5:length(names(file))]/1000)^3.13)),na.rm=T)
  #BiomassSD[3]<-sd(as.numeric(
  #(26.6*(file[3,5:length(names(file))]/1000)^3.13)))*file$Count[3]
  
  #Camptocercus
  BiomassSubsample[4]<-mean(as.numeric(
    (15.92*(file[4,5:length(names(file))]/1000)^3.84)),na.rm=T)
  # BiomassSD[4]<-sd(as.numeric(
  #(15.92*(file[4,5:length(names(file))]/1000)^3.84)))*file$Count[4]
  
  #Ceriodaphnia
  BiomassSubsample[5]<-mean(as.numeric(
    (1.76*10^-6)*(file[5,5:length(names(file))]^2.26)),na.rm=T)
  #BiomassSD[5]<-sd(as.numeric(
  #(1.76*10^-6)*(file[5,5:length(names(file))]^2.26)))*file$Count[5]
  
  #Chydorus
  BiomassSubsample[6]<-mean(as.numeric(
    (89.43*(file[6,5:length(names(file))]/1000)^3.03)),na.rm=T)
  #BiomassSD[6]<-sd(as.numeric(
  #(89.43*(file[6,5:length(names(file))]/1000)^3.03)))*file$Count[6]
  
  #Daphnia
  BiomassSubsample[7]<-mean(as.numeric(
    (1.5*10^-8)*(file[7,5:length(names(file))]^2.84)),na.rm=T)
  # BiomassSD[7]<-sd(as.numeric(
  #(1.5*10^-8)*(file[7,5:length(names(file))]^2.84)))*file$Count[7]
  
  #Daphnia.lumholtzi
  BiomassSubsample[8]<-mean(as.numeric(
    (1.5*10^-8)*(file[8,5:length(names(file))]^2.84)),na.rm=T)
  #BiomassSD[8]<-sd(as.numeric(
  #(1.5*10^-8)*(file[8,5:length(names(file))]^2.84)))*file$Count[8]
  
  #Diaphanosoma
  BiomassSubsample[9]<-mean(as.numeric(
    (1.76*10^-6)*(file[9,5:length(names(file))]^2.11)),na.rm=T)
  # BiomassSD[9]<-sd(as.numeric(
  #(1.76*10^-6)*(file[9,5:length(names(file))]^2.11)))*file$Count[9]
  
  #Graptoleberis
  BiomassSubsample[10]<-mean(as.numeric(
    (15.92*(file[10,5:length(names(file))]/1000)^3.84)),na.rm=T)
  #BiomassSD[10]<-sd(as.numeric(
  # (15.92*(file[10,5:length(names(file))]/1000)^3.84)))*file$Count[10]
  
  #Leptodora
  BiomassSubsample[11]<-exp(mean(as.numeric(
    (-0.822+2.76*log(file[11,5:length(names(file))]/1000))),na.rm=T))
  # BiomassSD[11]<-exp(sd(as.numeric(
  #(-0.822+2.76*log(file[11,5:length(names(file))]/1000))))*file$Count[11])
  
  #Moina
  BiomassSubsample[12]<-mean(as.numeric(
    (6.61*(file[12,5:length(names(file))]/1000)^2.57)),na.rm=T)
  #BiomassSD[12]<-sd(as.numeric(
  # (6.61*(file[12,5:length(names(file))]/1000)^2.57)))*file$Count[12]
  
  #Pleuroxus
  BiomassSubsample[13]<-mean(as.numeric(
    (35.6*(file[13,5:length(names(file))]/1000)^4.03)),na.rm=T)
  #BiomassSD[13]<-sd(as.numeric(
  #(35.6*(file[13,5:length(names(file))]/1000)^4.03)))*file$Count[13]
  
  #Scapholeberis
  BiomassSubsample[14]<-mean(as.numeric(
    (8.9*10^-8)*(file[14,5:length(names(file))]^2.7)),na.rm=T)
  #BiomassSD[14]<-sd(as.numeric(
  #(8.9*10^-8)*(file[14,5:length(names(file))]^2.7)),na.rm=T)*file$Count[14]
  
  #Simocephalus
  BiomassSubsample[15]<-mean(as.numeric(
    (7.43*(file[15,5:length(names(file))]/1000)^3.28)),na.rm=T)
  #BiomassSD[15]<-sd(as.numeric(
  #(7.43*(file[15,5:length(names(file))]/1000)^3.28)))*file$Count[15]
  
  #Calanoida
  BiomassSubsample[16]<-mean(as.numeric(
    (7.9*10^-7)*(file[16,5:length(names(file))]^2.33)),na.rm=T)
  #BiomassSD[16]<-sd(as.numeric(
  #(7.9*10^-7)*(file[16,5:length(names(file))]^2.33)))*file$Count[16]
  
  #Cyclopoida
  BiomassSubsample[17]<-mean(as.numeric(
    (1.1*10^-7)*(file[17,5:length(names(file))]^2.59)),na.rm=T)
  # BiomassSD[17]<-sd(as.numeric(
  # (1.1*10^-7)*(file[17,5:length(names(file))]^2.59)))*file$Count[17]
  
  #Nauplii
  BiomassSubsample[18]<-mean(as.numeric(
    (1.1*10^-5)*(file[18,5:length(names(file))]^1.89)),na.rm=T)
  #BiomassSD[18]<-sd(as.numeric(
  #(1.1*10^-5)*(file[18,5:length(names(file))]^1.89))*file$Count[18]
  
  # ROTIFERS
  
  #Anuraeopsis
  BiomassSubsample[19]<-((0.1*(mean(as.numeric(
    (0.03*(file[19,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Ascomorpha
  BiomassSubsample[20]<-((0.1*(mean(as.numeric(
    (0.12*(file[20,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Asplanchna
  BiomassSubsample[21]<-((0.039*(mean(as.numeric(
    (0.23*(file[21,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Asplanchnopus
  BiomassSubsample[22]<-((0.039*(mean(as.numeric(
    (0.23*(file[22,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Brachionus
  BiomassSubsample[23]<-((0.1*(mean(as.numeric(
    (0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Conochilus
  #NOTE: two measurements for this genus, hence the shift in indexing
  BiomassSubsample[24]<-((sum((0.16*file[24,5:length(names(file))]/1000*
                                 (file[25,5:length(names(file))]/1000)^2)/1000,na.rm=T))*10^6)
  
  #Euchlanis
  BiomassSubsample[25]<-((0.1*(mean(as.numeric(
    (0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Filinia
  BiomassSubsample[26]<-((0.1*(mean(as.numeric(
    (0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.01*(mean(as.numeric((0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Gastropus
  BiomassSubsample[27]<-((0.1*(mean(as.numeric(
    (0.2*(file[28,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Hexarthra
  BiomassSubsample[28]<-((0.1*(mean(as.numeric(
    (0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.33*(mean(as.numeric((0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Kellicotia
  BiomassSubsample[29]<-((0.1*(mean(as.numeric(
    (0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+ 
      0.015*(mean(as.numeric((0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Keratella.cochlearis
  BiomassSubsample[30]<-((0.1*(mean(as.numeric(
    (0.02*(file[31,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Keratella.quadrata
  BiomassSubsample[31]<-((0.1*(mean(as.numeric(
    (0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Lecane
  BiomassSubsample[32]<-((0.1*(mean(as.numeric(
    (0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Lepadella
  BiomassSubsample[33]<-((0.1*(mean(as.numeric(
    ((file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.05*(mean(as.numeric((0.1*(file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Macrochaetus
  BiomassSubsample[34]<-((0.1*(mean(as.numeric(
    (0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Monostyla
  BiomassSubsample[35]<-((0.1*(mean(as.numeric(
    (0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Mytilina
  a<-as.numeric(file[37,5:length(names(file))])/1000
  b<-as.numeric(file[38,5:length(names(file))])/1000
  c<-((0.52*a*b^2)+(0.6*0.52*a*b^2))/1000
  BiomassSubsample[36]<-(mean(as.numeric(c),na.rm=T)*10^6)
  
  #Notholca
  BiomassSubsample[37]<-((0.1*(mean(as.numeric(
    (0.035*(file[39,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Platyias
  BiomassSubsample[38]<-(((mean(as.numeric(
    (0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +(mean(as.numeric((0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Ploesoma
  BiomassSubsample[39]<-((0.1*(mean(as.numeric(
    (0.23*(file[41,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Polyarthra
  BiomassSubsample[40]<-((0.1*(mean(as.numeric(
    (0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Pompholyx
  BiomassSubsample[41]<-((0.1*(mean(as.numeric(
    (0.15*(file[43,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Synchaeta
  BiomassSubsample[42]<-((0.1*(mean(as.numeric(
    (0.1*(file[44,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Testudinella
  BiomassSubsample[43]<-((0.1*(mean(as.numeric(
    (0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Trichotria
  BiomassSubsample[44]<-(((mean(as.numeric(
    (0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
  # SAMPLE IDS
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., A19114204001)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,9),44)) #44 is the number of taxa
  
  #If the file names are saved as the 9-character sampleID (e.g., A19114204) use this:
  # SampleID<-(rep(substr(filenames[i],1,9),45)) #45 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),44)) #lake number
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),44)) #DOY is the 7-9th spot
  
  #Extract the 2-digit year from the sampleID which is the file name 
  year <- (rep(substr(filenames[i],2,3),44)) #Year is the 2-3rd spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  YEAR <- append(YEAR, year)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  BIOMASS.UG <- append(BIOMASS.UG, BiomassSubsample)
  # BIOMASS.UG.SD <- append(BIOMASS.UG.SD, BiomassSD)
  
}

# STEP 7: MAKE A USEFUL OUTPUT
library(tidyverse)

# Start by making a data frame of the useful info from above
Zoop.carp<-data.frame(SAMPLE.ID, LAKE.NO, YEAR,  DOY, TAXON, BIOMASS.UG)
Zoop.carp

# Replace NAs with 0s 
Zoop.carp[is.na(Zoop.carp)] <- 0
Zoop.carp = as_tibble(Zoop.carp)
Zoop.carp
Zoop.carp$YEAR[Zoop.carp$YEAR == 18] <- 2018
Zoop.carp$YEAR[Zoop.carp$YEAR == 19] <- 2019
Zoop.carp$YEAR[Zoop.carp$YEAR == 20] <- 2020
Zoop.carp

## Add Season Column ## 
## 2018: Spring = 060 - 181; Summer = 182 - 273
## 2019: Spring = 060 - 181; Summer = 182 - 273
## 2020: Spring = 061 - 182; Summer = 183 - 274

# Example Tidyverse Code to make the zoop data wide and summarize by GROUP
carp.zoop.biomass = Zoop.carp %>%
  rename(sampleID = SAMPLE.ID,
         lake = LAKE.NO, 
         taxon = TAXON, 
         doy = DOY, 
         year = YEAR,
         avg_biomass_ug = BIOMASS.UG) %>% 
  mutate(year = as.factor(year), 
         doy = as.numeric(doy),
         lake = as.factor(lake)) %>% 
  mutate(season = case_when(.$year == '2018' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2018' & .$doy >181 ~ 'summer', 
                            .$year == '2019' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2019' & .$doy >181 ~ 'summer',
                            .$year == '2020' & .$doy <183 & .$doy >59 ~ 'spring', 
                            .$year == '2020' & .$doy >182 ~ 'summer')) %>% 
  mutate(season = as.factor(season)) %>%
  filter(avg_biomass_ug != 0)

## Zooplankton Biomass Data Frame ##===========================
carp.zoop.biomass # 771 data points

## Add 2020 ALM Biomass Data ##==========================
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files/ALM 2020 Zoops")
setwd("J:/Box Sync/Carp Zooplankton Files/ALM 2020 Zoops")

# Purpose: this script is designed to convert the length and count data for zooplankton samples in to biomass data, based on the allometric equations provided by the Downing Lab
# Script originally developed by Dr. Eric Moody - June 2017
# Updated by Dr. Grace Wilkinson - Feb 2021

# # Removed Trichocerca from list of taxa - only appeared in one lake and the calculations were getting messed up by it 

# STEP 1: Place the files in a working directory
# Place all of the count files into a folder, no other files should be in this folder
# DO NOT put the sample log into this folder, only count files
# Make sure the files are saved as .csv
# Make sure the file names are consistent (they SHOULD just be the 12 character sample ID)
# Set the working directory in R to this folder

# Have to remove the multiplication by file count so average biomass is not weighted by density - density is incorporated later 

# STEP 2: Set up empty vectors that will be filled using a for loop
filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
YEAR <- c()
DOY <- c()
TAXON <- c()
BIOMASS.UG <- c()
BIOMASS.UG.SD <- c()


# STEP 3: Use a for loop to fill the vectors above based on the information in the count files
# Note that this is set up to work with the exact column headers and row order for the
# ZooCountTemplate file used in the lab - another format will not work with this script

############ NOTE: RUN THE ENTIRE FOR-LOOP AT ONCE, LINES 27-296
#DRY2WET = 0.2 
#DRY2WET.ROTI = 0.1
#DRY2WET.ASPLANCH = 0.039
i<-1
for (i in c(1:numfiles)){  
  filenames[i] <- paste(filenames[i],sep="")  
  file<- read.csv(filenames[i])
  
  #This is the list of taxa that we identify and the order that they are in for the rows
  #in the ZooCountTemplate; there are 44 taxa in this list
  Taxon<-c("Alona","Alonella","Bosmina","Camptocercus","Ceriodaphnia","Chydorus",
           "Daphnia","Daphnia.lumholtzi","Diaphanosoma","Graptoleberis","Leptodora",
           "Moina","Pleuroxus","Scapholeberis","Simocephalus","Calanoida","Cyclopoida",
           "Nauplii","Anuraeopsis","Ascomorpha","Asplanchna","Asplanchnopus",
           "Brachionus","Conochilus","Euchlanis","Filinia","Gastropus","Hexarthra",
           "Kellicottia","Keratella.cochlearis","Keratella.quadrata","Lecane",
           "Lepadella","Macrochaetus","Monostyla","Mytilina","Notholca","Platyias",
           "Ploesoma","Polyarthra","Pompholyx","Synchaeta","Testudinella",
           "Trichotria")
  
  #Calculate the biomass in micrograms per liter by converting the length and counts
  #for each taxa using the allometric equations below
  #Note that the equations are specific to each taxa, and reference the indexed list of 
  #taxon above (n=1-44)
  
  z<-c()
  y<-c()
  x<-c()
  BiomassSubsample<-c()
  #BiomassSD<-c()
  
  #Alona 
  BiomassSubsample[1]<-mean(as.numeric(
    (15.92*(file[1,5:length(names(file))]/1000)^3.84)),na.rm=TRUE)
  #BiomassSD[1]<-sd(as.numeric(
  #(15.92*(file[1,5:length(names(file))]/1000)^3.84)))*file$Count[1]
  
  #Alonella
  BiomassSubsample[2]<-mean(as.numeric(
    (15.92*(file[2,5:length(names(file))]/1000)^3.84)),na.rm=T)
  #BiomassSD[2]<-sd(as.numeric(
  #(15.92*(file[2,5:length(names(file))]/1000)^3.84)))*file$Count[2]
  
  #Bosmina
  BiomassSubsample[3]<-mean(as.numeric(
    (26.6*(file[3,5:length(names(file))]/1000)^3.13)),na.rm=T)
  #BiomassSD[3]<-sd(as.numeric(
  #(26.6*(file[3,5:length(names(file))]/1000)^3.13)))*file$Count[3]
  
  #Camptocercus
  BiomassSubsample[4]<-mean(as.numeric(
    (15.92*(file[4,5:length(names(file))]/1000)^3.84)),na.rm=T)
  # BiomassSD[4]<-sd(as.numeric(
  #(15.92*(file[4,5:length(names(file))]/1000)^3.84)))*file$Count[4]
  
  #Ceriodaphnia
  BiomassSubsample[5]<-mean(as.numeric(
    (1.76*10^-6)*(file[5,5:length(names(file))]^2.26)),na.rm=T)
  #BiomassSD[5]<-sd(as.numeric(
  #(1.76*10^-6)*(file[5,5:length(names(file))]^2.26)))*file$Count[5]
  
  #Chydorus
  BiomassSubsample[6]<-mean(as.numeric(
    (89.43*(file[6,5:length(names(file))]/1000)^3.03)),na.rm=T)
  #BiomassSD[6]<-sd(as.numeric(
  #(89.43*(file[6,5:length(names(file))]/1000)^3.03)))*file$Count[6]
  
  #Daphnia
  BiomassSubsample[7]<-mean(as.numeric(
    (1.5*10^-8)*(file[7,5:length(names(file))]^2.84)),na.rm=T)
  # BiomassSD[7]<-sd(as.numeric(
  #(1.5*10^-8)*(file[7,5:length(names(file))]^2.84)))*file$Count[7]
  
  #Daphnia.lumholtzi
  BiomassSubsample[8]<-mean(as.numeric(
    (1.5*10^-8)*(file[8,5:length(names(file))]^2.84)),na.rm=T)
  #BiomassSD[8]<-sd(as.numeric(
  #(1.5*10^-8)*(file[8,5:length(names(file))]^2.84)))*file$Count[8]
  
  #Diaphanosoma
  BiomassSubsample[9]<-mean(as.numeric(
    (1.76*10^-6)*(file[9,5:length(names(file))]^2.11)),na.rm=T)
  # BiomassSD[9]<-sd(as.numeric(
  #(1.76*10^-6)*(file[9,5:length(names(file))]^2.11)))*file$Count[9]
  
  #Graptoleberis
  BiomassSubsample[10]<-mean(as.numeric(
    (15.92*(file[10,5:length(names(file))]/1000)^3.84)),na.rm=T)
  #BiomassSD[10]<-sd(as.numeric(
  # (15.92*(file[10,5:length(names(file))]/1000)^3.84)))*file$Count[10]
  
  #Leptodora
  BiomassSubsample[11]<-exp(mean(as.numeric(
    (-0.822+2.76*log(file[11,5:length(names(file))]/1000))),na.rm=T))
  # BiomassSD[11]<-exp(sd(as.numeric(
  #(-0.822+2.76*log(file[11,5:length(names(file))]/1000))))*file$Count[11])
  
  #Moina
  BiomassSubsample[12]<-mean(as.numeric(
    (6.61*(file[12,5:length(names(file))]/1000)^2.57)),na.rm=T)
  #BiomassSD[12]<-sd(as.numeric(
  # (6.61*(file[12,5:length(names(file))]/1000)^2.57)))*file$Count[12]
  
  #Pleuroxus
  BiomassSubsample[13]<-mean(as.numeric(
    (35.6*(file[13,5:length(names(file))]/1000)^4.03)),na.rm=T)
  #BiomassSD[13]<-sd(as.numeric(
  #(35.6*(file[13,5:length(names(file))]/1000)^4.03)))*file$Count[13]
  
  #Scapholeberis
  BiomassSubsample[14]<-mean(as.numeric(
    (8.9*10^-8)*(file[14,5:length(names(file))]^2.7)),na.rm=T)
  #BiomassSD[14]<-sd(as.numeric(
  #(8.9*10^-8)*(file[14,5:length(names(file))]^2.7)),na.rm=T)*file$Count[14]
  
  #Simocephalus
  BiomassSubsample[15]<-mean(as.numeric(
    (7.43*(file[15,5:length(names(file))]/1000)^3.28)),na.rm=T)
  #BiomassSD[15]<-sd(as.numeric(
  #(7.43*(file[15,5:length(names(file))]/1000)^3.28)))*file$Count[15]
  
  #Calanoida
  BiomassSubsample[16]<-mean(as.numeric(
    (7.9*10^-7)*(file[16,5:length(names(file))]^2.33)),na.rm=T)
  #BiomassSD[16]<-sd(as.numeric(
  #(7.9*10^-7)*(file[16,5:length(names(file))]^2.33)))*file$Count[16]
  
  #Cyclopoida
  BiomassSubsample[17]<-mean(as.numeric(
    (1.1*10^-7)*(file[17,5:length(names(file))]^2.59)),na.rm=T)
  # BiomassSD[17]<-sd(as.numeric(
  # (1.1*10^-7)*(file[17,5:length(names(file))]^2.59)))*file$Count[17]
  
  #Nauplii
  BiomassSubsample[18]<-mean(as.numeric(
    (1.1*10^-5)*(file[18,5:length(names(file))]^1.89)),na.rm=T)
  #BiomassSD[18]<-sd(as.numeric(
  #(1.1*10^-5)*(file[18,5:length(names(file))]^1.89))*file$Count[18]
  
  # ROTIFERS
  
  #Anuraeopsis
  BiomassSubsample[19]<-((0.1*(mean(as.numeric(
    (0.03*(file[19,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Ascomorpha
  BiomassSubsample[20]<-((0.1*(mean(as.numeric(
    (0.12*(file[20,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Asplanchna
  BiomassSubsample[21]<-((0.039*(mean(as.numeric(
    (0.23*(file[21,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Asplanchnopus
  BiomassSubsample[22]<-((0.039*(mean(as.numeric(
    (0.23*(file[22,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Brachionus
  BiomassSubsample[23]<-((0.1*(mean(as.numeric(
    (0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Conochilus
  #NOTE: two measurements for this genus, hence the shift in indexing
  BiomassSubsample[24]<-((sum((0.16*file[24,5:length(names(file))]/1000*
                                 (file[25,5:length(names(file))]/1000)^2)/1000,na.rm=T))*10^6)
  
  #Euchlanis
  BiomassSubsample[25]<-((0.1*(mean(as.numeric(
    (0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Filinia
  BiomassSubsample[26]<-((0.1*(mean(as.numeric(
    (0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.01*(mean(as.numeric((0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Gastropus
  BiomassSubsample[27]<-((0.1*(mean(as.numeric(
    (0.2*(file[28,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Hexarthra
  BiomassSubsample[28]<-((0.1*(mean(as.numeric(
    (0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.33*(mean(as.numeric((0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Kellicotia
  BiomassSubsample[29]<-((0.1*(mean(as.numeric(
    (0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+ 
      0.015*(mean(as.numeric((0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Keratella.cochlearis
  BiomassSubsample[30]<-((0.1*(mean(as.numeric(
    (0.02*(file[31,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Keratella.quadrata
  BiomassSubsample[31]<-((0.1*(mean(as.numeric(
    (0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Lecane
  BiomassSubsample[32]<-((0.1*(mean(as.numeric(
    (0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Lepadella
  BiomassSubsample[33]<-((0.1*(mean(as.numeric(
    ((file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.05*(mean(as.numeric((0.1*(file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Macrochaetus
  BiomassSubsample[34]<-((0.1*(mean(as.numeric(
    (0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Monostyla
  BiomassSubsample[35]<-((0.1*(mean(as.numeric(
    (0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Mytilina
  a<-as.numeric(file[37,5:length(names(file))])/1000
  b<-as.numeric(file[38,5:length(names(file))])/1000
  c<-((0.52*a*b^2)+(0.6*0.52*a*b^2))/1000
  BiomassSubsample[36]<-(mean(as.numeric(c),na.rm=T)*10^6)
  
  #Notholca
  BiomassSubsample[37]<-((0.1*(mean(as.numeric(
    (0.035*(file[39,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Platyias
  BiomassSubsample[38]<-(((mean(as.numeric(
    (0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +(mean(as.numeric((0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Ploesoma
  BiomassSubsample[39]<-((0.1*(mean(as.numeric(
    (0.23*(file[41,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Polyarthra
  BiomassSubsample[40]<-((0.1*(mean(as.numeric(
    (0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Pompholyx
  BiomassSubsample[41]<-((0.1*(mean(as.numeric(
    (0.15*(file[43,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Synchaeta
  BiomassSubsample[42]<-((0.1*(mean(as.numeric(
    (0.1*(file[44,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Testudinella
  BiomassSubsample[43]<-((0.1*(mean(as.numeric(
    (0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Trichotria
  BiomassSubsample[44]<-(((mean(as.numeric(
    (0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
  # SAMPLE IDS
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., A19114204001)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,9),44)) #44 is the number of taxa
  
  #If the file names are saved as the 9-character sampleID (e.g., A19114204) use this:
  # SampleID<-(rep(substr(filenames[i],1,9),45)) #45 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),44)) #lake number
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),44)) #DOY is the 7-9th spot
  
  #Extract the 2-digit year from the sampleID which is the file name 
  year <- (rep(substr(filenames[i],2,3),44)) #Year is the 2-3rd spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  YEAR <- append(YEAR, year)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  BIOMASS.UG <- append(BIOMASS.UG, BiomassSubsample)
  # BIOMASS.UG.SD <- append(BIOMASS.UG.SD, BiomassSD)
  
}

# STEP 7: MAKE A USEFUL OUTPUT
library(tidyverse)

# Start by making a data frame of the useful info from above
a.Zoop.carp<-data.frame(SAMPLE.ID, LAKE.NO, YEAR,  DOY, TAXON, BIOMASS.UG)
a.Zoop.carp

# Replace NAs with 0s 
a.Zoop.carp[is.na(a.Zoop.carp)] <- 0
a.Zoop.carp = as_tibble(a.Zoop.carp)
a.Zoop.carp
a.Zoop.carp$YEAR[a.Zoop.carp$YEAR == 18] <- 2018
a.Zoop.carp$YEAR[a.Zoop.carp$YEAR == 19] <- 2019
a.Zoop.carp$YEAR[a.Zoop.carp$YEAR == 20] <- 2020
a.Zoop.carp

## Add Season Column ## 
## 2018: Spring = 060 - 181; Summer = 182 - 273
## 2019: Spring = 060 - 181; Summer = 182 - 273
## 2020: Spring = 061 - 182; Summer = 183 - 274

# Example Tidyverse Code to make the zoop data wide and summarize by GROUP
a.carp.zoop.biomass = a.Zoop.carp %>%
  rename(sampleID = SAMPLE.ID,
         lake = LAKE.NO, 
         taxon = TAXON, 
         doy = DOY, 
         year = YEAR,
         avg_biomass_ug = BIOMASS.UG) %>% 
  mutate(year = as.factor(year), 
         doy = as.numeric(doy),
         lake = as.factor(lake)) %>% 
  mutate(season = case_when(.$year == '2018' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2018' & .$doy >181 ~ 'summer', 
                            .$year == '2019' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2019' & .$doy >181 ~ 'summer',
                            .$year == '2020' & .$doy <183 & .$doy >59 ~ 'spring', 
                            .$year == '2020' & .$doy >182 ~ 'summer')) %>% 
  mutate(season = as.factor(season)) %>%
  filter(avg_biomass_ug != 0)

## Missing biomass data for two measured conochilus individuals 
## P19090198 - Conochilus 
## Added the necessary width measurement to fix ## 
a.carp.zoop.biomass

## Create Zooplankton Areal Density Data Frame ##===================
# Purpose: this script is designed to convert the length and count data for zooplankton samples in to biomass data, based on the allometric equations provided by the Downing Lab
# Script originally developed by Dr. Eric Moody - June 2017
# Updated by Dr. Grace Wilkinson - Feb 2021 
# Modified to collect areal density information on zooplankton 


# STEP 1: Place the files in a working directory
# Place all of the count files into a folder, no other files should be in this folder
# DO NOT put the sample log into this folder, only count files
# Make sure the files are saved as .csv
# Make sure the file names are consistent (they SHOULD just be the 12 character sample ID)
# Set the working directory in R to this folder

setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files/Count Files")
setwd("J:/Box Sync/Carp Zooplankton Files/Count Files")

# STEP 2: Set up empty vectors that will be filled using a for loop
filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
DOY <- c()
TAXON <- c()
COUNT <- c()
YEAR <- c()

# STEP 3: Use a for loop to fill the vectors above based on the information in the count files
# Note that this is set up to work with the exact column headers and row order for the
# ZooCountTemplate file used in the lab - another format will not work with this script

############ NOTE: RUN THE ENTIRE FOR-LOOP AT ONCE, LINES 27-296

i<-1
for (i in c(1:numfiles)){  
  filenames[i] <- paste(filenames[i],sep="")  
  file<- read.csv(filenames[i])
  
  #This is the list of taxa that we identify and the order that they are in for the rows
  #in the ZooCountTemplate; there are 44 taxa in this list 
  
  Taxon<-c("Alona","Alonella","Bosmina","Camptocercus","Ceriodaphnia","Chydorus",
           "Daphnia","Daphnia.lumholtzi","Diaphanosoma","Graptoleberis","Leptodora",
           "Moina","Pleuroxus","Scapholeberis","Simocephalus","Calanoida","Cyclopoida",
           "Nauplii","Anuraeopsis","Ascomorpha","Asplanchna","Asplanchnopus",
           "Brachionus","Conochilus","Euchlanis","Filinia","Gastropus","Hexarthra",
           "Kellicottia","Keratella.cochlearis","Keratella.quadrata","Lecane",
           "Lepadella","Macrochaetus","Monostyla","Mytilina","Notholca","Platyias",
           "Ploesoma","Polyarthra","Pompholyx","Synchaeta","Testudinella",
           "Trichotria")
  
  #Calculate the density by extracting the counts of each species enumerated below 
  
  z<-c()
  y<-c()
  x<-c()
  DensitySubsample<-c()
  
  #Alona 
  DensitySubsample[1]<-file$Count[1]
  
  #Alonella
  DensitySubsample[2]<-file$Count[2]
  
  #Bosmina
  DensitySubsample[3]<-file$Count[3]
  
  #Camptocercus
  DensitySubsample[4]<-file$Count[4]
  
  #Ceriodaphnia
  DensitySubsample[5]<-file$Count[5]
  
  #Chydorus
  DensitySubsample[6]<-file$Count[6]
  
  #Daphnia
  DensitySubsample[7]<-file$Count[7]
  
  #Daphnia.lumholtzi
  DensitySubsample[8]<-file$Count[8]
  
  #Diaphanosoma
  DensitySubsample[9]<-file$Count[9]
  
  #Graptoleberis
  DensitySubsample[10]<-file$Count[10]
  
  #Leptodora
  DensitySubsample[11]<-file$Count[11]
  
  #Moina
  DensitySubsample[12]<-file$Count[12]
  
  #Pleuroxus
  DensitySubsample[13]<-file$Count[13]
  
  #Scapholeberis
  DensitySubsample[14]<-file$Count[14]
  
  #Simocephalus
  DensitySubsample[15]<-file$Count[15]
  
  #Calanoida
  DensitySubsample[16]<-file$Count[16]
  
  #Cyclopoida
  DensitySubsample[17]<-file$Count[17]
  
  #Nauplii
  DensitySubsample[18]<-file$Count[18]
  
  
  # ROTIFERS
  
  #Anuraeopsis
  DensitySubsample[19]<-file$Count[19]
  
  #Ascomorpha
  DensitySubsample[20]<-file$Count[20]
  
  #Asplanchna
  DensitySubsample[21]<-file$Count[21]
  
  #Asplanchnopus
  DensitySubsample[22]<-file$Count[22]
  
  #Brachionus
  DensitySubsample[23]<-file$Count[23]
  
  #Conochilus
  #NOTE: two measurements for this genus, hence the shift in indexing
  DensitySubsample[24]<-file$Count[24]
  
  #Euchlanis
  DensitySubsample[25]<-file$Count[26]
  
  #Filinia
  DensitySubsample[26]<-file$Count[27]
  
  #Gastropus
  DensitySubsample[27]<-file$Count[28]
  
  #Hexarthra
  DensitySubsample[28]<-file$Count[29]
  
  #Kellicotia
  DensitySubsample[29]<-file$Count[30]
  
  #Keratella.cochlearis
  DensitySubsample[30]<-file$Count[31]
  
  #Keratella.quadrata
  DensitySubsample[31]<-file$Count[32]
  
  #Lecane
  DensitySubsample[32]<-file$Count[33]
  
  #Lepadella
  DensitySubsample[33]<-file$Count[34]
  
  #Macrochaetus
  DensitySubsample[34]<-file$Count[35]
  
  #Monostyla
  DensitySubsample[35]<-file$Count[36]
  
  #Mytilina
  DensitySubsample[36]<-file$Count[36]
  
  #Notholca
  DensitySubsample[37]<-file$Count[39]
  
  #Platyias
  DensitySubsample[38]<-file$Count[40]
  
  #Ploesoma
  DensitySubsample[39]<-file$Count[41]
  
  #Polyarthra
  DensitySubsample[40]<-file$Count[42]
  
  #Pompholyx
  DensitySubsample[41]<-file$Count[43]
  
  #Synchaeta
  DensitySubsample[42]<-file$Count[44]
  
  #Testudinella
  DensitySubsample[43]<-file$Count[45]
  
  #Trichotria
  DensitySubsample[44]<-file$Count[48]
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
  # SAMPLE IDS 
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., A19114204001)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,9),44)) #44 is the number of taxa
  
  #If the file names are saved as the 9-character sampleID (e.g., A19114204) use this:
  # SampleID<-(rep(substr(filenames[i],1,9),45)) #45 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),44)) #lake number
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),44)) #DOY is the 7-9th spot
  
  #Extract the 2-digit year from the sampleID which is the file name 
  year <- (rep(substr(filenames[i],2,3),44)) #Year is the 2-3rd spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  YEAR <- append(YEAR, year)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  COUNT <- append(COUNT, DensitySubsample)
}

# STEP 7: MAKE A USEFUL OUTPUT
library(tidyverse)

# Start by making a data frame of the useful info from above
Zoop.carp.dens<-data.frame(SAMPLE.ID, LAKE.NO, YEAR,  DOY, TAXON, COUNT)
Zoop.carp.dens
# Replace NAs with 0s 
Zoop.carp.dens[is.na(Zoop.carp.dens)] <- 0
Zoop.carp.dens = as_tibble(Zoop.carp.dens)
Zoop.carp.dens
Zoop.carp.dens$YEAR[Zoop.carp.dens$YEAR == 18] <- 2018
Zoop.carp.dens$YEAR[Zoop.carp.dens$YEAR == 19] <- 2019
Zoop.carp.dens$YEAR[Zoop.carp.dens$YEAR == 20] <- 2020
Zoop.carp.dens$LAKE.NO[Zoop.carp.dens$LAKE.NO == '012'] <- '12'
Zoop.carp.dens$LAKE.NO[Zoop.carp.dens$LAKE.NO == '019'] <- '19'
Zoop.carp.dens$LAKE.NO[Zoop.carp.dens$LAKE.NO == '036'] <- '36' 
Zoop.carp.dens$LAKE.NO[Zoop.carp.dens$LAKE.NO == '090'] <- '90' 
Zoop.carp.dens

#12, 19, 36, 90

## Add Season Column ## 
## 2018: Spring = 060 - 181; Summer = 182 - 273
## 2019: Spring = 060 - 181; Summer = 182 - 273
## 2020: Spring = 061 - 182; Summer = 183 - 274

# Example Tidyverse Code to make the zoop data wide and summarize by GROUP
zoop.carp.density = Zoop.carp.dens %>%
  rename(sampleID = SAMPLE.ID,
         lake = LAKE.NO, 
         taxon = TAXON, 
         doy = DOY, 
         year = YEAR,
         count = COUNT) %>% 
  mutate(year = as.factor(year), 
         lake = as.factor(lake),
         doy = as.numeric(doy)) %>%
  mutate(season = case_when(.$year == '2018' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2018' & .$doy >181 ~ 'summer', 
                            .$year == '2019' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2019' & .$doy >181 ~ 'summer',
                            .$year == '2020' & .$doy <183 & .$doy >59 ~ 'spring', 
                            .$year == '2020' & .$doy >182 ~ 'summer')) %>%
  mutate(season = as.factor(season)) %>% 
  # Create a new column ("GROUP") that creates the broader taxonomic groups for analysis
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
                                          "Trichotria") ~ "Rotifer",
                           .$taxon %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$taxon %in% c("Calanoida") ~ "Calanoid",
                           .$taxon %in% c("Nauplii") ~ "Nauplii")) %>%
  filter(count != 0)
zoop.carp.density


# STEP 5: Read in the sample log
# In the parentheses below, put the full path to the sample log file
# A quick way to get this path is to right click on the file, go to "Properties", and copy the path -- NOTE THAT THE SLASHES IN THE PATH MUST BE "/"
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files")
setwd("J:/Box Sync/Carp Zooplankton Files")
zvol<-read.csv('CarpZoopLOG_missingsamplesremoved.csv') %>%
  rename(sampleID = SAMPLEID, 
         lake = LAKE,
         year = YEAR, 
         doy = DOY) %>% 
  mutate(year = as.factor(year), 
         lake = as.factor(lake)) %>% 
  as_tibble()
zvol
names(zvol) #Check that your column headers are correct and match the headers listed above

# STEP 4: SAMPLE LOG
# Create a sample log that has the following column headers -- COPIED EXACTLY:

# SAMPLEID	- the 12-character sample ID that is a perfect match to the file names
# SAMPLEVOLUME	- the volume of the total sample in milliliters
# VOLUMECOUNTED	- the volume of the subsample counted in milliliters
# TOW - the depth of the net tow for the zooplankton sample

# Note: there can be other columns, if desired, they will just be ignored in this script

# Each count file will be a row in this log, all of the columns must be filled in for each row
# Save the file as .csv in a DIFFERENT folder from the count files
# Do not include duplicate samples in the log unless they are being reported to DNR
# Each row must have a unique sample ID, so if duplicate samples were mis-named for the file, the file name will need to be changed to be unique


# JOIN VOLUME TO DENSITY DATA 
zoop.carp.density
carp_dens_join = left_join(zoop.carp.density, zvol, by = c('sampleID', 'doy', 'lake', 'year'))
carp_dens_join # Missing volume information in here 

# STEP 6: Correct the densities from Step 3 for volume of water towed and sample volume
zvol

WI.NET.AREA<-0.0113 #m^2 ##this is the current diameter of the nets we use##
nsample<-length(zvol$sampleID)
SSVOL<-c()
SVOL<-c()
TVOL<-c()
PROCDATE<-c()
PROCBY<-c()
for(i in c(1:nsample)){
  ssvol<-rep((zvol$VOLUMECOUNTED[i]/1000),44) #44 = number of taxa
  SSVOL<-append(SSVOL,ssvol)
  svol<-rep((zvol$SAMPLEVOLUME[i]/1000),44) #44 = number of taxa
  SVOL<-append(SVOL,svol)
  tvol<-rep(zvol$TOW[i], 44) #44 = number of taxa
  TVOL<-append(TVOL,tvol)
}
TVOLF<-(WI.NET.AREA*TVOL)*1000

density.indv.l<-c()
for(i in c(1:length(COUNT))){
  density.indv.l[i]<-(COUNT[i]*SVOL[i])/(SSVOL[i]*TVOLF[i])
}
density.indv.l[is.na(density.indv.l)] <- 0
density.indv.l = as.data.frame(density.indv.l) 
density.indv.l = filter(.data = density.indv.l, density.indv.l != 0)
density.indv.l

# Create a CSV file of the data frame -- NOTE: this will be in the working directory
# Will need to remove file from working directory before re-running the analysis
carp.zoop.density = carp_dens_join %>%
  select(sampleID, lake, year, doy, taxon, group, count, season, TOW) %>% 
  rename(tow = TOW) %>%
  mutate(density.indv.l = density.indv.l$density.indv.l) %>% 
  mutate(density.areal = density.indv.l*1000*tow) %>%  # Key info for this analysis L -> m^3 = *1000
  mutate(lake = as.factor(lake), 
         year = as.factor(year), 
         season = as.factor(season))
carp.zoop.density

## Zooplankton Areal Density Dataset ##===========================
carp.zoop.density # Make sure to use areal density 

# Add ALM 2020 Areal Density ##=========================
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files/ALM 2020 Zoops")
setwd("J:/Box Sync/Carp Zooplankton Files/ALM 2020 Zoops")

# STEP 2: Set up empty vectors that will be filled using a for loop
filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
DOY <- c()
TAXON <- c()
COUNT <- c()
YEAR <- c()

# STEP 3: Use a for loop to fill the vectors above based on the information in the count files
# Note that this is set up to work with the exact column headers and row order for the
# ZooCountTemplate file used in the lab - another format will not work with this script

############ NOTE: RUN THE ENTIRE FOR-LOOP AT ONCE, LINES 27-296

i<-1
for (i in c(1:numfiles)){  
  filenames[i] <- paste(filenames[i],sep="")  
  file<- read.csv(filenames[i])
  
  #This is the list of taxa that we identify and the order that they are in for the rows
  #in the ZooCountTemplate; there are 44 taxa in this list 
  
  Taxon<-c("Alona","Alonella","Bosmina","Camptocercus","Ceriodaphnia","Chydorus",
           "Daphnia","Daphnia.lumholtzi","Diaphanosoma","Graptoleberis","Leptodora",
           "Moina","Pleuroxus","Scapholeberis","Simocephalus","Calanoida","Cyclopoida",
           "Nauplii","Anuraeopsis","Ascomorpha","Asplanchna","Asplanchnopus",
           "Brachionus","Conochilus","Euchlanis","Filinia","Gastropus","Hexarthra",
           "Kellicottia","Keratella.cochlearis","Keratella.quadrata","Lecane",
           "Lepadella","Macrochaetus","Monostyla","Mytilina","Notholca","Platyias",
           "Ploesoma","Polyarthra","Pompholyx","Synchaeta","Testudinella",
           "Trichotria")
  
  #Calculate the density by extracting the counts of each species enumerated below 
  
  z<-c()
  y<-c()
  x<-c()
  DensitySubsample<-c()
  
  #Alona 
  DensitySubsample[1]<-file$Count[1]
  
  #Alonella
  DensitySubsample[2]<-file$Count[2]
  
  #Bosmina
  DensitySubsample[3]<-file$Count[3]
  
  #Camptocercus
  DensitySubsample[4]<-file$Count[4]
  
  #Ceriodaphnia
  DensitySubsample[5]<-file$Count[5]
  
  #Chydorus
  DensitySubsample[6]<-file$Count[6]
  
  #Daphnia
  DensitySubsample[7]<-file$Count[7]
  
  #Daphnia.lumholtzi
  DensitySubsample[8]<-file$Count[8]
  
  #Diaphanosoma
  DensitySubsample[9]<-file$Count[9]
  
  #Graptoleberis
  DensitySubsample[10]<-file$Count[10]
  
  #Leptodora
  DensitySubsample[11]<-file$Count[11]
  
  #Moina
  DensitySubsample[12]<-file$Count[12]
  
  #Pleuroxus
  DensitySubsample[13]<-file$Count[13]
  
  #Scapholeberis
  DensitySubsample[14]<-file$Count[14]
  
  #Simocephalus
  DensitySubsample[15]<-file$Count[15]
  
  #Calanoida
  DensitySubsample[16]<-file$Count[16]
  
  #Cyclopoida
  DensitySubsample[17]<-file$Count[17]
  
  #Nauplii
  DensitySubsample[18]<-file$Count[18]
  
  
  # ROTIFERS
  
  #Anuraeopsis
  DensitySubsample[19]<-file$Count[19]
  
  #Ascomorpha
  DensitySubsample[20]<-file$Count[20]
  
  #Asplanchna
  DensitySubsample[21]<-file$Count[21]
  
  #Asplanchnopus
  DensitySubsample[22]<-file$Count[22]
  
  #Brachionus
  DensitySubsample[23]<-file$Count[23]
  
  #Conochilus
  #NOTE: two measurements for this genus, hence the shift in indexing
  DensitySubsample[24]<-file$Count[24]
  
  #Euchlanis
  DensitySubsample[25]<-file$Count[26]
  
  #Filinia
  DensitySubsample[26]<-file$Count[27]
  
  #Gastropus
  DensitySubsample[27]<-file$Count[28]
  
  #Hexarthra
  DensitySubsample[28]<-file$Count[29]
  
  #Kellicotia
  DensitySubsample[29]<-file$Count[30]
  
  #Keratella.cochlearis
  DensitySubsample[30]<-file$Count[31]
  
  #Keratella.quadrata
  DensitySubsample[31]<-file$Count[32]
  
  #Lecane
  DensitySubsample[32]<-file$Count[33]
  
  #Lepadella
  DensitySubsample[33]<-file$Count[34]
  
  #Macrochaetus
  DensitySubsample[34]<-file$Count[35]
  
  #Monostyla
  DensitySubsample[35]<-file$Count[36]
  
  #Mytilina
  DensitySubsample[36]<-file$Count[36]
  
  #Notholca
  DensitySubsample[37]<-file$Count[39]
  
  #Platyias
  DensitySubsample[38]<-file$Count[40]
  
  #Ploesoma
  DensitySubsample[39]<-file$Count[41]
  
  #Polyarthra
  DensitySubsample[40]<-file$Count[42]
  
  #Pompholyx
  DensitySubsample[41]<-file$Count[43]
  
  #Synchaeta
  DensitySubsample[42]<-file$Count[44]
  
  #Testudinella
  DensitySubsample[43]<-file$Count[45]
  
  #Trichotria
  DensitySubsample[44]<-file$Count[48]
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
  # SAMPLE IDS 
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., A19114204001)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,9),44)) #44 is the number of taxa
  
  #If the file names are saved as the 9-character sampleID (e.g., A19114204) use this:
  # SampleID<-(rep(substr(filenames[i],1,9),45)) #45 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),44)) #lake number
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),44)) #DOY is the 7-9th spot
  
  #Extract the 2-digit year from the sampleID which is the file name 
  year <- (rep(substr(filenames[i],2,3),44)) #Year is the 2-3rd spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  YEAR <- append(YEAR, year)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  COUNT <- append(COUNT, DensitySubsample)
}

# STEP 7: MAKE A USEFUL OUTPUT
library(tidyverse)

# Start by making a data frame of the useful info from above
a.Zoop.carp.dens<-data.frame(SAMPLE.ID, LAKE.NO, YEAR,  DOY, TAXON, COUNT)
a.Zoop.carp.dens
# Replace NAs with 0s 
a.Zoop.carp.dens[is.na(a.Zoop.carp.dens)] <- 0
a.Zoop.carp.dens = as_tibble(a.Zoop.carp.dens)
a.Zoop.carp.dens
a.Zoop.carp.dens$YEAR[a.Zoop.carp.dens$YEAR == 18] <- 2018
a.Zoop.carp.dens$YEAR[a.Zoop.carp.dens$YEAR == 19] <- 2019
a.Zoop.carp.dens$YEAR[a.Zoop.carp.dens$YEAR == 20] <- 2020
a.Zoop.carp.dens$LAKE.NO[a.Zoop.carp.dens$LAKE.NO == '012'] <- '12'
a.Zoop.carp.dens$LAKE.NO[a.Zoop.carp.dens$LAKE.NO == '019'] <- '19'
a.Zoop.carp.dens$LAKE.NO[a.Zoop.carp.dens$LAKE.NO == '036'] <- '36' 
a.Zoop.carp.dens$LAKE.NO[a.Zoop.carp.dens$LAKE.NO == '090'] <- '90' 
a.Zoop.carp.dens

#12, 19, 36, 90

## Add Season Column ## 
## 2018: Spring = 060 - 181; Summer = 182 - 273
## 2019: Spring = 060 - 181; Summer = 182 - 273
## 2020: Spring = 061 - 182; Summer = 183 - 274

# Example Tidyverse Code to make the zoop data wide and summarize by GROUP
a.zoop.carp.density = a.Zoop.carp.dens %>%
  rename(sampleID = SAMPLE.ID,
         lake = LAKE.NO, 
         taxon = TAXON, 
         doy = DOY, 
         year = YEAR,
         count = COUNT) %>% 
  mutate(year = as.factor(year), 
         lake = as.factor(lake),
         doy = as.numeric(doy)) %>%
  mutate(season = case_when(.$year == '2018' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2018' & .$doy >181 ~ 'summer', 
                            .$year == '2019' & .$doy <182 & .$doy >59 ~ 'spring', 
                            .$year == '2019' & .$doy >181 ~ 'summer',
                            .$year == '2020' & .$doy <183 & .$doy >59 ~ 'spring', 
                            .$year == '2020' & .$doy >182 ~ 'summer')) %>%
  mutate(season = as.factor(season)) %>% 
  # Create a new column ("GROUP") that creates the broader taxonomic groups for analysis
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
                                          "Trichotria") ~ "Rotifer",
                           .$taxon %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$taxon %in% c("Calanoida") ~ "Calanoid",
                           .$taxon %in% c("Nauplii") ~ "Nauplii")) %>%
  filter(count != 0)
a.zoop.carp.density


# STEP 5: Read in the sample log
# In the parentheses below, put the full path to the sample log file
# A quick way to get this path is to right click on the file, go to "Properties", and copy the path -- NOTE THAT THE SLASHES IN THE PATH MUST BE "/"
setwd("J:/Box Sync/Carp Zooplankton Files")
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files")
a.zvol<-read_csv("CarpZoopLOG.csv") %>%
  rename(sampleID = SAMPLEID, 
         lake = LAKE,
         year = YEAR, 
         doy = DOY) %>% 
  mutate(year = as.factor(year), 
         lake = as.factor(lake)) %>% 
  as_tibble()
a.zvol
names(zvol) #Check that your column headers are correct and match the headers listed above

# STEP 4: SAMPLE LOG
# Create a sample log that has the following column headers -- COPIED EXACTLY:

# SAMPLEID	- the 12-character sample ID that is a perfect match to the file names
# SAMPLEVOLUME	- the volume of the total sample in milliliters
# VOLUMECOUNTED	- the volume of the subsample counted in milliliters
# TOW - the depth of the net tow for the zooplankton sample

# Note: there can be other columns, if desired, they will just be ignored in this script

# Each count file will be a row in this log, all of the columns must be filled in for each row
# Save the file as .csv in a DIFFERENT folder from the count files
# Do not include duplicate samples in the log unless they are being reported to DNR
# Each row must have a unique sample ID, so if duplicate samples were mis-named for the file, the file name will need to be changed to be unique


# JOIN VOLUME TO DENSITY DATA 
a.zoop.carp.density
a.carp_dens_join = left_join(a.zoop.carp.density, a.zvol, by = c('sampleID', 'doy', 'lake', 'year'))
a.carp_dens_join # Missing volume information in here 

# STEP 6: Correct the densities from Step 3 for volume of water towed and sample volume
a.zvol

WI.NET.AREA<-0.0113 #m^2 ##this is the current diameter of the nets we use##
nsample<-length(a.zvol$sampleID)
SSVOL<-c()
SVOL<-c()
TVOL<-c()
PROCDATE<-c()
PROCBY<-c()
for(i in c(1:nsample)){
  ssvol<-rep((a.zvol$VOLUMECOUNTED[i]/1000),44) #44 = number of taxa
  SSVOL<-append(SSVOL,ssvol)
  svol<-rep((a.zvol$SAMPLEVOLUME[i]/1000),44) #44 = number of taxa
  SVOL<-append(SVOL,svol)
  tvol<-rep(a.zvol$TOW[i], 44) #44 = number of taxa
  TVOL<-append(TVOL,tvol)
}
TVOLF<-(WI.NET.AREA*TVOL)*1000

density.indv.l<-c()
for(i in c(1:length(COUNT))){
  density.indv.l[i]<-(COUNT[i]*SVOL[i])/(SSVOL[i]*TVOLF[i])
}
density.indv.l[is.na(density.indv.l)] <- 0
density.indv.l = as.data.frame(density.indv.l) 
density.indv.l = filter(.data = density.indv.l, density.indv.l != 0)
density.indv.l

# Create a CSV file of the data frame -- NOTE: this will be in the working directory
# Will need to remove file from working directory before re-running the analysis
a.carp.zoop.density = a.carp_dens_join %>%
  select(sampleID, lake, year, doy, taxon, group, count, season, TOW) %>% 
  rename(tow = TOW) %>%
  mutate(density.indv.l = density.indv.l$density.indv.l) %>% 
  mutate(density.areal = density.indv.l*1000*tow) %>%  # Key info for this analysis L -> m^3 = *1000
  mutate(lake = as.factor(lake), 
         year = as.factor(year), 
         season = as.factor(season))
a.carp.zoop.density
## Put together size spectra data frame ## ============================
carp.zoop.density # Zooplankton Areal Density  
carp.zoop.biomass # Zooplankton Average Taxon Biomass (ug) 
a.carp.zoop.biomass
a.carp.zoop.density

# Combine datasets into output dataframe 
zp.dens1 = carp.zoop.density %>% 
  select(sampleID, lake, year, season, doy, taxon, group, count, density.areal) 
zp.dens1
zp.dens2 = a.carp.zoop.density %>% 
  select(sampleID, lake, year, season, doy, taxon, group, count, density.areal)
zp.dens2

zp.dens = rbind(zp.dens1, zp.dens2)
zp.dens

zp.biom1 = carp.zoop.biomass %>% 
  mutate(biomass_g = avg_biomass_ug*(10^-6)) %>% # Convert data to grams
  select(sampleID, lake, year, season, doy, taxon, biomass_g)
zp.biom1

zp.biom2 = a.carp.zoop.biomass %>% 
  mutate(biomass_g = avg_biomass_ug*(10^-6)) %>% # Convert data to grams 
  select(sampleID, lake, year, season, doy, taxon, biomass_g)

zp.biom = rbind(zp.biom1, zp.biom2)
zp.biom

output = left_join(zp.biom, zp.dens, by = c('sampleID', 
                                            'year', 'season',
                                            'doy','taxon')) %>%
  select(sampleID, lake.y, year, season, doy, taxon, group, biomass_g, count, density.areal) %>%
  rename(lake = lake.y) 
output

taxa = as.data.frame(c(output[output$group == 'Rotifer', 'biomass_g'], output[output$group == 'Rotifer', 'taxon'], 
                       output[output$group == 'Rotifer', 'count'], output[output$group == 'Rotifer', 'sampleID']))
head(taxa)



## VANILLA ANALYSIS ##=========================
output = left_join(zp.biom, zp.dens, by = c('sampleID', 
                                            'year', 'season',
                                            'doy','taxon')) %>%
  select(sampleID, lake.y, year, season, doy, taxon, group, biomass_g, count, density.areal) %>%
  rename(lake = lake.y) 
output

output.filt = output %>%
filter(count > 1) # Reduce the influence of singletons 

## Calculate the number of log2 bins, taken from Edwards et al. 2017 - assign bins
log2bins_butts = function(x = NULL, counts = NULL)
{
  if(!is.null(x) & !is.null(counts)) {
    stop("need only one of x or counts in log2bins") }
  if(is.null(x) & is.null(counts)) {
    stop("need x or counts in log2bins") }
  if(!is.null(x)) {
    if(!is.vector(x))stop("x not a vector in log2bins")
    if(anyNA(x)) stop("x contains NA's in log2bins")
    if(min(x) <= 0)stop("x needs to be >0 in log2bins")
  }
  if(!is.null(counts))  {
    if(dim(counts)[2] != 2)stop("counts needs two cols in log2bins")
    if(min(counts[,1]) < 0) {
      stop("x values in counts need to be >= 0 in log2bins") }
    if(min(counts[,2]) < 0) {
      stop("numbers in counts need to be >= 0 in log2bins") }
  }
  # As for LBNbiom.method(), could write code that would make
  #  use of the counts dataframe explicitly, but actually quite easy
  #  to just create the longer vector x (though may be slightly slower
  #  computationally), to save writing extensive new code.
  if(is.null(x))
  {x = rep(counts[,1], counts[,2]) }
  #
  binBreaks = 2^( floor(log2(min(x))) : ceiling(log2(max(x))) )
  
  indiv = data.frame(x)       # dataframe with one row for each individual
  indiv$binMid =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-length(binBreaks)] + 0.5*diff(binBreaks))
  indiv$binMin =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-length(binBreaks)])
  indiv$binMax =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-1])
  # indiv$binWidth =cut(x, breaks=binBreaks, right=FALSE,
  #    include.lowest=TRUE, labels = diff(binBreaks))
  # indiv = mutate(indiv, binWidth = binMax - binMin)
  # Above commands avoid any problems with bins with 0 counts.
  # Don't really need all of them, but include for completeness.
  indiv$binMid = as.numeric(as.character(indiv$binMid))
  indiv$binMin = as.numeric(as.character(indiv$binMin))
  indiv$binMax = as.numeric(as.character(indiv$binMax))
  # Now calculate biomass in each bin class:
  binVals = dplyr::summarise(dplyr::group_by(indiv, binMid),
                             binMin = unique(binMin),
                             binMax = unique(binMax),
                             binWidth = binMax - binMin,
                             binCount = length(x),
                             binCountNorm = binCount / binWidth,
                             binSum = sum(x),
                             binSumNorm = binSum / binWidth )
  # binWidth uses new columns binMax and binMin
  binVals = binVals[order(binVals$binMid),]   # order by binMid
  #
  if(dim(indiv)[1] < 10^6) {       # only save indiv if not too big
    y = list(indiv = indiv, binVals = binVals)
  } else
  {
    y = list(binVals = binVals)
  }
  return(y)
}

# Rewrote the base function removing the log10 bit that was causing errors, only need the log2 part anyway
log2bins.list = log2bins_butts(output.filt$biomass_g)
output.filt$BINMID <- as.factor(log2bins.list$indiv$binMid)
output.filt$BINMIN <- log2bins.list$indiv$binMin
output.filt$BINMAX <- log2bins.list$indiv$binMax
output.filt

### Plot Raw Data with Bins added ###===================
# Reference - no removal # Blue, South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

output.filt$lake = factor(output.filt$lake, levels = c('12','19', '36', '90', '105', '113', '406'))
output.filt = output.filt %>% arrange(lake)

# Replace lake number with lake name #
levels(output.filt$lake) = c(levels(output.filt$lake), 'Blue','Center', 
                               'Five.Island', 'North.Twin', 
                               'Silver', 'Storm', 'South.Twin')

output.plot = output.filt
output.plot$lake[output.plot$lake == '12'] <- 'Blue'
output.plot$lake[output.plot$lake == '19'] <- 'Center' 
output.plot$lake[output.plot$lake == '36'] <- 'Five.Island' 
output.plot$lake[output.plot$lake == '90'] <- 'North.Twin'
output.plot$lake[output.plot$lake == '105'] <- 'Silver'
output.plot$lake[output.plot$lake == '113'] <- 'Storm'
output.plot$lake[output.plot$lake == '406'] <- 'South.Twin'
output.plot$lake
output.plot$BINMID = as.numeric(as.character(output.plot$BINMID))

output.plot2 = output.plot 

rot.y = as.data.frame(c(output.plot2[output.plot2$group == 'Rotifer', 'density.areal'],output.plot2[output.plot2$group == 'Rotifer', 'taxon'] ))
rot.x = as.data.frame(c(output.plot2[output.plot2$group == 'Rotifer', 'BINMID'], output.plot2[output.plot2$group == 'Rotifer', 'taxon'] ))

windows(width = 5, height = 5)

colfunc = c('#01665e', '#80cdc1', '#c7eae5' ,'#f5f5f5', '#dfc27d', '#bf812d', '#8c510a')

plot(log2(rot.y[rot.y$taxon == 'Brachionus', 'density.areal'])~jitter(log2(rot.x[rot.x$taxon == 'Brachionus', 'BINMID']), 1), data = output.plot2, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = ref_col_18, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)
#, log2(1.397e-9), log2(2.794e-9), log2(5.588e-9), log2(1.118e-8), log2(2.24e-8), 
#log2(4.48e-08), log2(8.94e-08), log2(1.788e-7), log2(3.58e-7), log2(7.15e-7), log2(1.43e-6), 
#log2(2.86e-06), log2(5.72e-6), log2(1.14e-5),


points(log2(rot.y[rot.y$taxon == 'Keratella.cochlearis', 'density.areal'])~
         jitter(log2(rot.x[rot.x$taxon == 'Keratella.cochlearis', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[1], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(rot.y[rot.y$taxon == 'Polyarthra', 'density.areal'])~
         jitter(log2(rot.x[rot.x$taxon == 'Polyarthra', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[2], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(rot.y[rot.y$taxon == 'Filinia', 'density.areal'])~
         jitter(log2(rot.x[rot.x$taxon == 'Filinia', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[4], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(rot.y[rot.y$taxon == 'Asplanchna', 'density.areal'])~
         jitter(log2(rot.x[rot.x$taxon == 'Asplanchna', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[5], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(rot.y[rot.y$taxon == 'Pompholyx', 'density.areal'])~
         jitter(log2(rot.x[rot.x$taxon == 'Pompholyx', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'steelblue', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(rot.y[rot.y$taxon == 'Keratella.quadrata', 'density.areal'])~
         jitter(log2(rot.x[rot.x$taxon == 'Keratella.quadrata', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[6], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(rot.y[rot.y$taxon == 'Ascomorpha', 'density.areal'])~
         jitter(log2(rot.x[rot.x$taxon == 'Ascomorpha', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[7], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
rot.y2 = rot.y %>% 
  as_tibble() %>%
  filter(taxon != 'Ascomorpha' & taxon != 'Keratella.quadrata' & taxon !=  'Pompholyx' 
         & taxon !=  'Asplanchna' & taxon !=  'Filinia' & taxon !=  'Polyarthra' & taxon !=  
                    'Keratella.cochlearis' & taxon != 'Brachionus')
rot.y2

rot.x2 = rot.x %>%   
  filter(taxon != 'Ascomorpha' & taxon != 'Keratella.quadrata' & taxon !=  'Pompholyx' 
         & taxon !=  'Asplanchna' & taxon !=  'Filinia' & taxon !=  'Polyarthra' & taxon !=  
           'Keratella.cochlearis' & taxon != 'Brachionus')
rot.x2

points(log2(rot.y2$density.areal)~jitter(log2(rot.x2$BINMID), 1), 
       yaxt = 'n', xaxt = 'n', pch = 21, col = 'black', bg = ref_col_20, cex = 1.5, lwd = 2)

abline(v = log2(unique(output.plot2$BINMIN)))

output.plot2
mean = output.plot2 %>% 
  group_by(BINMID) %>%
  summarize(midpoint = mean(density.areal)) %>%
  ungroup()
mean

points(log2(midpoint)~log2(BINMID), data = mean, yaxt = 'n', xaxt = 'n', pch = 23, 
       col = 'black', bg = 'white', lwd = 2, cex = 2.5)

#output.plot2 = as.data.frame(output.plot2)
#points(log2(output.plot2[output.plot2$group == 'Cladocera', 'density.areal']) ~ 
       #  jitter(log2(output.plot2[output.plot2$group == 'Cladocera', 'BINMID']), 1), 
     #  yaxt = 'n', xaxt = 'n', pch = 21, col = colfunc[1], cex = 1.5, lwd = 2,
     #  xlab = '', ylab = '')

legend('bottomright', legend = c('K.cochlearis', 'Polyarthra', 'Brachionus', 'Filinia', 'Asplanchna', 'Pompholyx', 'K.Quadrata', 'Ascomorpha', 'Rare Rotifers' ,'Avg. Density'), 
       pch = c(rep(19, 9), 23), col = c(colfunc[1], colfunc[2], ref_col_18, colfunc[4], colfunc[5], 'steelblue', colfunc[6], colfunc[7], ref_col_20 , 'gray40'))
mtext(side=1, line =2, 'Log2(Individual Biomass)')
mtext(side=2, line =2, 'Log2(Areal Density)')
mtext(side=3, line=1, 'Rotifers Only', cex = 2, font =2)

## Do the same thing for different cladocera taxa ## ====================
clads = as.data.frame(output.plot2)

colfunc = c('#01665e', '#80cdc1', '#c7eae5' ,'#f5f5f5', '#dfc27d', '#bf812d', '#8c510a')

plot(log2(clads[clads$group == 'Bosmina', 'density.areal'])~
       jitter(log2(clads[clads$group == 'Bosmina', 'BINMID']), 1), data = clads, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = ref_col_18, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(clads[clads$group == 'Chydorid', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Chydorid', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[1], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Daphnia', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Daphnia', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[2], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Ceriodaphnia', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Ceriodaphnia', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[4], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Lg.Cladocera', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Lg.Cladocera', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[5], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Calanoid', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Calanoid', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'steelblue', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Cyclopoid', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[6], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Nauplii', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Nauplii', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = colfunc[7], cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
lightblack = rgb(0, 0, 0, max = 255, alpha = 100) 
#points(log2(clads[clads$group == 'Rotifer', 'density.areal'])~
 #        jitter(log2(clads[clads$group == 'Rotifer', 'BINMID']), 1), 
 #       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = lightblack, cex = 1.5, lwd = 2,
 #       xlab = '', ylab = '')

abline(v = log2(unique(output.plot2$BINMIN)))

output.plot2
mean = output.plot2 %>% 
  group_by(BINMID) %>%
  summarize(midpoint = mean(density.areal)) %>%
  ungroup()
mean

points(log2(midpoint)~log2(BINMID), data = mean, yaxt = 'n', xaxt = 'n', pch = 23, 
       col = 'black', bg = 'white', lwd = 2, cex = 2.5)

legend('bottomleft', legend = c('Bosmina', 'Chydorid', 'Daphnia', 'Ceriodaphnia', 
                                'Lg.Cladocera', 'Calanoid', 'Cyclopoid', 'Nauplii','Avg. Density'), 
       pch = c(rep(19, 8), 23), col = c(ref_col_18, colfunc[1], colfunc[2],  colfunc[4], colfunc[5], 'steelblue', colfunc[6], colfunc[7]))
mtext(side=1, line =2, 'Log2(Individual Biomass)')
mtext(side=2, line =2, 'Log2(Areal Density)')
mtext(side=3, line=1, 'Cladocera & Copepods Only', cex = 2, font =2)

## Split by lake and year - raw data ##=========================
# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)
# 7x3 plot #
output.plot3 = output.plot2 %>% mutate(group = case_when(.$taxon %in% c("Alona",
                                                     "Alonella",
                                                     "Pleuroxus", 
                                                     "Camptocercus", 
                                                     "Graptoleberis") ~ "Cladocera",
                                      .$taxon %in% c("Bosmina") ~ 'Bosmina',
                                      .$taxon %in% c("Chydorus") ~ 'Chydorid',
                                      .$taxon %in% c("Daphnia.lumholtzi",
                                                     "Diaphanosoma",
                                                     "Leptodora",
                                                     "Moina",
                                                     "Scapholeberis") ~ "Cladocera",
                                      .$taxon %in% c("Ceriodaphnia") ~ 'Cladocera', 
                                      .$taxon %in% c("Daphnia") ~ 'Daphnia',
                                      .$taxon %in% c('Simocephalus') ~ 'Cladocera',
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
                                                     "Trichotria") ~ "Rotifer",
                                      .$taxon %in% c("Cyclopoida") ~ "Cyclopoid",
                                      .$taxon %in% c("Calanoida") ~ "Calanoid",
                                      .$taxon %in% c("Nauplii") ~ "Nauplii")) %>% 
  as.data.frame()

## Taxa Colors ## 
clad = adjustcolor(col = '#dd4444', alpha.f = 0.6)
bos = adjustcolor(col = '#f48080', alpha.f = 0.6)
chy = adjustcolor(col = '#ffdcdc', alpha.f = 0.6)
dap = adjustcolor(col = '#540101', alpha.f = 0.6)
rot = adjustcolor(col = 'gray99', alpha.f = 0.6)
cyc = adjustcolor(col = '#2d676f', alpha.f = 0.4)
cal = adjustcolor(col = '#194b4f', alpha.f = 0.8)
nau = adjustcolor(col = '#00afbf', alpha.f = 0.6)

## Spring Plots - by lake and year ##====================================== 
spring = filter(.data = output.plot3, season == 'spring')
spring

summer = filter(.data = output.plot3, season == 'summer')
summer 

full = output.plot3

# Reference lakes # ============================
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


#Storm 
sp.storm18 = spring %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2018')
sp.storm18  

plot(log2(sp.storm18[sp.storm18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.storm18[sp.storm18$group == 'Cladcoera', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(sp.storm18[sp.storm18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.storm18[sp.storm18$group == 'Bosmina', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm18[sp.storm18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.storm18[sp.storm18$group == 'Chydorid', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm18[sp.storm18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.storm18[sp.storm18$group == 'Daphnia', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm18[sp.storm18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.storm18[sp.storm18$group == 'Rotifer', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm18[sp.storm18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.storm18[sp.storm18$group == 'Cyclopoid', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm18[sp.storm18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.storm18[sp.storm18$group == 'Calanoid', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm18[sp.storm18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.storm18[sp.storm18$group == 'Nauplii', 'BINMID']), 1), data = sp.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

abline(v = log2(unique(output.plot2$BINMIN)))


sp.storm19 = spring %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2019')
sp.storm19 

plot(log2(sp.storm19[sp.storm19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.storm19[sp.storm19$group == 'Cladcoera', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.storm19[sp.storm19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.storm19[sp.storm19$group == 'Bosmina', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm19[sp.storm19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.storm19[sp.storm19$group == 'Chydorid', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm19[sp.storm19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.storm19[sp.storm19$group == 'Daphnia', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm19[sp.storm19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.storm19[sp.storm19$group == 'Rotifer', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm19[sp.storm19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.storm19[sp.storm19$group == 'Cyclopoid', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm19[sp.storm19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.storm19[sp.storm19$group == 'Calanoid', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm19[sp.storm19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.storm19[sp.storm19$group == 'Nauplii', 'BINMID']), 1), data = sp.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



sp.storm20 = spring %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2020')
sp.storm20  

plot(log2(sp.storm20[sp.storm20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.storm20[sp.storm20$group == 'Cladcoera', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.storm20[sp.storm20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.storm20[sp.storm20$group == 'Bosmina', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm20[sp.storm20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.storm20[sp.storm20$group == 'Chydorid', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm20[sp.storm20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.storm20[sp.storm20$group == 'Daphnia', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm20[sp.storm20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.storm20[sp.storm20$group == 'Rotifer', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm20[sp.storm20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.storm20[sp.storm20$group == 'Cyclopoid', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm20[sp.storm20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.storm20[sp.storm20$group == 'Calanoid', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.storm20[sp.storm20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.storm20[sp.storm20$group == 'Nauplii', 'BINMID']), 1), data = sp.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



#South.Twin
sp.southtwin18 = spring %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2018')
sp.southtwin18  

plot(log2(sp.southtwin18[sp.southtwin18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Cladcoera', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.southtwin18[sp.southtwin18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Bosmina', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin18[sp.southtwin18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Chydorid', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin18[sp.southtwin18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Daphnia', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin18[sp.southtwin18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Rotifer', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin18[sp.southtwin18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Cyclopoid', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin18[sp.southtwin18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Calanoid', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin18[sp.southtwin18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.southtwin18[sp.southtwin18$group == 'Nauplii', 'BINMID']), 1), data = sp.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



sp.southtwin19 = spring %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2019')
sp.southtwin19 

plot(log2(sp.southtwin19[sp.southtwin19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Cladcoera', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.southtwin19[sp.southtwin19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Bosmina', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin19[sp.southtwin19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Chydorid', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin19[sp.southtwin19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Daphnia', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin19[sp.southtwin19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Rotifer', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin19[sp.southtwin19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Cyclopoid', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin19[sp.southtwin19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Calanoid', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin19[sp.southtwin19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.southtwin19[sp.southtwin19$group == 'Nauplii', 'BINMID']), 1), data = sp.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



sp.southtwin20 = spring %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2020')
sp.southtwin20  

plot(log2(sp.southtwin20[sp.southtwin20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Cladcoera', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.southtwin20[sp.southtwin20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Bosmina', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin20[sp.southtwin20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Chydorid', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin20[sp.southtwin20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Daphnia', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin20[sp.southtwin20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Rotifer', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin20[sp.southtwin20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Cyclopoid', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin20[sp.southtwin20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Calanoid', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.southtwin20[sp.southtwin20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.southtwin20[sp.southtwin20$group == 'Nauplii', 'BINMID']), 1), data = sp.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

## Taxa Colors ## 
clad = adjustcolor(col = '#dd4444', alpha.f = 0.6)
bos = adjustcolor(col = '#f48080', alpha.f = 0.6)
chy = adjustcolor(col = '#ffdcdc', alpha.f = 0.6)
dap = adjustcolor(col = '#540101', alpha.f = 0.6)
rot = adjustcolor(col = 'gray99', alpha.f = 0.6)
cyc = adjustcolor(col = '#2d676f', alpha.f = 0.4)
cal = adjustcolor(col = '#194b4f', alpha.f = 0.8)
nau = adjustcolor(col = '#00afbf', alpha.f = 0.6)

windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend('center', legend = c('Other Cladocera', 'bosmina', 'chydorid', 'daphnia', 'rotifer', 'cyclopoid', 'calanoid', 'nauplii') , pch = 19, 
     bty = 'n',
       pt.cex=1.5, cex=0.8,
       col = c(clad, bos, chy, dap, rot, cyc, cal, nau), ncol=2)
legend('center', legend = c('Other Cladocera', 'bosmina', 'chydorid', 'daphnia', 'rotifer', 'cyclopoid', 'calanoid', 'nauplii'), pch = 21,
       bty = 'n', 
       pt.cex = 1.5, cex=0.8, 
       col = 'black', ncol = 2)

# RRN Lakes #===================================
# removal, removal, null # Center, Five Island 
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

sp.center18 = spring %>% 
  filter(lake == 'Center') %>%
  filter(year == '2018')
sp.center18  

plot(log2(sp.center18[sp.center18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.center18[sp.center18$group == 'Cladcoera', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.center18[sp.center18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.center18[sp.center18$group == 'Bosmina', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center18[sp.center18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.center18[sp.center18$group == 'Chydorid', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center18[sp.center18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.center18[sp.center18$group == 'Daphnia', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center18[sp.center18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.center18[sp.center18$group == 'Rotifer', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center18[sp.center18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.center18[sp.center18$group == 'Cyclopoid', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center18[sp.center18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.center18[sp.center18$group == 'Calanoid', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center18[sp.center18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.center18[sp.center18$group == 'Nauplii', 'BINMID']), 1), data = sp.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
bins = as.data.frame(output.plot2$BINMID) %>% arrange()

sp.center19 = spring %>% 
  filter(lake == 'Center') %>%
  filter(year == '2019')
sp.center19 

plot(log2(sp.center19[sp.center19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.center19[sp.center19$group == 'Cladcoera', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.center19[sp.center19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.center19[sp.center19$group == 'Bosmina', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center19[sp.center19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.center19[sp.center19$group == 'Chydorid', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center19[sp.center19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.center19[sp.center19$group == 'Daphnia', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center19[sp.center19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.center19[sp.center19$group == 'Rotifer', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center19[sp.center19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.center19[sp.center19$group == 'Cyclopoid', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center19[sp.center19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.center19[sp.center19$group == 'Calanoid', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center19[sp.center19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.center19[sp.center19$group == 'Nauplii', 'BINMID']), 1), data = sp.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

sp.center20 = spring %>% 
  filter(lake == 'Center') %>%
  filter(year == '2020')
sp.center20  

plot(log2(sp.center20[sp.center20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.center20[sp.center20$group == 'Cladcoera', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.center20[sp.center20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.center20[sp.center20$group == 'Bosmina', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center20[sp.center20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.center20[sp.center20$group == 'Chydorid', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center20[sp.center20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.center20[sp.center20$group == 'Daphnia', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center20[sp.center20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.center20[sp.center20$group == 'Rotifer', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center20[sp.center20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.center20[sp.center20$group == 'Cyclopoid', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center20[sp.center20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.center20[sp.center20$group == 'Calanoid', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.center20[sp.center20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.center20[sp.center20$group == 'Nauplii', 'BINMID']), 1), data = sp.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

#Five Island 
sp.fivisl18 = spring %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2018')
sp.fivisl18  

plot(log2(sp.fivisl18[sp.fivisl18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Cladcoera', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.fivisl18[sp.fivisl18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Bosmina', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl18[sp.fivisl18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Chydorid', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl18[sp.fivisl18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Daphnia', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl18[sp.fivisl18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Rotifer', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl18[sp.fivisl18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Cyclopoid', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl18[sp.fivisl18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Calanoid', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl18[sp.fivisl18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.fivisl18[sp.fivisl18$group == 'Nauplii', 'BINMID']), 1), data = sp.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

sp.fivisl19 = spring %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2019')
sp.fivisl19 

plot(log2(sp.fivisl19[sp.fivisl19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Cladcoera', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.fivisl19[sp.fivisl19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Bosmina', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl19[sp.fivisl19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Chydorid', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl19[sp.fivisl19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Daphnia', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl19[sp.fivisl19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Rotifer', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl19[sp.fivisl19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Cyclopoid', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl19[sp.fivisl19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Calanoid', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl19[sp.fivisl19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.fivisl19[sp.fivisl19$group == 'Nauplii', 'BINMID']), 1), data = sp.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

sp.fivisl20 = spring %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2020')
sp.fivisl20  

plot(log2(sp.fivisl20[sp.fivisl20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Cladcoera', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.fivisl20[sp.fivisl20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Bosmina', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl20[sp.fivisl20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Chydorid', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl20[sp.fivisl20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Daphnia', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl20[sp.fivisl20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Rotifer', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl20[sp.fivisl20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Cyclopoid', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl20[sp.fivisl20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Calanoid', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.fivisl20[sp.fivisl20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.fivisl20[sp.fivisl20$group == 'Nauplii', 'BINMID']), 1), data = sp.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

# NRR # ===========================
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# null, removal, removal # North Twin, Silver
sp.northtwin18 = spring %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2018')
sp.northtwin18  

plot(log2(sp.northtwin18[sp.northtwin18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Cladcoera', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.northtwin18[sp.northtwin18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Bosmina', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin18[sp.northtwin18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Chydorid', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin18[sp.northtwin18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Daphnia', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin18[sp.northtwin18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Rotifer', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin18[sp.northtwin18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Cyclopoid', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin18[sp.northtwin18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Calanoid', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin18[sp.northtwin18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.northtwin18[sp.northtwin18$group == 'Nauplii', 'BINMID']), 1), data = sp.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

sp.northtwin19 = spring %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2019')
sp.northtwin19 

plot(log2(sp.northtwin19[sp.northtwin19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Cladcoera', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.northtwin19[sp.northtwin19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Bosmina', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin19[sp.northtwin19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Chydorid', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin19[sp.northtwin19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Daphnia', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin19[sp.northtwin19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Rotifer', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin19[sp.northtwin19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Cyclopoid', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin19[sp.northtwin19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Calanoid', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin19[sp.northtwin19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.northtwin19[sp.northtwin19$group == 'Nauplii', 'BINMID']), 1), data = sp.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

sp.northtwin20 = spring %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2020')
sp.northtwin20  

plot(log2(sp.northtwin20[sp.northtwin20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Cladcoera', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.northtwin20[sp.northtwin20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Bosmina', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin20[sp.northtwin20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Chydorid', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin20[sp.northtwin20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Daphnia', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin20[sp.northtwin20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Rotifer', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin20[sp.northtwin20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Cyclopoid', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin20[sp.northtwin20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Calanoid', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.northtwin20[sp.northtwin20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.northtwin20[sp.northtwin20$group == 'Nauplii', 'BINMID']), 1), data = sp.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

#Silver 
sp.silver18 = spring %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2018')
sp.silver18  

plot(log2(sp.silver18[sp.silver18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.silver18[sp.silver18$group == 'Cladcoera', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.silver18[sp.silver18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.silver18[sp.silver18$group == 'Bosmina', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver18[sp.silver18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.silver18[sp.silver18$group == 'Chydorid', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver18[sp.silver18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.silver18[sp.silver18$group == 'Daphnia', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver18[sp.silver18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.silver18[sp.silver18$group == 'Rotifer', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver18[sp.silver18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.silver18[sp.silver18$group == 'Cyclopoid', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver18[sp.silver18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.silver18[sp.silver18$group == 'Calanoid', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver18[sp.silver18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.silver18[sp.silver18$group == 'Nauplii', 'BINMID']), 1), data = sp.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

sp.silver19 = spring %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2019')
sp.silver19 

plot(log2(sp.silver19[sp.silver19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.silver19[sp.silver19$group == 'Cladcoera', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.silver19[sp.silver19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.silver19[sp.silver19$group == 'Bosmina', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver19[sp.silver19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.silver19[sp.silver19$group == 'Chydorid', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver19[sp.silver19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.silver19[sp.silver19$group == 'Daphnia', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver19[sp.silver19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.silver19[sp.silver19$group == 'Rotifer', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver19[sp.silver19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.silver19[sp.silver19$group == 'Cyclopoid', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver19[sp.silver19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.silver19[sp.silver19$group == 'Calanoid', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver19[sp.silver19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.silver19[sp.silver19$group == 'Nauplii', 'BINMID']), 1), data = sp.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

sp.silver20 = spring %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2020')
sp.silver20  

plot(log2(sp.silver20[sp.silver20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(sp.silver20[sp.silver20$group == 'Cladcoera', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(sp.silver20[sp.silver20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(sp.silver20[sp.silver20$group == 'Bosmina', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver20[sp.silver20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(sp.silver20[sp.silver20$group == 'Chydorid', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver20[sp.silver20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(sp.silver20[sp.silver20$group == 'Daphnia', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver20[sp.silver20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(sp.silver20[sp.silver20$group == 'Rotifer', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver20[sp.silver20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(sp.silver20[sp.silver20$group == 'Cyclopoid', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver20[sp.silver20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(sp.silver20[sp.silver20$group == 'Calanoid', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(sp.silver20[sp.silver20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(sp.silver20[sp.silver20$group == 'Nauplii', 'BINMID']), 1), data = sp.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

## Summer Plots - by lake and year ##======================
# NRR # ===========================
windows(height = 7, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

#Blue 
su.blue18 = summer %>% 
  filter(lake == 'Blue') %>%
  filter(year == '2018')
su.blue18  

plot(log2(su.blue18[su.blue18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.blue18[su.blue18$group == 'Cladcoera', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.blue18[su.blue18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.blue18[su.blue18$group == 'Bosmina', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue18[su.blue18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.blue18[su.blue18$group == 'Chydorid', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue18[su.blue18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.blue18[su.blue18$group == 'Daphnia', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue18[su.blue18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.blue18[su.blue18$group == 'Rotifer', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue18[su.blue18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.blue18[su.blue18$group == 'Cyclopoid', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue18[su.blue18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.blue18[su.blue18$group == 'Calanoid', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue18[su.blue18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.blue18[su.blue18$group == 'Nauplii', 'BINMID']), 1), data = su.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 


su.blue19 = summer %>% 
  filter(lake == 'Blue') %>%
  filter(year == '2019')
su.blue19 

plot(log2(su.blue19[su.blue19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.blue19[su.blue19$group == 'Cladcoera', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.blue19[su.blue19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.blue19[su.blue19$group == 'Bosmina', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue19[su.blue19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.blue19[su.blue19$group == 'Chydorid', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue19[su.blue19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.blue19[su.blue19$group == 'Daphnia', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue19[su.blue19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.blue19[su.blue19$group == 'Rotifer', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue19[su.blue19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.blue19[su.blue19$group == 'Cyclopoid', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue19[su.blue19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.blue19[su.blue19$group == 'Calanoid', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue19[su.blue19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.blue19[su.blue19$group == 'Nauplii', 'BINMID']), 1), data = su.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



su.blue20 = summer %>% 
  filter(lake == 'Blue') %>%
  filter(year == '2020')
su.blue20  

plot(log2(su.blue20[su.blue20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.blue20[su.blue20$group == 'Cladcoera', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.blue20[su.blue20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.blue20[su.blue20$group == 'Bosmina', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue20[su.blue20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.blue20[su.blue20$group == 'Chydorid', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue20[su.blue20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.blue20[su.blue20$group == 'Daphnia', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue20[su.blue20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.blue20[su.blue20$group == 'Rotifer', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue20[su.blue20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.blue20[su.blue20$group == 'Cyclopoid', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue20[su.blue20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.blue20[su.blue20$group == 'Calanoid', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.blue20[su.blue20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.blue20[su.blue20$group == 'Nauplii', 'BINMID']), 1), data = su.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

#Storm 
su.storm18 = summer %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2018')
su.storm18  

plot(log2(su.storm18[su.storm18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.storm18[su.storm18$group == 'Cladcoera', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.storm18[su.storm18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.storm18[su.storm18$group == 'Bosmina', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm18[su.storm18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.storm18[su.storm18$group == 'Chydorid', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm18[su.storm18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.storm18[su.storm18$group == 'Daphnia', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm18[su.storm18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.storm18[su.storm18$group == 'Rotifer', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm18[su.storm18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.storm18[su.storm18$group == 'Cyclopoid', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm18[su.storm18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.storm18[su.storm18$group == 'Calanoid', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm18[su.storm18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.storm18[su.storm18$group == 'Nauplii', 'BINMID']), 1), data = su.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 


su.storm19 = summer %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2019')
su.storm19 

plot(log2(su.storm19[su.storm19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.storm19[su.storm19$group == 'Cladcoera', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.storm19[su.storm19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.storm19[su.storm19$group == 'Bosmina', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm19[su.storm19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.storm19[su.storm19$group == 'Chydorid', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm19[su.storm19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.storm19[su.storm19$group == 'Daphnia', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm19[su.storm19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.storm19[su.storm19$group == 'Rotifer', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm19[su.storm19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.storm19[su.storm19$group == 'Cyclopoid', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm19[su.storm19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.storm19[su.storm19$group == 'Calanoid', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm19[su.storm19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.storm19[su.storm19$group == 'Nauplii', 'BINMID']), 1), data = su.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



su.storm20 = summer %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2020')
su.storm20  

plot(log2(su.storm20[su.storm20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.storm20[su.storm20$group == 'Cladcoera', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.storm20[su.storm20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.storm20[su.storm20$group == 'Bosmina', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm20[su.storm20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.storm20[su.storm20$group == 'Chydorid', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm20[su.storm20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.storm20[su.storm20$group == 'Daphnia', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm20[su.storm20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.storm20[su.storm20$group == 'Rotifer', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm20[su.storm20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.storm20[su.storm20$group == 'Cyclopoid', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm20[su.storm20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.storm20[su.storm20$group == 'Calanoid', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.storm20[su.storm20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.storm20[su.storm20$group == 'Nauplii', 'BINMID']), 1), data = su.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

#South.Twin
su.southtwin18 = summer %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2018')
su.southtwin18  

plot(log2(su.southtwin18[su.southtwin18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.southtwin18[su.southtwin18$group == 'Cladcoera', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.southtwin18[su.southtwin18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.southtwin18[su.southtwin18$group == 'Bosmina', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin18[su.southtwin18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.southtwin18[su.southtwin18$group == 'Chydorid', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin18[su.southtwin18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.southtwin18[su.southtwin18$group == 'Daphnia', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin18[su.southtwin18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.southtwin18[su.southtwin18$group == 'Rotifer', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin18[su.southtwin18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.southtwin18[su.southtwin18$group == 'Cyclopoid', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin18[su.southtwin18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.southtwin18[su.southtwin18$group == 'Calanoid', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin18[su.southtwin18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.southtwin18[su.southtwin18$group == 'Nauplii', 'BINMID']), 1), data = su.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



su.southtwin19 = summer %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2019')
su.southtwin19 

plot(log2(su.southtwin19[su.southtwin19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.southtwin19[su.southtwin19$group == 'Cladcoera', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.southtwin19[su.southtwin19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.southtwin19[su.southtwin19$group == 'Bosmina', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin19[su.southtwin19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.southtwin19[su.southtwin19$group == 'Chydorid', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin19[su.southtwin19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.southtwin19[su.southtwin19$group == 'Daphnia', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin19[su.southtwin19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.southtwin19[su.southtwin19$group == 'Rotifer', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin19[su.southtwin19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.southtwin19[su.southtwin19$group == 'Cyclopoid', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin19[su.southtwin19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.southtwin19[su.southtwin19$group == 'Calanoid', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin19[su.southtwin19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.southtwin19[su.southtwin19$group == 'Nauplii', 'BINMID']), 1), data = su.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



su.southtwin20 = summer %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2020')
su.southtwin20  

plot(log2(su.southtwin20[su.southtwin20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.southtwin20[su.southtwin20$group == 'Cladcoera', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.southtwin20[su.southtwin20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.southtwin20[su.southtwin20$group == 'Bosmina', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin20[su.southtwin20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.southtwin20[su.southtwin20$group == 'Chydorid', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin20[su.southtwin20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.southtwin20[su.southtwin20$group == 'Daphnia', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin20[su.southtwin20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.southtwin20[su.southtwin20$group == 'Rotifer', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin20[su.southtwin20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.southtwin20[su.southtwin20$group == 'Cyclopoid', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin20[su.southtwin20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.southtwin20[su.southtwin20$group == 'Calanoid', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.southtwin20[su.southtwin20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.southtwin20[su.southtwin20$group == 'Nauplii', 'BINMID']), 1), data = su.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



# RRN #==========================
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
# removal, removal, null # Center, Five Island 

su.center18 = summer %>% 
  filter(lake == 'Center') %>%
  filter(year == '2018')
su.center18  

plot(log2(su.center18[su.center18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.center18[su.center18$group == 'Cladcoera', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.center18[su.center18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.center18[su.center18$group == 'Bosmina', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center18[su.center18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.center18[su.center18$group == 'Chydorid', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center18[su.center18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.center18[su.center18$group == 'Daphnia', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center18[su.center18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.center18[su.center18$group == 'Rotifer', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center18[su.center18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.center18[su.center18$group == 'Cyclopoid', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center18[su.center18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.center18[su.center18$group == 'Calanoid', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center18[su.center18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.center18[su.center18$group == 'Nauplii', 'BINMID']), 1), data = su.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
bins = as.data.frame(output.plot2$BINMID) %>% arrange()

su.center19 = summer %>% 
  filter(lake == 'Center') %>%
  filter(year == '2019')
su.center19 

plot(log2(su.center19[su.center19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.center19[su.center19$group == 'Cladcoera', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.center19[su.center19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.center19[su.center19$group == 'Bosmina', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center19[su.center19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.center19[su.center19$group == 'Chydorid', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center19[su.center19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.center19[su.center19$group == 'Daphnia', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center19[su.center19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.center19[su.center19$group == 'Rotifer', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center19[su.center19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.center19[su.center19$group == 'Cyclopoid', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center19[su.center19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.center19[su.center19$group == 'Calanoid', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center19[su.center19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.center19[su.center19$group == 'Nauplii', 'BINMID']), 1), data = su.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

su.center20 = summer %>% 
  filter(lake == 'Center') %>%
  filter(year == '2020')
su.center20  

plot(log2(su.center20[su.center20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.center20[su.center20$group == 'Cladcoera', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.center20[su.center20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.center20[su.center20$group == 'Bosmina', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center20[su.center20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.center20[su.center20$group == 'Chydorid', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center20[su.center20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.center20[su.center20$group == 'Daphnia', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center20[su.center20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.center20[su.center20$group == 'Rotifer', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center20[su.center20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.center20[su.center20$group == 'Cyclopoid', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center20[su.center20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.center20[su.center20$group == 'Calanoid', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.center20[su.center20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.center20[su.center20$group == 'Nauplii', 'BINMID']), 1), data = su.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

#Five Island 
su.fivisl18 = summer %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2018')
su.fivisl18  

plot(log2(su.fivisl18[su.fivisl18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.fivisl18[su.fivisl18$group == 'Cladcoera', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.fivisl18[su.fivisl18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.fivisl18[su.fivisl18$group == 'Bosmina', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl18[su.fivisl18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.fivisl18[su.fivisl18$group == 'Chydorid', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl18[su.fivisl18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.fivisl18[su.fivisl18$group == 'Daphnia', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl18[su.fivisl18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.fivisl18[su.fivisl18$group == 'Rotifer', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl18[su.fivisl18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.fivisl18[su.fivisl18$group == 'Cyclopoid', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl18[su.fivisl18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.fivisl18[su.fivisl18$group == 'Calanoid', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl18[su.fivisl18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.fivisl18[su.fivisl18$group == 'Nauplii', 'BINMID']), 1), data = su.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

su.fivisl19 = summer %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2019')
su.fivisl19 

plot(log2(su.fivisl19[su.fivisl19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.fivisl19[su.fivisl19$group == 'Cladcoera', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.fivisl19[su.fivisl19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.fivisl19[su.fivisl19$group == 'Bosmina', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl19[su.fivisl19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.fivisl19[su.fivisl19$group == 'Chydorid', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl19[su.fivisl19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.fivisl19[su.fivisl19$group == 'Daphnia', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl19[su.fivisl19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.fivisl19[su.fivisl19$group == 'Rotifer', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl19[su.fivisl19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.fivisl19[su.fivisl19$group == 'Cyclopoid', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl19[su.fivisl19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.fivisl19[su.fivisl19$group == 'Calanoid', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl19[su.fivisl19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.fivisl19[su.fivisl19$group == 'Nauplii', 'BINMID']), 1), data = su.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

su.fivisl20 = summer %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2020')
su.fivisl20  

plot(log2(su.fivisl20[su.fivisl20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.fivisl20[su.fivisl20$group == 'Cladcoera', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.fivisl20[su.fivisl20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.fivisl20[su.fivisl20$group == 'Bosmina', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl20[su.fivisl20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.fivisl20[su.fivisl20$group == 'Chydorid', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl20[su.fivisl20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.fivisl20[su.fivisl20$group == 'Daphnia', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl20[su.fivisl20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.fivisl20[su.fivisl20$group == 'Rotifer', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl20[su.fivisl20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.fivisl20[su.fivisl20$group == 'Cyclopoid', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl20[su.fivisl20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.fivisl20[su.fivisl20$group == 'Calanoid', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.fivisl20[su.fivisl20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.fivisl20[su.fivisl20$group == 'Nauplii', 'BINMID']), 1), data = su.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

# NRR #========================
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
# null, removal, removal # North Twin, Silver
su.northtwin18 = summer %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2018')
su.northtwin18  

plot(log2(su.northtwin18[su.northtwin18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.northtwin18[su.northtwin18$group == 'Cladcoera', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.northtwin18[su.northtwin18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.northtwin18[su.northtwin18$group == 'Bosmina', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin18[su.northtwin18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.northtwin18[su.northtwin18$group == 'Chydorid', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin18[su.northtwin18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.northtwin18[su.northtwin18$group == 'Daphnia', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin18[su.northtwin18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.northtwin18[su.northtwin18$group == 'Rotifer', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin18[su.northtwin18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.northtwin18[su.northtwin18$group == 'Cyclopoid', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin18[su.northtwin18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.northtwin18[su.northtwin18$group == 'Calanoid', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin18[su.northtwin18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.northtwin18[su.northtwin18$group == 'Nauplii', 'BINMID']), 1), data = su.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

su.northtwin19 = summer %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2019')
su.northtwin19 

plot(log2(su.northtwin19[su.northtwin19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.northtwin19[su.northtwin19$group == 'Cladcoera', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.northtwin19[su.northtwin19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.northtwin19[su.northtwin19$group == 'Bosmina', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin19[su.northtwin19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.northtwin19[su.northtwin19$group == 'Chydorid', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin19[su.northtwin19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.northtwin19[su.northtwin19$group == 'Daphnia', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin19[su.northtwin19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.northtwin19[su.northtwin19$group == 'Rotifer', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin19[su.northtwin19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.northtwin19[su.northtwin19$group == 'Cyclopoid', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin19[su.northtwin19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.northtwin19[su.northtwin19$group == 'Calanoid', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin19[su.northtwin19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.northtwin19[su.northtwin19$group == 'Nauplii', 'BINMID']), 1), data = su.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

su.northtwin20 = summer %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2020')
su.northtwin20  

plot(log2(su.northtwin20[su.northtwin20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.northtwin20[su.northtwin20$group == 'Cladcoera', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.northtwin20[su.northtwin20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.northtwin20[su.northtwin20$group == 'Bosmina', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin20[su.northtwin20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.northtwin20[su.northtwin20$group == 'Chydorid', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin20[su.northtwin20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.northtwin20[su.northtwin20$group == 'Daphnia', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin20[su.northtwin20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.northtwin20[su.northtwin20$group == 'Rotifer', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin20[su.northtwin20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.northtwin20[su.northtwin20$group == 'Cyclopoid', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin20[su.northtwin20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.northtwin20[su.northtwin20$group == 'Calanoid', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.northtwin20[su.northtwin20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.northtwin20[su.northtwin20$group == 'Nauplii', 'BINMID']), 1), data = su.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

#Silver 
su.silver18 = summer %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2018')
su.silver18  

plot(log2(su.silver18[su.silver18$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.silver18[su.silver18$group == 'Cladcoera', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.silver18[su.silver18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.silver18[su.silver18$group == 'Bosmina', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver18[su.silver18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.silver18[su.silver18$group == 'Chydorid', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver18[su.silver18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.silver18[su.silver18$group == 'Daphnia', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver18[su.silver18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.silver18[su.silver18$group == 'Rotifer', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver18[su.silver18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.silver18[su.silver18$group == 'Cyclopoid', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver18[su.silver18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.silver18[su.silver18$group == 'Calanoid', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver18[su.silver18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.silver18[su.silver18$group == 'Nauplii', 'BINMID']), 1), data = su.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

su.silver19 = summer %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2019')
su.silver19 

plot(log2(su.silver19[su.silver19$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.silver19[su.silver19$group == 'Cladcoera', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.silver19[su.silver19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.silver19[su.silver19$group == 'Bosmina', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver19[su.silver19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.silver19[su.silver19$group == 'Chydorid', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver19[su.silver19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.silver19[su.silver19$group == 'Daphnia', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver19[su.silver19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.silver19[su.silver19$group == 'Rotifer', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver19[su.silver19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.silver19[su.silver19$group == 'Cyclopoid', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver19[su.silver19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.silver19[su.silver19$group == 'Calanoid', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver19[su.silver19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.silver19[su.silver19$group == 'Nauplii', 'BINMID']), 1), data = su.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

su.silver20 = summer %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2020')
su.silver20  

plot(log2(su.silver20[su.silver20$group == 'Cladcoera', 'density.areal'])~
       jitter(log2(su.silver20[su.silver20$group == 'Cladcoera', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(su.silver20[su.silver20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(su.silver20[su.silver20$group == 'Bosmina', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver20[su.silver20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(su.silver20[su.silver20$group == 'Chydorid', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver20[su.silver20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(su.silver20[su.silver20$group == 'Daphnia', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver20[su.silver20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(su.silver20[su.silver20$group == 'Rotifer', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver20[su.silver20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(su.silver20[su.silver20$group == 'Cyclopoid', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver20[su.silver20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(su.silver20[su.silver20$group == 'Calanoid', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(su.silver20[su.silver20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(su.silver20[su.silver20$group == 'Nauplii', 'BINMID']), 1), data = su.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(2.29e-5)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

# Generate MIV Data ## =================================

# Load in data # 
mivlength = read_csv('MIV_lengthdat.csv')
mivlength

# Need to adjust code as data is already all in one file # 
unique(mivlength$taxa)

# Correct taxa names  - remove bivalvia, hydrachnellae, and consolidate unionidae # 
mivlength.fin = mivlength %>%  
  filter(taxa != 'hydrachnellae') # couldn't find length-mass for mites 
mivlength.fin$taxa[mivlength.fin$taxa == "unionoidae"] <- "unionidae"
mivlength.fin

unique(mivlength.fin$taxa)


#This is the list of taxa that we identify and the order that they are in for the rows
#in the ZooCountTemplate; there are 44 taxa in this list
taxon<-c('planorbidae', 'physidae', 'chaoboridae', 'diptera', 'chironomidae', 'oligochaeta', 
         'hirudinea', 'ceratopogonidae', 'ephemeroptera', 'trichoptera', 'no organisms',  
         'corbiculidae',   'sphaeriidae',  'sialidae',  'molannidae', 'hydropsychidae', 
         'unionidae', 'bivalvia', 'simuliidae',
         'ephydridae', 'gammaridae', 'ephemeridae','dreissenidae')

#Calculate the biomass in micrograms per liter by converting the length and counts
#for each taxa using the allometric equations below
#Note that the equations are specific to each taxa, and reference the indexed list of 
#taxon above (n=1-44)

## Calculate MIV Biomass ## 
taxon[1]
t1 = mivlength.fin %>% 
  filter( taxa == taxon[1]) %>% 
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>% 
  drop_na() %>% 
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t1

# add equation 
# ln(dry mass - g) = 1.72*SL + 0.8*AW - 9.49  (g, mm) 

t1 = t1 %>% mutate(ln_drymass_g = (1.72*SL)+(0.8*AW)-9.49) %>%
  mutate(drymass_g = exp(ln_drymass_g)) %>%
  mutate(drymass_mg = drymass_g*1000) %>% 
  mutate(ln_drymass_g = ln_drymass_g) %>%
  group_by(sampleid) %>% # count isn't important here as I already have the density data - so average weight per taxa is what's important 
  summarize(avg_drymass_g = mean(drymass_g), 
            ln2_drymass_g = log2(drymass_g)) %>% 
  mutate(taxa = 'Planorbidae')
t1

# Physidae
taxon[2]
t2 = mivlength.fin %>% 
  filter( taxa == taxon[2]) %>% 
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>% 
  drop_na() %>% 
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t2

# add equation 
# ln(dry mass - mg) = (0.0269*SL)^3.003 
t2 = t2 %>% mutate(drymass_mg = 0.0269*(SL^3.003)) %>%
  mutate(drymass_g = drymass_mg/1000) %>% 
  mutate(ln_drymass_g = log(drymass_g)) %>%
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g), 
            ln2_drymass_g = log2(drymass_g)) %>% 
  mutate(taxa = 'Physidae')
t2

# Chaoboridae 
taxon[3]
t3 = mivlength.fin %>%
  filter( taxa == taxon[3]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>% 
  drop_na() %>% 
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t3  

# add equation 
# dry mass - ug = 0.0010(BL)^5.4707 
t3 = t3 %>% mutate(drymass_ug = (0.0533)*(BL^3.4906)) %>% # 2nd Instar regression based on length range # 
  mutate(drymass_g = drymass_ug/1000000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>% 
  mutate(taxa = 'Chaoboridae')
t3

# Diptera 
taxon[4]
t4 = mivlength.fin %>%
  filter( taxa == taxon[4]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm) 
t4 

# add equation 
# dry mass-mg = 0.0025*(BL^2.692) 
t4 = t4 %>% mutate(drymass_mg = (0.0025)*(BL^2.692)) %>% # 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Diptera')
t4

# chironomidae -  use head width length-mass
taxon[5]
t5 = mivlength.fin %>%
  filter( taxa == taxon[5]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t5

# add equation 
# dry mass-mg = 2.7842*(HW^2.835) 
t5 = t5 %>% mutate(drymass_mg = (2.7842)*(HW^2.835)) %>% # 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>% 
  mutate(taxa = 'Chironomidae')
t5


# oligochaeta 
taxon[6]
t6 = mivlength.fin %>%
  filter( taxa == taxon[6]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm) 
t6

# add equation 
# dry mass-mg = drymass = 0.0729*(BL^1.875)
t6 = t6 %>% mutate(drymass_mg = (0.0729*(BL^1.875))) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>% 
  mutate(taxa = 'Oligochaeta')
t6

# hirudinea 
taxon[7] 
t7 = mivlength.fin %>%
  filter( taxa == taxon[7]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t7

# add equation 
# dry mass-mg = drymass = .088037*(BL^2.06)
t7 = t7 %>% mutate(drymass_mg = (0.088037*(BL^2.06))) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Hirudinea')
t7

# certopogonidae
taxon[8]
t8 = mivlength.fin %>%
  filter( taxa == taxon[8]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t8

# add equation 
# dry mass-mg = drymass = 0.0025*(BL^2.469)
t8 = t8 %>% mutate(drymass_mg = 0.0025*(BL^2.469)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Ceratopogonidae')
t8

# ephemeroptera (use Head Width regression)
taxon[9]
t9 = mivlength.fin %>% 
  filter( taxa == taxon[9]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t9

# add equation 
# dry mass-mg = 3.319*(HW^0.57861)
t9 = t9 %>% mutate(drymass_mg = 3.319*(HW^0.57861)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Ephemeroptera')
t9

# trichoptera # 
taxon[10]
t10 = mivlength.fin %>% 
  filter( taxa == taxon[10]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t10

# add equation
# dry mass-mg = .0056*(BL^2.839) 
t10 = t10 %>% mutate(drymass_mg = 0.0056*(BL^2.839)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Trichoptera')
t10

# no organisms # 
# no data 

# corbiculidae
taxon[12]

t12 = mivlength.fin %>% 
  filter( taxa == taxon[12]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t12

# add equation 
# dry mass-mg = 0.0119*(BL^3.030) 
t12 = t12 %>% mutate(drymass_mg = 0.0119*(SL^3.030)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Corbiculidae')
t12

# sphaeriidae # 
taxon[13]
t13 = mivlength.fin %>% 
  filter( taxa == taxon[13]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t13

t13[t13$sampleid == 'P20113206001', 'SL'] <- 8.76 # Move the BL to SL (mis-entered)

# add equation 
# dry mass-mg = 0.0163*(SL^2.477) 
t13 = t13 %>% mutate(drymass_mg = 0.0163*(SL^2.477)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Sphaeriidae')
t13

# sialidae
taxon[14]
t14 = mivlength.fin %>% 
  filter( taxa == taxon[14]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t14

# add equation 
# dry mass-mg = 0.0037*(SL^2.753) 
t14 = t14 %>% mutate(drymass_mg = 0.0037*(BL^2.753)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Sialidae')
t14

# molannidae 
taxon[15]
t15 = mivlength.fin %>% 
  filter( taxa == taxon[15]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t15

# add equation 
# dry mass-mg = 0.0056*(SL^2.839)
t15 = t15 %>% mutate(drymass_mg = 0.0056*(BL^2.839)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Molannidae')
t15

# hydropsychidae 
taxon[16]
t16 = mivlength.fin %>% 
  filter( taxa == taxon[16]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t16

# add equation 
# dry mass-mg = 0.0046*(SL^2.926)
t16 = t16 %>% mutate(drymass_mg = 0.0046*(BL^2.926)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Hydropsychidae')
t16

# unionidae
taxon[17] 
t17 = mivlength.fin %>% 
  filter( taxa == taxon[17]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t17

t17[t17$sampleid == 'P18105163010', 'SL'] <- 2.67 # Move the BL to SL (mis-entered)

# add equation 
# dry mass-mg = 0.0193*(SL^2.822)
t17 = t17 %>% mutate(drymass_mg = 0.0193*(SL^2.822)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Unionidae')
t17

# bivalvia
taxon[18]
t18 = mivlength.fin %>% 
  filter( taxa == taxon[18]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t18

# add equation 
# dry mass-mg = 0.0178*(SL^2.856)

t18 = t18 %>% mutate(drymass_mg = 0.0178*(SL^2.856)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Bivalvia')
t18

# Simuliidae  
taxon[19]
t19 = mivlength.fin %>% 
  filter( taxa == taxon[19]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t19 

# add equation 
# dry mass-mg = 0.0178*(SL^2.856)

t19 = t19 %>% mutate(drymass_mg = 0.0178*(BL^2.856)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Simuliidae')
t19 

# ephydridae # - use head width equation 
taxon[20]

t20 = mivlength.fin %>% 
  filter( taxa == taxon[20]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t20

# add equation 
# Head Width - dry mass-mg = 1.9353*(HW^2.791)
t20 = t20 %>% mutate(drymass_mg = 1.9353*(HW^2.791)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Ephyridae')
t20

# gammaridae # - use head width equation 
taxon[21]

t21 = mivlength.fin %>% 
  filter( taxa == taxon[21]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t21

# add equation 
# Head Width - dry mass-mg = 0.012*(BL^2.74)
t21 = t21 %>% mutate(drymass_mg = 0.012*(BL^2.74)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Gammaridae')
t21

# ephemeridae # 
taxon[22]
t22 = mivlength.fin %>% 
  filter( taxa == taxon[22]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t22

# add equation 
# Head Width - dry mass-mg = 0.0034*(BL^2.791)
t22 = t22 %>% mutate(drymass_mg = 0.0034*(BL^2.791)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Ephemeridae')
t22

# dreissendiae # 
taxon[23]
t23 = mivlength.fin %>% 
  filter( taxa == taxon[23]) %>%
  pivot_longer(cols = c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                        x11, x12, x13, x14, x15, x16, x17, x18, 
                        x19, x20, x21, x22, x23, x24, x25, x26,
                        x27, x28, x29, x30, x31, x32, x33, x34,
                        x35, x36, x37, x38, x39, x40, x41, x42, x43), names_to = 'x', values_to = 'length_mm') %>% 
  arrange(x, sampleid) %>%
  drop_na() %>%
  pivot_wider(id_cols = c(sampleid, x, count), names_from = measurement, values_from = length_mm)
t23

# add equation 
# Head Width - dry mass-mg = 0.04736*(BL^3)
t23 = t23 %>% mutate(drymass_mg = 0.04736*(SL^3)) %>% 
  mutate(drymass_g = drymass_mg/1000) %>% 
  group_by(sampleid) %>%
  summarize(avg_drymass_g = mean(drymass_g)) %>% 
  ungroup() %>% 
  mutate(ln2_drymass_g = log2(avg_drymass_g)) %>%
  mutate(taxa = 'Dreissendiae')
t23

## Create MIV Biomass Dataset ## ======================
miv.biomass = rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, 
                    t12, t13, t14, t15, t16, t17, t18, t19,
                    t20, t21, t22, t23)
miv.biomass 

## Join Density to MIV Biomass Dataset ## =====================
miv.dens.raw = read_csv('18_20_carp_mivs.csv') %>% 
  select(sampleid, sample, lake, year, taxa, count, sample_area_m2, density) %>% 
  filter(taxa != 'No Organisms') %>% # Count as missing sample 
  filter(taxa != 'Hydrachnellae') # No Length-mass regressions available for mites 
miv.dens.raw$taxa <- gsub('Unionoida', 'Unionidae', miv.dens.raw$taxa)

miv.join = left_join(miv.dens.raw, miv.biomass, by = c('sampleid', 'taxa'))
miv.join

## NA Values derived from individual not measured in sample due to destruction or missed poor preservation ## 
## Replace NA value for the average weight of the taxa within that lake ##=======================
miv.join.NA = miv.join %>% 
  filter(is.na(avg_drymass_g))
miv.join.NA

# average weight per taxa per lake per year 
miv.avg.ply = miv.biomass %>% 
  mutate(lake = substr(sampleid, 4,6)) %>%
  group_by(lake, taxa) %>% 
  summarize(avg_drymass_g = mean(avg_drymass_g, na.rm = T), 
            ln2_drymass_g = mean(ln2_drymass_g, na.rm = T)) %>% 
  ungroup()
miv.avg.ply

# Average across sample to get the lake value 
miv.comb = miv.join %>%
  group_by(lake, year, taxa) %>% 
  summarize(density = mean(density, na.rm = T), 
            avg_drymass_g = mean(avg_drymass_g, na.rm = T)) %>% 
  ungroup()
miv.comb 

# Replace NAs with either the lake's average value Or the global value of that taxa across the study lakes # 
miv.comb[miv.comb$taxa == 'Sphaeriidae' & miv.comb$year == '2018' & miv.comb$lake == '12', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Sphaeriidae' & miv.avg.ply$lake == '012', 'avg_drymass_g']
miv.comb[miv.comb$taxa == 'Diptera' & miv.comb$year == '2019' & miv.comb$lake == '19', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Diptera' & miv.avg.ply$lake == '019', 'avg_drymass_g']
miv.comb[miv.comb$taxa == 'Trichoptera' & miv.comb$year == '2018' & miv.comb$lake == '36', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Trichoptera' & miv.avg.ply$lake == '036', 'avg_drymass_g']
global.trichoptera = miv.avg.ply[miv.avg.ply$taxa == 'Trichoptera', 'avg_drymass_g']
miv.comb[miv.comb$taxa == 'Trichoptera' & miv.comb$year == '2018' & miv.comb$lake == '105', 'avg_drymass_g'] <- mean(global.trichoptera$avg_drymass_g)
miv.comb[miv.comb$taxa == 'Trichoptera' & miv.comb$year == '2019' & miv.comb$lake == '105', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Trichoptera' & miv.avg.ply$lake == '105', 'avg_drymass_g'] 
miv.comb[miv.comb$taxa == 'Ephydridae' & miv.comb$year == '2018' & miv.comb$lake == '113', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Ephyridae' & miv.avg.ply$lake == '113', 'avg_drymass_g'] # Correcting a spelling error
global.phys = miv.avg.ply[miv.avg.ply$taxa == 'Physidae' , 'avg_drymass_g'] 
miv.comb[miv.comb$taxa == 'Physidae' & miv.comb$year == '2018' & miv.comb$lake == '113', 'avg_drymass_g'] <- mean(global.phys$avg_drymass_g) 
miv.comb[miv.comb$taxa == 'Sphaeriidae' & miv.comb$year == '2018' & miv.comb$lake == '113', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Sphaeriidae' & miv.avg.ply$lake == '113', 'avg_drymass_g']
miv.comb[miv.comb$taxa == 'Physidae' & miv.comb$year == '2019' & miv.comb$lake == '113', 'avg_drymass_g'] <- mean(global.phys$avg_drymass_g) 
global.planorb = miv.avg.ply[miv.avg.ply$taxa == 'Planorbidae' , 'avg_drymass_g'] 
miv.comb[miv.comb$taxa == 'Planorbidae' & miv.comb$year == '2019' & miv.comb$lake == '113', 'avg_drymass_g'] <- mean(global.planorb$avg_drymass_g) 
miv.comb[miv.comb$taxa == 'Dreissenidae' & miv.comb$year == '2020' & miv.comb$lake == '113', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Dreissendiae' & miv.avg.ply$lake == '113', 'avg_drymass_g']
miv.comb[miv.comb$taxa == 'Hirudinea' & miv.comb$year == '2020' & miv.comb$lake == '113', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Hirudinea' & miv.avg.ply$lake == '113', 'avg_drymass_g']
miv.comb[miv.comb$taxa == 'Sphaeriidae' & miv.comb$year == '2019' & miv.comb$lake == '406', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Sphaeriidae' & miv.avg.ply$lake == '406', 'avg_drymass_g']
miv.comb[miv.comb$taxa == 'Planorbidae' & miv.comb$year == '2019' & miv.comb$lake == '105', 'avg_drymass_g'] <- 
  miv.avg.ply[miv.avg.ply$taxa == 'Planorbidae' & miv.avg.ply$lake == '105', 'avg_drymass_g']

# combine zooplankton data with MIV data #==============================
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")
zoop.dat = as_tibble(output.plot3) %>% 
  select(!c(sampleID, count, BINMID, BINMIN, BINMAX)) %>% 
  mutate(type = 'zoop')
zoop.dat

miv.dat = miv.comb %>% 
  rename(taxon = taxa) %>% 
  rename(density.areal = density) %>%
  rename(biomass_g = avg_drymass_g) %>% 
  mutate(doy = 200) %>% # All samples were taken in summer - this just places the samples there 
  mutate(season = 'summer') %>% # similar step to above 
  mutate(type = 'miv')
miv.dat

miv.groups = read_csv('miv_taxa_groupings.csv')

# Replace lake number with lake name #
levels(miv.dat$lake) = c(levels(miv.dat$lake), 'Blue','Center', 
                             'Five.Island', 'North.Twin', 
                             'Silver', 'Storm', 'South.Twin')

miv.dat$lake[miv.dat$lake == '12'] <- 'Blue'
miv.dat$lake[miv.dat$lake == '19'] <- 'Center' 
miv.dat$lake[miv.dat$lake == '36'] <- 'Five.Island' 
miv.dat$lake[miv.dat$lake == '90'] <- 'North.Twin'
miv.dat$lake[miv.dat$lake == '105'] <- 'Silver'
miv.dat$lake[miv.dat$lake == '113'] <- 'Storm'
miv.dat$lake[miv.dat$lake == '406'] <- 'South.Twin'
miv.dat

miv.dat = left_join(miv.dat, miv.groups, by = 'taxon')

miv.dat = miv.dat %>% 
  select(lake, year, season, doy, taxon, group, biomass_g, density.areal, type)
miv.dat

# Join Zoop-MIV datasets # ============================
taxa = rbind(zoop.dat, miv.dat) %>% 
  arrange(lake)
taxa

# Calculate log2 bins #===========================
## Calculate the number of log2 bins, taken from Edwards et al. 2017 - assign bins
log2bins_butts = function(x = NULL, counts = NULL)
{
  if(!is.null(x) & !is.null(counts)) {
    stop("need only one of x or counts in log2bins") }
  if(is.null(x) & is.null(counts)) {
    stop("need x or counts in log2bins") }
  if(!is.null(x)) {
    if(!is.vector(x))stop("x not a vector in log2bins")
    if(anyNA(x)) stop("x contains NA's in log2bins")
    if(min(x) <= 0)stop("x needs to be >0 in log2bins")
  }
  if(!is.null(counts))  {
    if(dim(counts)[2] != 2)stop("counts needs two cols in log2bins")
    if(min(counts[,1]) < 0) {
      stop("x values in counts need to be >= 0 in log2bins") }
    if(min(counts[,2]) < 0) {
      stop("numbers in counts need to be >= 0 in log2bins") }
  }
  # As for LBNbiom.method(), could write code that would make
  #  use of the counts dataframe explicitly, but actually quite easy
  #  to just create the longer vector x (though may be slightly slower
  #  computationally), to save writing extensive new code.
  if(is.null(x))
  {x = rep(counts[,1], counts[,2]) }
  #
  binBreaks = 2^( floor(log2(min(x))) : ceiling(log2(max(x))) )
  
  indiv = data.frame(x)       # dataframe with one row for each individual
  indiv$binMid =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-length(binBreaks)] + 0.5*diff(binBreaks))
  indiv$binMin =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-length(binBreaks)])
  indiv$binMax =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                    labels = binBreaks[-1])
  # indiv$binWidth =cut(x, breaks=binBreaks, right=FALSE,
  #    include.lowest=TRUE, labels = diff(binBreaks))
  # indiv = mutate(indiv, binWidth = binMax - binMin)
  # Above commands avoid any problems with bins with 0 counts.
  # Don't really need all of them, but include for completeness.
  indiv$binMid = as.numeric(as.character(indiv$binMid))
  indiv$binMin = as.numeric(as.character(indiv$binMin))
  indiv$binMax = as.numeric(as.character(indiv$binMax))
  # Now calculate biomass in each bin class:
  binVals = dplyr::summarise(dplyr::group_by(indiv, binMid),
                             binMin = unique(binMin),
                             binMax = unique(binMax),
                             binWidth = binMax - binMin,
                             binCount = length(x),
                             binCountNorm = binCount / binWidth,
                             binSum = sum(x),
                             binSumNorm = binSum / binWidth )
  # binWidth uses new columns binMax and binMin
  binVals = binVals[order(binVals$binMid),]   # order by binMid
  #
  if(dim(indiv)[1] < 10^6) {       # only save indiv if not too big
    y = list(indiv = indiv, binVals = binVals)
  } else
  {
    y = list(binVals = binVals)
  }
  return(y)
}

# Rewrote the base function removing the log10 bit that was causing errors, only need the log2 part anyway
log2bins.list = log2bins_butts(taxa$biomass_g)
taxa$BINMID <- as.factor(log2bins.list$indiv$binMid)
taxa$BINMIN <- log2bins.list$indiv$binMin
taxa$BINMAX <- log2bins.list$indiv$binMax
taxa$BINMID = as.numeric(as.character(taxa$BINMID))
taxa



# Reference lakes # ============================
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

taxa = as.data.frame(taxa)
min(taxa$BINMIN)

#Storm 
tx.storm18 = taxa %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2018')
tx.storm18  

plot(log2(tx.storm18[tx.storm18$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.storm18[tx.storm18$group == 'Cladocera', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.storm18[tx.storm18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$group == 'Bosmina', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm18[tx.storm18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$group == 'Chydorid', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm18[tx.storm18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$group == 'Daphnia', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm18[tx.storm18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$group == 'Rotifer', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm18[tx.storm18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$group == 'Cyclopoid', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm18[tx.storm18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$group == 'Calanoid', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm18[tx.storm18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$group == 'Nauplii', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

# abline(v = log2(unique(taxa$BINMIN))) 

points(log2(tx.storm18[tx.storm18$type == 'miv', 'density.areal'])~
         jitter(log2(tx.storm18[tx.storm18$type == 'miv', 'BINMID']), 1), data = tx.storm18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.storm19 = taxa %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2019')
tx.storm19 

plot(log2(tx.storm19[tx.storm19$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.storm19[tx.storm19$group == 'Cladocera', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.storm19[tx.storm19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$group == 'Bosmina', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm19[tx.storm19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$group == 'Chydorid', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm19[tx.storm19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$group == 'Daphnia', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm19[tx.storm19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$group == 'Rotifer', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm19[tx.storm19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$group == 'Cyclopoid', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm19[tx.storm19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$group == 'Calanoid', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm19[tx.storm19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$group == 'Nauplii', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.storm19[tx.storm19$type == 'miv', 'density.areal'])~
         jitter(log2(tx.storm19[tx.storm19$type == 'miv', 'BINMID']), 1), data = tx.storm19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.storm20 = taxa %>% 
  filter(lake == 'Storm') %>%
  filter(year == '2020')
tx.storm20  

plot(log2(tx.storm20[tx.storm20$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.storm20[tx.storm20$group == 'Cladocera', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.storm20[tx.storm20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$group == 'Bosmina', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm20[tx.storm20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$group == 'Chydorid', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm20[tx.storm20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$group == 'Daphnia', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm20[tx.storm20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$group == 'Rotifer', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm20[tx.storm20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$group == 'Cyclopoid', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm20[tx.storm20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$group == 'Calanoid', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.storm20[tx.storm20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$group == 'Nauplii', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.storm20[tx.storm20$type == 'miv', 'density.areal'])~
         jitter(log2(tx.storm20[tx.storm20$type == 'miv', 'BINMID']), 1), data = tx.storm20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

#South.Twin
tx.southtwin18 = taxa %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2018')
tx.southtwin18  

plot(log2(tx.southtwin18[tx.southtwin18$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Cladocera', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.southtwin18[tx.southtwin18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Bosmina', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin18[tx.southtwin18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Chydorid', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin18[tx.southtwin18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Daphnia', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin18[tx.southtwin18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Rotifer', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin18[tx.southtwin18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Cyclopoid', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin18[tx.southtwin18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Calanoid', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin18[tx.southtwin18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$group == 'Nauplii', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.southtwin18[tx.southtwin18$type == 'miv', 'density.areal'])~
         jitter(log2(tx.southtwin18[tx.southtwin18$type == 'miv', 'BINMID']), 1), data = tx.southtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.southtwin19 = taxa %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2019')
tx.southtwin19 

plot(log2(tx.southtwin19[tx.southtwin19$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Cladocera', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.southtwin19[tx.southtwin19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Bosmina', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin19[tx.southtwin19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Chydorid', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin19[tx.southtwin19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Daphnia', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin19[tx.southtwin19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Rotifer', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin19[tx.southtwin19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Cyclopoid', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin19[tx.southtwin19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Calanoid', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin19[tx.southtwin19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$group == 'Nauplii', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.southtwin19[tx.southtwin19$type == 'miv', 'density.areal'])~
         jitter(log2(tx.southtwin19[tx.southtwin19$type == 'miv', 'BINMID']), 1), data = tx.southtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.southtwin20 = taxa %>% 
  filter(lake == 'South.Twin') %>%
  filter(year == '2020')
tx.southtwin20  

plot(log2(tx.southtwin20[tx.southtwin20$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Cladocera', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.southtwin20[tx.southtwin20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Bosmina', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin20[tx.southtwin20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Chydorid', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin20[tx.southtwin20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Daphnia', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin20[tx.southtwin20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Rotifer', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin20[tx.southtwin20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Cyclopoid', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin20[tx.southtwin20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Calanoid', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.southtwin20[tx.southtwin20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$group == 'Nauplii', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.southtwin20[tx.southtwin20$type == 'miv', 'density.areal'])~
         jitter(log2(tx.southtwin20[tx.southtwin20$type == 'miv', 'BINMID']), 1), data = tx.southtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.blue18 = taxa %>% 
  filter(lake == 'Blue') %>%
  filter(year == '2018')
tx.blue18  

plot(log2(tx.blue18[tx.blue18$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.blue18[tx.blue18$group == 'Cladocera', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.blue18[tx.blue18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$group == 'Bosmina', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue18[tx.blue18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$group == 'Chydorid', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue18[tx.blue18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$group == 'Daphnia', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue18[tx.blue18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$group == 'Rotifer', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue18[tx.blue18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$group == 'Cyclopoid', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue18[tx.blue18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$group == 'Calanoid', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue18[tx.blue18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$group == 'Nauplii', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.blue18[tx.blue18$type == 'miv', 'density.areal'])~
         jitter(log2(tx.blue18[tx.blue18$type == 'miv', 'BINMID']), 1), data = tx.blue18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.blue19 = taxa %>% 
  filter(lake == 'Blue') %>%
  filter(year == '2019')
tx.blue19  

plot(log2(tx.blue19[tx.blue19$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.blue19[tx.blue19$group == 'Cladocera', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.blue19[tx.blue19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$group == 'Bosmina', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue19[tx.blue19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$group == 'Chydorid', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue19[tx.blue19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$group == 'Daphnia', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue19[tx.blue19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$group == 'Rotifer', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue19[tx.blue19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$group == 'Cyclopoid', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue19[tx.blue19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$group == 'Calanoid', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue19[tx.blue19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$group == 'Nauplii', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.blue19[tx.blue19$type == 'miv', 'density.areal'])~
         jitter(log2(tx.blue19[tx.blue19$type == 'miv', 'BINMID']), 1), data = tx.blue19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.blue20 = taxa %>% 
  filter(lake == 'Blue') %>%
  filter(year == '2020')
tx.blue20  

plot(log2(tx.blue20[tx.blue20$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.blue20[tx.blue20$group == 'Cladocera', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.blue20[tx.blue20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$group == 'Bosmina', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue20[tx.blue20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$group == 'Chydorid', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue20[tx.blue20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$group == 'Daphnia', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue20[tx.blue20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$group == 'Rotifer', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue20[tx.blue20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$group == 'Cyclopoid', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue20[tx.blue20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$group == 'Calanoid', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.blue20[tx.blue20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$group == 'Nauplii', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.blue20[tx.blue20$type == 'miv', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$type == 'miv', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

## Taxa Colors ## 
clad = adjustcolor(col = '#dd4444', alpha.f = 0.6)
bos = adjustcolor(col = '#f48080', alpha.f = 0.6)
chy = adjustcolor(col = '#ffdcdc', alpha.f = 0.6)
dap = adjustcolor(col = '#540101', alpha.f = 0.6)
rot = adjustcolor(col = 'gray99', alpha.f = 0.6)
cyc = adjustcolor(col = '#2d676f', alpha.f = 0.4)
cal = adjustcolor(col = '#194b4f', alpha.f = 0.8)
nau = adjustcolor(col = '#00afbf', alpha.f = 0.6)

windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend('center', legend = c('Other Cladocera', 'bosmina', 'chydorid', 'daphnia', 'rotifer', 'cyclopoid', 'calanoid', 'nauplii', 'macroinvertebrates') , pch = 19, 
       bty = 'n',
       pt.cex=1.5, cex=0.8,
       col = c(clad, bos, chy, dap, rot, cyc, cal, nau, 'black'), ncol=2)
legend('center', legend = c('Other Cladocera', 'bosmina', 'chydorid', 'daphnia', 'rotifer', 'cyclopoid', 'calanoid', 'nauplii', 'macroinvertebrates'), pch = 21,
       bty = 'n', 
       pt.cex = 1.5, cex=0.8, 
       col = 'black', ncol = 2)

# RRN Lakes #===================================
# removal, removal, null # Center, Five Island 
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

tx.center18 = taxa %>% 
  filter(lake == 'Center') %>%
  filter(year == '2018')
tx.center18  

plot(log2(tx.center18[tx.center18$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.center18[tx.center18$group == 'Cladocera', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.center18[tx.center18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$group == 'Bosmina', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center18[tx.center18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$group == 'Chydorid', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center18[tx.center18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$group == 'Daphnia', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center18[tx.center18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$group == 'Rotifer', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center18[tx.center18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$group == 'Cyclopoid', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center18[tx.center18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$group == 'Calanoid', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center18[tx.center18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$group == 'Nauplii', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
bins = as.data.frame(output.plot2$BINMID) %>% arrange()

points(log2(tx.center18[tx.center18$type == 'miv', 'density.areal'])~
         jitter(log2(tx.center18[tx.center18$type == 'miv', 'BINMID']), 1), data = tx.center18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.center19 = taxa %>% 
  filter(lake == 'Center') %>%
  filter(year == '2019')
tx.center19 

plot(log2(tx.center19[tx.center19$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.center19[tx.center19$group == 'Cladocera', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.center19[tx.center19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$group == 'Bosmina', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center19[tx.center19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$group == 'Chydorid', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center19[tx.center19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$group == 'Daphnia', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center19[tx.center19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$group == 'Rotifer', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center19[tx.center19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$group == 'Cyclopoid', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center19[tx.center19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$group == 'Calanoid', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center19[tx.center19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$group == 'Nauplii', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.center19[tx.center19$type == 'miv', 'density.areal'])~
         jitter(log2(tx.center19[tx.center19$type == 'miv', 'BINMID']), 1), data = tx.center19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')


tx.center20 = taxa %>% 
  filter(lake == 'Center') %>%
  filter(year == '2020')
tx.center20  

plot(log2(tx.center20[tx.center20$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.center20[tx.center20$group == 'Cladocera', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.center20[tx.center20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$group == 'Bosmina', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center20[tx.center20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$group == 'Chydorid', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center20[tx.center20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$group == 'Daphnia', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center20[tx.center20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$group == 'Rotifer', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center20[tx.center20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$group == 'Cyclopoid', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center20[tx.center20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$group == 'Calanoid', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.center20[tx.center20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$group == 'Nauplii', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.center20[tx.center20$type == 'miv', 'density.areal'])~
         jitter(log2(tx.center20[tx.center20$type == 'miv', 'BINMID']), 1), data = tx.center20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

#Five Island 
tx.fivisl18 = taxa %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2018')
tx.fivisl18  

plot(log2(tx.fivisl18[tx.fivisl18$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Cladocera', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

points(log2(tx.fivisl18[tx.fivisl18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Bosmina', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl18[tx.fivisl18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Chydorid', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl18[tx.fivisl18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Daphnia', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl18[tx.fivisl18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Rotifer', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl18[tx.fivisl18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Cyclopoid', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl18[tx.fivisl18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Calanoid', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl18[tx.fivisl18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$group == 'Nauplii', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.fivisl18[tx.fivisl18$type == 'miv', 'density.areal'])~
         jitter(log2(tx.fivisl18[tx.fivisl18$type == 'miv', 'BINMID']), 1), data = tx.fivisl18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.fivisl19 = taxa %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2019')
tx.fivisl19 

plot(log2(tx.fivisl19[tx.fivisl19$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Cladocera', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.fivisl19[tx.fivisl19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Bosmina', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl19[tx.fivisl19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Chydorid', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl19[tx.fivisl19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Daphnia', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl19[tx.fivisl19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Rotifer', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl19[tx.fivisl19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Cyclopoid', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl19[tx.fivisl19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Calanoid', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl19[tx.fivisl19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$group == 'Nauplii', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.fivisl19[tx.fivisl19$type == 'miv', 'density.areal'])~
         jitter(log2(tx.fivisl19[tx.fivisl19$type == 'miv', 'BINMID']), 1), data = tx.fivisl19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.fivisl20 = taxa %>% 
  filter(lake == 'Five.Island') %>%
  filter(year == '2020')
tx.fivisl20  

plot(log2(tx.fivisl20[tx.fivisl20$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Cladocera', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.fivisl20[tx.fivisl20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Bosmina', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl20[tx.fivisl20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Chydorid', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl20[tx.fivisl20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Daphnia', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl20[tx.fivisl20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Rotifer', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl20[tx.fivisl20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Cyclopoid', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl20[tx.fivisl20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Calanoid', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.fivisl20[tx.fivisl20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$group == 'Nauplii', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.fivisl20[tx.fivisl20$type == 'miv', 'density.areal'])~
         jitter(log2(tx.fivisl20[tx.fivisl20$type == 'miv', 'BINMID']), 1), data = tx.fivisl20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

# NRR # ===========================
windows(height = 5, width = 7) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# null, removal, removal # North Twin, Silver
tx.northtwin18 = taxa %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2018')
tx.northtwin18  

plot(log2(tx.northtwin18[tx.northtwin18$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Cladocera', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.northtwin18[tx.northtwin18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Bosmina', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin18[tx.northtwin18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Chydorid', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin18[tx.northtwin18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Daphnia', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin18[tx.northtwin18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Rotifer', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin18[tx.northtwin18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Cyclopoid', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin18[tx.northtwin18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Calanoid', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin18[tx.northtwin18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$group == 'Nauplii', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.northtwin18[tx.northtwin18$type == 'miv', 'density.areal'])~
         jitter(log2(tx.northtwin18[tx.northtwin18$type == 'miv', 'BINMID']), 1), data = tx.northtwin18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.northtwin19 = taxa %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2019')
tx.northtwin19 

plot(log2(tx.northtwin19[tx.northtwin19$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Cladocera', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.northtwin19[tx.northtwin19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Bosmina', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin19[tx.northtwin19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Chydorid', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin19[tx.northtwin19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Daphnia', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin19[tx.northtwin19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Rotifer', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin19[tx.northtwin19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Cyclopoid', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin19[tx.northtwin19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Calanoid', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin19[tx.northtwin19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$group == 'Nauplii', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.northtwin19[tx.northtwin19$type == 'miv', 'density.areal'])~
         jitter(log2(tx.northtwin19[tx.northtwin19$type == 'miv', 'BINMID']), 1), data = tx.northtwin19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.northtwin20 = taxa %>% 
  filter(lake == 'North.Twin') %>%
  filter(year == '2020')
tx.northtwin20  

plot(log2(tx.northtwin20[tx.northtwin20$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Cladocera', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.northtwin20[tx.northtwin20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Bosmina', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin20[tx.northtwin20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Chydorid', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin20[tx.northtwin20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Daphnia', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin20[tx.northtwin20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Rotifer', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin20[tx.northtwin20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Cyclopoid', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin20[tx.northtwin20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Calanoid', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.northtwin20[tx.northtwin20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.northtwin20[tx.northtwin20$group == 'Nauplii', 'BINMID']), 1), data = tx.northtwin20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.blue20[tx.blue20$type == 'miv', 'density.areal'])~
         jitter(log2(tx.blue20[tx.blue20$type == 'miv', 'BINMID']), 1), data = tx.blue20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

#Silver 
tx.silver18 = taxa %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2018')
tx.silver18  

plot(log2(tx.silver18[tx.silver18$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.silver18[tx.silver18$group == 'Cladocera', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.silver18[tx.silver18$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$group == 'Bosmina', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver18[tx.silver18$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$group == 'Chydorid', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver18[tx.silver18$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$group == 'Daphnia', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver18[tx.silver18$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$group == 'Rotifer', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver18[tx.silver18$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$group == 'Cyclopoid', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver18[tx.silver18$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$group == 'Calanoid', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver18[tx.silver18$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$group == 'Nauplii', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.silver18[tx.silver18$type == 'miv', 'density.areal'])~
         jitter(log2(tx.silver18[tx.silver18$type == 'miv', 'BINMID']), 1), data = tx.silver18, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.silver19 = taxa %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2019')
tx.silver19 

plot(log2(tx.silver19[tx.silver19$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.silver19[tx.silver19$group == 'Cladocera', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.silver19[tx.silver19$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$group == 'Bosmina', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver19[tx.silver19$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$group == 'Chydorid', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver19[tx.silver19$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$group == 'Daphnia', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver19[tx.silver19$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$group == 'Rotifer', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver19[tx.silver19$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$group == 'Cyclopoid', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver19[tx.silver19$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$group == 'Calanoid', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver19[tx.silver19$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$group == 'Nauplii', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.silver19[tx.silver19$type == 'miv', 'density.areal'])~
         jitter(log2(tx.silver19[tx.silver19$type == 'miv', 'BINMID']), 1), data = tx.silver19, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

tx.silver20 = taxa %>% 
  filter(lake == 'Silver') %>%
  filter(year == '2020')
tx.silver20  

plot(log2(tx.silver20[tx.silver20$group == 'Cladocera', 'density.areal'])~
       jitter(log2(tx.silver20[tx.silver20$group == 'Cladocera', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     xaxt = 'n', pch = 21, col = 'black', bg = clad, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
max(output.plot2$density.areal)
min(output.plot2$density.areal, na.rm = T)
axis(side=2,
     at=c(log2(1), log2(2), log2(3), log2(4), log2(5), log2(6),log2(7),log2(8), log2(9),
          log2(10), log2(20), log2(30), log2(40), log2(50), log2(60),log2(70),log2(80), log2(90),
          log2(100), log2(200), log2(300), log2(400), log2(500), log2(600),log2(700),log2(800), log2(900), log2(1000), 
          log2(2000), log2(3000), log2(4000), log2(5000), log2(6000), log2(7000), log2(8000), log2(9000), log2(10000),
          log2(20000), log2(30000), log2(40000), log2(50000), log2(60000), log2(70000), log2(80000), log2(90000), log2(100000),
          log2(200000), log2(300000), log2(400000), log2(500000), log2(600000), log2(700000), log2(800000), log2(900000), log2(1000000),
          log2(2000000), log2(3000000), log2(4000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10', '', '','','','','','','','100','','','','','600', '', '', '', '1000', '', '','','','','','','','10000','','','','','','','','','100000',
                '','','','','','','','','1000000','','','4000000'),
     las=0)

bins = as.data.frame(output.plot2$BINMID) %>% arrange()

axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)



points(log2(tx.silver20[tx.silver20$group == 'Bosmina', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$group == 'Bosmina', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = bos, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver20[tx.silver20$group == 'Chydorid', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$group == 'Chydorid', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = chy, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver20[tx.silver20$group == 'Daphnia', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$group == 'Daphnia', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = dap, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver20[tx.silver20$group == 'Rotifer', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$group == 'Rotifer', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = rot, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver20[tx.silver20$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$group == 'Cyclopoid', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cyc, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver20[tx.silver20$group == 'Calanoid', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$group == 'Calanoid', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = cal, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 
points(log2(tx.silver20[tx.silver20$group == 'Nauplii', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$group == 'Nauplii', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
       xaxt = 'n', pch = 21, col = 'black', bg = nau, cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '') 

points(log2(tx.silver20[tx.silver20$type == 'miv', 'density.areal'])~
         jitter(log2(tx.silver20[tx.silver20$type == 'miv', 'BINMID']), 1), data = tx.silver20, yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)), 
       xaxt = 'n', pch = 21, col = 'black', bg = 'black', cex = 1.5, lwd = 2,ylim = c(log2(1), log2(4000000)), 
       xlab = '', ylab = '')

mod2 = lm(log2(tx.silver20$density.areal)~log2(tx.silver20$BINMID), data = tx.silver20)
summary(mod2)
abline(mod2)
