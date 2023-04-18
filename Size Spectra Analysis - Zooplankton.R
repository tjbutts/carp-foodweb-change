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

## Missing biomass data for two measured conochilus individuals 
## P19090198 - Conochilus 
  ## Added the necessary width measurement to fix ## 

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
                                          "Pleuroxus") ~ "Cladocera",
                           .$taxon %in% c("Bosmina") ~ 'Cladocera',
                           .$taxon %in% c("Chydorus") ~ 'Cladocera',
                           .$taxon %in% c("Camptocercus",
                                          "Daphnia.lumholtzi",
                                          "Diaphanosoma",
                                          "Graptoleberis",
                                          "Leptodora",
                                          "Moina",
                                          "Scapholeberis") ~ "Cladocera",
                           .$taxon %in% c("Ceriodaphnia") ~ 'Cladocera', 
                           .$taxon %in% c("Daphnia") ~ 'Cladocera',
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

## Put together size spectra data frame ## ============================
carp.zoop.density # Zooplankton Areal Density  
carp.zoop.biomass # Zooplankton Average Taxon Biomass (ug) 

# Combine datasets into output dataframe 
zp.dens = carp.zoop.density %>% 
  select(sampleID, lake, year, season, doy, taxon, group, count, density.areal)
zp.dens
zp.biom = carp.zoop.biomass %>% 
  mutate(biomass_g = avg_biomass_ug*(10^-6)) %>% # Convert data to grams
  select(sampleID, lake, year, season, doy, taxon, biomass_g)
zp.biom

output = left_join(zp.biom, zp.dens, by = c('sampleID', 
                                            'year', 'season',
                                            'doy','taxon')) %>%
  select(sampleID, lake.y, year, season, doy, taxon, group, biomass_g, count, density.areal) %>%
  rename(lake = lake.y) 
output

## VANILLA ANALYSIS ##=========================
output = left_join(zp.biom, zp.dens, by = c('sampleID', 
                                            'year', 'season',
                                            'doy','taxon')) %>%
  select(sampleID, lake.y, year, season, doy, taxon, group, biomass_g, count, density.areal) %>%
  rename(lake = lake.y) 
output


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
log2bins.list = log2bins_butts(output$biomass_g)
output$BINMID <- as.factor(log2bins.list$indiv$binMid)
output$BINMIN <- log2bins.list$indiv$binMin
output$BINMAX <- log2bins.list$indiv$binMax
output

#output = filter(.data = output, sampleID != 'P19113177') # Only one data point
#output

#Something in the middle bins is creating high density 
stripchart(density.areal~BINMID, data=output, vertical = T)

output.combined = output %>% 
  group_by(year, lake, season, BINMID, BINMIN, BINMAX) %>%
  summarize(avg.density.areal = mean(density.areal, na.rm = T)) %>%
  ungroup()

output.combined

output.combined$BINMID = as.numeric(as.character(output.combined$BINMID))
#BINMIN and BINMAX are included here but will not do anything, 
    ## but by including they will be carried through to the summary table 
    ## and will be usuable for relative density

output.combined = as_tibble(output.combined) # Save in a tidy format
output.combined

# log 2 binning # 

output.binned = output.combined %>%
  group_by(year, lake, season, BINMID, BINMIN, BINMAX) %>%
  mutate(BINMID_LOG = log2(BINMID), BINMIN_LOG = log2(BINMIN), BINMAX_LOG = log2(BINMAX), DENS_LOG = log2(avg.density.areal))
output.binned

output.binned$lake = factor(output.binned$lake, levels = c('12','19', '36', '90', '105', '113', '406'))
output.binned = output.binned %>% arrange(lake)

# Replace lake number with lake name #
levels(output.binned$lake) = c(levels(output.binned$lake), 'Blue','Center', 
                               'Five.Island', 'North.Twin', 
                               'Silver', 'Storm', 'South.Twin')

output.binned$lake[output.binned$lake == '12'] <- 'Blue'
output.binned$lake[output.binned$lake == '19'] <- 'Center' 
output.binned$lake[output.binned$lake == '36'] <- 'Five.Island' 
output.binned$lake[output.binned$lake == '90'] <- 'North.Twin'
output.binned$lake[output.binned$lake == '105'] <- 'Silver'
output.binned$lake[output.binned$lake == '113'] <- 'Storm'
output.binned$lake[output.binned$lake == '406'] <- 'South.Twin'
output.binned$lake

# reorder factors as follows # 
# Blue, Storm, South.Twin = Reference 
# Center, Five.Island = Remove, Remove, Reference 
# North.Twin, Silver = Reference, Remove, Remove 
output.binned$lake = factor(output.binned$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
output.binned = output.binned %>% arrange(lake)
output.binned

## Linear fits of pelagic size spectrum by lake by year ## 
fits.yr.lm = data.frame ( matrix ( NA, ncol = 16, nrow = length( unique (output.binned$lake)) * (length( unique (output$year) )-1) * (length( unique (output$season) ) + 1) ) )
colnames ( fits.yr.lm ) = c('year', 'lake', 'season','outlierpres', 'n', 'fitmin', 'fitmax', 'slope', 'slp_se', 'slp_l95ci' ,'slp_u95ci', 'intcpt', 'int_se', 'int_l95ci', 'int_u95CI', 'height')

fits.yr.lm$year = rep( sort( unique( output.binned$year)), each = length(unique(output.binned$lake)))
fits.yr.lm$lake = rep( as.factor ( unique (output.binned$lake) ), each = 2)
fits.yr.lm$season = rep(as.character( unique(output.binned$season)), each = 1)

fits.yr.lm$outlierpres = factor ( fits.yr.lm$outlierpres, levels = c('Y', 'N'))
fits.yr.lm

## Set up a short dataframe for easier export ## 
short.output = setNames ( data.frame ( matrix (NA, ncol = 6, nrow = 0) ), c( 'year', 'lake', 'season', 'DENS_LOG', 'BINMID_LOG', 'DATAUSEDINFIT') )

short.output$lake = factor ( short.output$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
short.output$season = factor ( short.output$season, levels = c('spring', 'summer'))

# Remove lake years with no data 
fits.yr.lm = fits.yr.lm %>% filter(!row_number() %in% c(2,16,29,30)) 
fits.yr.lm

## Loop for filling with breaks ## 
for ( i in 1:nrow(fits.yr.lm) ) { 
  
  
  if (fits.yr.lm$season[i] == 'spring' || fits.yr.lm$season[i] == 'summer') {
    tempdata = output.binned %>% 
      filter ( year == fits.yr.lm$year[i] ) %>%
      filter ( lake == fits.yr.lm$lake[i] ) %>%
      filter ( season == fits.yr.lm$season[i] )
    
    templin = lm (DENS_LOG ~ BINMID_LOG, data = tempdata) 
    
    cookcutoff = 10/length(templin$residuals) # Set Cook's value, the general rule is 4/n for detection of influence, 1 is a rough cutoff for points with too much leverage
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, 'dropped', 'used') 
    tempjoin = data.frame ( 'year' = tempdata$year, 'lake' = tempdata$lake, 'season' = tempdata$season,
                            'DENS_LOG' = tempdata$DENS_LOG, 'BINMID_LOG' = tempdata$BINMID_LOG, 'DATAUSEDINFIT' = tempcooks)
    short.output = rbind ( short.output, tempjoin)
    
    if (any ( cooks.distance(templin) > cookcutoff ) ) { # remove poitns with high leverage or just save the lm as it is 
      fits.yr.lm$outlierpres[i] = 'Y' 
      templin = lm ( DENS_LOG [-which ( cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data = tempdata)
      } else {fits.yr.lm$outlierpres[i] = 'N' } # Report that no high leverage points were found 
  }

   fits.yr.lm$n[i] = length(templin$residuals) 
   fits.yr.lm$fitmin[i] = min(templin$model$BINMID_LOG, na.rm = T)
   fits.yr.lm$fitmax[i] = max(templin$model$BINMID_LOG, na.rm = T) 
   fits.yr.lm$slope[i] = coef(templin)[[2]] 
   fits.yr.lm$slp_se[i] = coef(summary(templin))[2,2] 
   fits.yr.lm$slp_l95ci[i] = confint(templin, level = 0.95)[2,1]
   fits.yr.lm$slp_u95ci[i] = confint(templin, level=0.95)[2,2]
   fits.yr.lm$intcpt[i] = coef(templin)[[1]]
   fits.yr.lm$int_se[i] = coef(summary(templin))[1,2] 
   fits.yr.lm$int_l95ci[i] = confint(templin, level=0.95)[1,1]
   fits.yr.lm$int_u95CI[i] = confint(templin, level = 0.95)[1,2]
   
   temp.short <- tempjoin %>%
     filter ( DATAUSEDINFIT == 'used') 
   
   fits.yr.lm$height[i] = fits.yr.lm$slope[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lm$intcpt[i]
   
}


fits.yr.lm

### Fits of height from overall ### 
fits.yr.lm$height.overall = fits.yr.lm$slope * ( floor(min(fits.yr.lm$fitmin)) + ceiling(max(fits.yr.lm$fitmax)) ) / 2 + fits.yr.lm$intcpt

## output the data ## 
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")
setwd("J:/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")

#write_csv(fits.yr.lm, 'fitsdata_zooplankton_long.csv')
#write_csv(short.output, 'shortoutput_zooplankton.csv')

## Size Spectra Data ## 
short.output$lake = factor(short.output$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
plot.data = short.output 
plot.data
color_custom <- c("limegreen", "royalblue2", "gray50")
color_custom2 <- c( "purple", "royalblue2")

#### Size spectrum analysis with different spectra plotted ####
library(ggplot2)

plot_ssa <-  
  ggplot(plot.data,
         aes(BINMID_LOG, DENS_LOG, color=season) ) +
  geom_point ( alpha = 1/2, size=3 ) +
  ylim (8 , 22) +
  xlim (-35, -5) +
  facet_grid ( year ~ lake ) +
  scale_color_manual(values = color_custom2, name = 'Season', labels = c('summer', 'spring')) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          strip.text.x = element_text(size = 12 ),
          strip.text.y = element_text(size = 12 ) )

plot_ssa

#### Size Spectrum analysis of all lakes with slopes ( Need to fix graphics) ####
# Output graphs with linear fits # 
short.output

output_for_plot = short.output %>% 
  mutate(PLACEMENT = ifelse(season == 'spring', 35, ifelse (season == 'summer', 24, 13))) %>%
  left_join(., fits.yr.lm, by = c('year', 'season', 'lake'), all.x=T) # Join makes it easier for ggplot to take the data, of course it creates many rep values 
output_for_plot

options(repr.plot.width = 4, repr.plot.height = 2)
windows(width = 9, height = 4)
plot_ssa2 = 
  ggplot(short.output, 
         aes(BINMID_LOG, DENS_LOG, color=season)) + 
  geom_point( alpha = 1/2, size = 1.5) +
  ylim (8 , 22) +
  xlim (-35, -5) +
  facet_grid(year ~ lake) + 
  scale_color_manual(values = color_custom, name = 'season', labels = c('spring', 'summer') ) + 
  labs(title = 'Average Density') +

  labs( x = expression ( paste ( 'Log'[2], 'Wet Weight Biomass (g)') ), 
        y = expression ( paste ( 'Log'[2], 'Abundance (Individuals/m'^2,')' ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          strip.text.x = element_text(size = 10, face = 'bold.italic' ),
          strip.text.y = element_text(size = 10, face = 'bold.italic' ) ) +
  geom_smooth( data = output_for_plot %>% subset ( fitmax & fitmin), 
               method = lm, se = FALSE) + 
  geom_label(data = output_for_plot, aes(x=-7, y=PLACEMENT, label = format ( round( slope, 2 ), 2) ), show.legend = FALSE)
plot_ssa2


# Set working directory for plot #
ggsave('size.spectra.pdf', plot = plot_ssa2, height = 6, width = 6, dpi=600)
pdf('size.spectra.pdf')
print(plot_ssa2)
dev.off()

## MEDIAN DENSITY INSTEAD OF AVERAGE DENSITY ##=========================
output = left_join(zp.biom, zp.dens, by = c('sampleID', 
                                            'year', 'season',
                                            'doy','taxon')) %>%
  select(sampleID, lake.y, year, season, doy, taxon, group, biomass_g, count, density.areal) %>%
  rename(lake = lake.y) 
output


log2bins.list = log2bins_butts(output$biomass_g)
output$BINMID <- as.factor(log2bins.list$indiv$binMid)
output$BINMIN <- log2bins.list$indiv$binMin
output$BINMAX <- log2bins.list$indiv$binMax
output

#output = filter(.data = output, sampleID != 'P19113177') # Only one data point
#output

#Something in the middle bins is creating high density 
#stripchart(density.areal~BINMID, data=output, vertical = T)

output.combined = output %>% 
  group_by(year, lake, season, BINMID, BINMIN, BINMAX) %>%
  summarize(median.density.areal = median(density.areal, na.rm = T)) %>%
  ungroup()

output.combined

output.combined$BINMID = as.numeric(as.character(output.combined$BINMID))
#BINMIN and BINMAX are included here but will not do anything, 
## but by including they will be carried through to the summary table 
## and will be usuable for relative density

output.combined = as_tibble(output.combined) # Save in a tidy format
output.combined

# log 2 binning # 

output.binned = output.combined %>%
  group_by(year, lake, season, BINMID, BINMIN, BINMAX) %>%
  mutate(BINMID_LOG = log2(BINMID), BINMIN_LOG = log2(BINMIN), BINMAX_LOG = log2(BINMAX), DENS_LOG = log2(median.density.areal))
output.binned

output.binned$lake = factor(output.binned$lake, levels = c('12','19', '36', '90', '105', '113', '406'))
output.binned = output.binned %>% arrange(lake)

# Replace lake number with lake name #
levels(output.binned$lake) = c(levels(output.binned$lake), 'Blue','Center', 
                               'Five.Island', 'North.Twin', 
                               'Silver', 'Storm', 'South.Twin')

output.binned$lake[output.binned$lake == '12'] <- 'Blue'
output.binned$lake[output.binned$lake == '19'] <- 'Center' 
output.binned$lake[output.binned$lake == '36'] <- 'Five.Island' 
output.binned$lake[output.binned$lake == '90'] <- 'North.Twin'
output.binned$lake[output.binned$lake == '105'] <- 'Silver'
output.binned$lake[output.binned$lake == '113'] <- 'Storm'
output.binned$lake[output.binned$lake == '406'] <- 'South.Twin'
output.binned$lake

# reorder factors as follows # 
# Blue, Storm, South.Twin = Reference 
# Center, Five.Island = Remove, Remove, Reference 
# North.Twin, Silver = Reference, Remove, Remove 
output.binned$lake = factor(output.binned$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
output.binned = output.binned %>% arrange(lake)
output.binned

## Linear fits of pelagic size spectrum by lake by year ## 
fits.yr.lm = data.frame ( matrix ( NA, ncol = 16, nrow = length( unique (output.binned$lake)) * (length( unique (output$year) )-1) * (length( unique (output$season) ) + 1) ) )
colnames ( fits.yr.lm ) = c('year', 'lake', 'season','outlierpres', 'n', 'fitmin', 'fitmax', 'slope', 'slp_se', 'slp_l95ci' ,'slp_u95ci', 'intcpt', 'int_se', 'int_l95ci', 'int_u95CI', 'height')

fits.yr.lm$year = rep( sort( unique( output.binned$year)), each = length(unique(output.binned$lake)))
fits.yr.lm$lake = rep( as.factor ( unique (output.binned$lake) ), each = 2)
fits.yr.lm$season = rep(as.character( unique(output.binned$season)), each = 1)

fits.yr.lm$outlierpres = factor ( fits.yr.lm$outlierpres, levels = c('Y', 'N'))
fits.yr.lm

## Set up a short dataframe for easier export ## 
short.output = setNames ( data.frame ( matrix (NA, ncol = 6, nrow = 0) ), c( 'year', 'lake', 'season', 'DENS_LOG', 'BINMID_LOG', 'DATAUSEDINFIT') )

short.output$lake = factor ( short.output$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
short.output$season = factor ( short.output$season, levels = c('spring', 'summer'))

# Remove lake years with no data 
fits.yr.lm = fits.yr.lm %>% filter(!row_number() %in% c(2,16,29,30)) 
fits.yr.lm

## Loop for filling with breaks ## 
for ( i in 1:nrow(fits.yr.lm) ) { 
  
  
  if (fits.yr.lm$season[i] == 'spring' || fits.yr.lm$season[i] == 'summer') {
    tempdata = output.binned %>% 
      filter ( year == fits.yr.lm$year[i] ) %>%
      filter ( lake == fits.yr.lm$lake[i] ) %>%
      filter ( season == fits.yr.lm$season[i] )
    
    templin = lm (DENS_LOG ~ BINMID_LOG, data = tempdata) 
    
    cookcutoff = 10/length(templin$residuals) # Set Cook's value, the general rule is 4/n for detection of influence, 1 is a rough cutoff for points with too much leverage
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, 'dropped', 'used') 
    tempjoin = data.frame ( 'year' = tempdata$year, 'lake' = tempdata$lake, 'season' = tempdata$season,
                            'DENS_LOG' = tempdata$DENS_LOG, 'BINMID_LOG' = tempdata$BINMID_LOG, 'DATAUSEDINFIT' = tempcooks)
    short.output = rbind ( short.output, tempjoin)
    
    if (any ( cooks.distance(templin) > cookcutoff ) ) { # remove poitns with high leverage or just save the lm as it is 
      fits.yr.lm$outlierpres[i] = 'Y' 
      templin = lm ( DENS_LOG [-which ( cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data = tempdata)
    } else {fits.yr.lm$outlierpres[i] = 'N' } # Report that no high leverage points were found 
  }
  
  fits.yr.lm$n[i] = length(templin$residuals) 
  fits.yr.lm$fitmin[i] = min(templin$model$BINMID_LOG, na.rm = T)
  fits.yr.lm$fitmax[i] = max(templin$model$BINMID_LOG, na.rm = T) 
  fits.yr.lm$slope[i] = coef(templin)[[2]] 
  fits.yr.lm$slp_se[i] = coef(summary(templin))[2,2] 
  fits.yr.lm$slp_l95ci[i] = confint(templin, level = 0.95)[2,1]
  fits.yr.lm$slp_u95ci[i] = confint(templin, level=0.95)[2,2]
  fits.yr.lm$intcpt[i] = coef(templin)[[1]]
  fits.yr.lm$int_se[i] = coef(summary(templin))[1,2] 
  fits.yr.lm$int_l95ci[i] = confint(templin, level=0.95)[1,1]
  fits.yr.lm$int_u95CI[i] = confint(templin, level = 0.95)[1,2]
  
  temp.short <- tempjoin %>%
    filter ( DATAUSEDINFIT == 'used') 
  
  fits.yr.lm$height[i] = fits.yr.lm$slope[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lm$intcpt[i]
  
}


fits.yr.lm

### Fits of height from overall ### 
fits.yr.lm$height.overall = fits.yr.lm$slope * ( floor(min(fits.yr.lm$fitmin)) + ceiling(max(fits.yr.lm$fitmax)) ) / 2 + fits.yr.lm$intcpt

## output the data ## 
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")
setwd("J:/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")

#write_csv(fits.yr.lm, 'fitsdata_zooplankton_long.csv')
#write_csv(short.output, 'shortoutput_zooplankton.csv')

## Size Spectra Data ## 
short.output$lake = factor(short.output$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
plot.data = short.output 
plot.data
color_custom <- c("limegreen", "royalblue2", "gray50")
color_custom2 <- c( "purple", "royalblue2")

#### Size spectrum analysis with different spectra plotted ####
library(ggplot2)

plot_ssa <-  
  ggplot(plot.data,
         aes(BINMID_LOG, DENS_LOG, color=season) ) +
  geom_point ( alpha = 1/2, size=3 ) +
  ylim (8 , 22) +
  xlim (-35, -5) +
  facet_grid ( year ~ lake ) +
  scale_color_manual(values = color_custom2, name = 'Season', labels = c('summer', 'spring')) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          strip.text.x = element_text(size = 12 ),
          strip.text.y = element_text(size = 12 ) )

plot_ssa

#### Size Spectrum analysis of all lakes with slopes ( Need to fix graphics) ####
# Output graphs with linear fits # 
short.output

output_for_plot = short.output %>% 
  mutate(PLACEMENT = ifelse(season == 'spring', 35, ifelse (season == 'summer', 24, 13))) %>%
  left_join(., fits.yr.lm, by = c('year', 'season', 'lake'), all.x=T) # Join makes it easier for ggplot to take the data, of course it creates many rep values 
output_for_plot

options(repr.plot.width = 4, repr.plot.height = 2)
windows(width = 9, height = 4)
plot_ssa2 = 
  ggplot(short.output, 
         aes(BINMID_LOG, DENS_LOG, color=season)) + 
  geom_point( alpha = 1/2, size = 1.5) +
  ylim (8 , 22) +
  xlim (-35, -5) +
  facet_grid(year ~ lake) + 
  labs(title = 'Median Density', font = 2) +
  scale_color_manual(values = color_custom, name = 'season', labels = c('spring', 'summer') ) + 
  
  labs( x = expression ( paste ( 'Log'[2], 'Wet Weight Biomass (g)') ), 
        y = expression ( paste ( 'Log'[2], 'Abundance (Individuals/m'^2,')' ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          strip.text.x = element_text(size = 10, face = 'bold.italic' ),
          strip.text.y = element_text(size = 10, face = 'bold.italic' ) ) +
  geom_smooth( data = output_for_plot %>% subset ( fitmax & fitmin), 
               method = lm, se = FALSE) + 
  geom_label(data = output_for_plot, aes(x=-7, y=PLACEMENT, label = format ( round( slope, 2 ), 2) ), show.legend = FALSE)
plot_ssa2


# Set working directory for plot #
ggsave('size.spectra.pdf', plot = plot_ssa2, height = 6, width = 6, dpi=600)
pdf('size.spectra.pdf')
print(plot_ssa2)
dev.off()

## BIN BY LAKE ##========================= 
output = left_join(zp.biom, zp.dens, by = c('sampleID', 
                                            'year', 'season',
                                            'doy','taxon')) %>%
  select(sampleID, lake.y, year, season, doy, taxon, group, biomass_g, count, density.areal) %>%
  rename(lake = lake.y) 
output

# Replace lake number with lake name #
levels(output$lake) = c(levels(output$lake), 'Blue','Center', 
                               'Five.Island', 'North.Twin', 
                               'Silver', 'Storm', 'South.Twin')

output$lake[output$lake == '12'] <- 'Blue'
output$lake[output$lake == '19'] <- 'Center' 
output$lake[output$lake == '36'] <- 'Five.Island' 
output$lake[output$lake == '90'] <- 'North.Twin'
output$lake[output$lake == '105'] <- 'Silver'
output$lake[output$lake == '113'] <- 'Storm'
output$lake[output$lake == '406'] <- 'South.Twin'
output$lake

#Separate by lake # 
output
blueout = output %>% filter(lake == 'Blue')
stormout = output %>% filter(lake == 'Storm')
stout = output %>% filter(lake == 'South.Twin')

#Blue# 
log2bins.list = log2bins_butts(blueout$biomass_g)
blueout$BINMID <- as.factor(log2bins.list$indiv$binMid)
blueout$BINMIN <- log2bins.list$indiv$binMin
blueout$BINMAX <- log2bins.list$indiv$binMax
blueout 

#Storm# 
log2bins.list = log2bins_butts(stormout$biomass_g)
stormout$BINMID <- as.factor(log2bins.list$indiv$binMid)
stormout$BINMIN <- log2bins.list$indiv$binMin
stormout$BINMAX <- log2bins.list$indiv$binMax
stormout 

#South Twin#
log2bins.list = log2bins_butts(stout$biomass_g)
stout$BINMID <- as.factor(log2bins.list$indiv$binMid)
stout$BINMIN <- log2bins.list$indiv$binMin
stout$BINMAX <- log2bins.list$indiv$binMax
stout 

centerout = output %>% filter(lake == 'Center') 
fiout = output %>% filter(lake == 'Five.Island')

#Center#
log2bins.list = log2bins_butts(centerout$biomass_g)
centerout$BINMID <- as.factor(log2bins.list$indiv$binMid)
centerout$BINMIN <- log2bins.list$indiv$binMin
centerout$BINMAX <- log2bins.list$indiv$binMax
centerout

#Five Island#
log2bins.list = log2bins_butts(fiout$biomass_g)
fiout$BINMID <- as.factor(log2bins.list$indiv$binMid)
fiout$BINMIN <- log2bins.list$indiv$binMin
fiout$BINMAX <- log2bins.list$indiv$binMax
fiout

ntout = output %>% filter(lake == 'North.Twin')
silverout = output %>% filter(lake == 'Silver')

#North Twin# 
log2bins.list = log2bins_butts(ntout$biomass_g)
ntout$BINMID <- as.factor(log2bins.list$indiv$binMid)
ntout$BINMIN <- log2bins.list$indiv$binMin
ntout$BINMAX <- log2bins.list$indiv$binMax
ntout

#Silver#
log2bins.list = log2bins_butts(silverout$biomass_g)
silverout$BINMID <- as.factor(log2bins.list$indiv$binMid)
silverout$BINMIN <- log2bins.list$indiv$binMin
silverout$BINMAX <- log2bins.list$indiv$binMax
silverout

# Combine outputs together into one - 
output.indv = rbind(blueout, stormout, stout, centerout, fiout, ntout, silverout)
output.indv

#Something in the middle bins is creating high density 
#stripchart(density.areal~BINMID, data=output, vertical = T)

output.combined = output.indv %>% 
  group_by(year, lake, season, BINMID, BINMIN, BINMAX) %>%
  summarize(median.density.areal = median(density.areal, na.rm = T)) %>%
  ungroup()

output.combined

output.combined$BINMID = as.numeric(as.character(output.combined$BINMID))
#BINMIN and BINMAX are included here but will not do anything, 
## but by including they will be carried through to the summary table 
## and will be usuable for relative density

output.combined = as_tibble(output.combined) # Save in a tidy format
output.combined

# log 2 binning # 

output.binned = output.combined %>%
  group_by(year, lake, season, BINMID, BINMIN, BINMAX) %>%
  mutate(BINMID_LOG = log2(BINMID), BINMIN_LOG = log2(BINMIN), BINMAX_LOG = log2(BINMAX), DENS_LOG = log2(median.density.areal))
output.binned

## Linear fits of pelagic size spectrum by lake by year ## 
fits.yr.lm = data.frame ( matrix ( NA, ncol = 16, nrow = length( unique (output.binned$lake)) * (length( unique (output$year) )-1) * (length( unique (output$season) ) + 1) ) )
colnames ( fits.yr.lm ) = c('year', 'lake', 'season','outlierpres', 'n', 'fitmin', 'fitmax', 'slope', 'slp_se', 'slp_l95ci' ,'slp_u95ci', 'intcpt', 'int_se', 'int_l95ci', 'int_u95CI', 'height')

fits.yr.lm$year = rep( sort( unique( output.binned$year)), each = length(unique(output.binned$lake)))
fits.yr.lm$lake = rep( as.factor ( unique (output.binned$lake) ), each = 2)
fits.yr.lm$season = rep(as.character( unique(output.binned$season)), each = 1)

fits.yr.lm$outlierpres = factor ( fits.yr.lm$outlierpres, levels = c('Y', 'N'))
fits.yr.lm

## Set up a short dataframe for easier export ## 
short.output = setNames ( data.frame ( matrix (NA, ncol = 6, nrow = 0) ), c( 'year', 'lake', 'season', 'DENS_LOG', 'BINMID_LOG', 'DATAUSEDINFIT') )

short.output$lake = factor ( short.output$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
short.output$season = factor ( short.output$season, levels = c('spring', 'summer'))

# Remove lake years with no data 
fits.yr.lm = fits.yr.lm %>% filter(!row_number() %in% c(2,16,29,30)) 
fits.yr.lm

## Loop for filling with breaks ## 
for ( i in 1:nrow(fits.yr.lm) ) { 
  
  
  if (fits.yr.lm$season[i] == 'spring' || fits.yr.lm$season[i] == 'summer') {
    tempdata = output.binned %>% 
      filter ( year == fits.yr.lm$year[i] ) %>%
      filter ( lake == fits.yr.lm$lake[i] ) %>%
      filter ( season == fits.yr.lm$season[i] )
    
    templin = lm (DENS_LOG ~ BINMID_LOG, data = tempdata) 
    
    cookcutoff = 10/length(templin$residuals) # Set Cook's value, the general rule is 4/n for detection of influence, 1 is a rough cutoff for points with too much leverage
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, 'dropped', 'used') 
    tempjoin = data.frame ( 'year' = tempdata$year, 'lake' = tempdata$lake, 'season' = tempdata$season,
                            'DENS_LOG' = tempdata$DENS_LOG, 'BINMID_LOG' = tempdata$BINMID_LOG, 'DATAUSEDINFIT' = tempcooks)
    short.output = rbind ( short.output, tempjoin)
    
    if (any ( cooks.distance(templin) > cookcutoff ) ) { # remove poitns with high leverage or just save the lm as it is 
      fits.yr.lm$outlierpres[i] = 'Y' 
      templin = lm ( DENS_LOG [-which ( cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data = tempdata)
    } else {fits.yr.lm$outlierpres[i] = 'N' } # Report that no high leverage points were found 
  }
  
  fits.yr.lm$n[i] = length(templin$residuals) 
  fits.yr.lm$fitmin[i] = min(templin$model$BINMID_LOG, na.rm = T)
  fits.yr.lm$fitmax[i] = max(templin$model$BINMID_LOG, na.rm = T) 
  fits.yr.lm$slope[i] = coef(templin)[[2]] 
  fits.yr.lm$slp_se[i] = coef(summary(templin))[2,2] 
  fits.yr.lm$slp_l95ci[i] = confint(templin, level = 0.95)[2,1]
  fits.yr.lm$slp_u95ci[i] = confint(templin, level=0.95)[2,2]
  fits.yr.lm$intcpt[i] = coef(templin)[[1]]
  fits.yr.lm$int_se[i] = coef(summary(templin))[1,2] 
  fits.yr.lm$int_l95ci[i] = confint(templin, level=0.95)[1,1]
  fits.yr.lm$int_u95CI[i] = confint(templin, level = 0.95)[1,2]
  
  temp.short <- tempjoin %>%
    filter ( DATAUSEDINFIT == 'used') 
  
  fits.yr.lm$height[i] = fits.yr.lm$slope[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lm$intcpt[i]
  
}


fits.yr.lm

### Fits of height from overall ### 
fits.yr.lm$height.overall = fits.yr.lm$slope * ( floor(min(fits.yr.lm$fitmin)) + ceiling(max(fits.yr.lm$fitmax)) ) / 2 + fits.yr.lm$intcpt

## output the data ## 
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")
setwd("J:/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")

#write_csv(fits.yr.lm, 'fitsdata_zooplankton_long.csv')
#write_csv(short.output, 'shortoutput_zooplankton.csv')

## Size Spectra Data ## 
short.output$lake = factor(short.output$lake, levels = c('Blue','Storm', 'South.Twin', 'Center', 'Five.Island', 'North.Twin','Silver'))
plot.data = short.output 
plot.data
color_custom <- c("limegreen", "royalblue2", "gray50")
color_custom2 <- c( "purple", "royalblue2")

#### Size spectrum analysis with different spectra plotted ####
library(ggplot2)

plot_ssa <-  
  ggplot(plot.data,
         aes(BINMID_LOG, DENS_LOG, color=season) ) +
  geom_point ( alpha = 1/2, size=3 ) +
  ylim (8 , 22) +
  xlim (-35, -5) +
  facet_grid ( year ~ lake ) +
  scale_color_manual(values = color_custom2, name = 'Season', labels = c('summer', 'spring')) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          strip.text.x = element_text(size = 12 ),
          strip.text.y = element_text(size = 12 ) )

plot_ssa

#### Size Spectrum analysis of all lakes with slopes ( Need to fix graphics) ####
# Output graphs with linear fits # 
short.output

output_for_plot = short.output %>% 
  mutate(PLACEMENT = ifelse(season == 'spring', 35, ifelse (season == 'summer', 24, 13))) %>%
  left_join(., fits.yr.lm, by = c('year', 'season', 'lake'), all.x=T) # Join makes it easier for ggplot to take the data, of course it creates many rep values 
output_for_plot

options(repr.plot.width = 4, repr.plot.height = 2)
windows(width = 9, height = 4)
plot_ssa2 = 
  ggplot(short.output, 
         aes(BINMID_LOG, DENS_LOG, color=season)) + 
  geom_point( alpha = 1/2, size = 1.5) +
  ylim (8 , 22) +
  xlim (-35, -5) +
  facet_grid(year ~ lake) + 
  labs(title = 'Binned by Lake', font = 2) +
  scale_color_manual(values = color_custom, name = 'season', labels = c('spring', 'summer') ) + 
  
  labs( x = expression ( paste ( 'Log'[2], 'Wet Weight Biomass (g)') ), 
        y = expression ( paste ( 'Log'[2], 'Abundance (Individuals/m'^2,')' ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          strip.text.x = element_text(size = 10, face = 'bold.italic' ),
          strip.text.y = element_text(size = 10, face = 'bold.italic' ) ) +
  geom_smooth( data = output_for_plot %>% subset ( fitmax & fitmin), 
               method = lm, se = FALSE) + 
  geom_label(data = output_for_plot, aes(x=-7, y=PLACEMENT, label = format ( round( slope, 2 ), 2) ), show.legend = FALSE)
plot_ssa2


# Set working directory for plot #
ggsave('size.spectra.pdf', plot = plot_ssa2, height = 6, width = 6, dpi=600)
pdf('size.spectra.pdf')
print(plot_ssa2)
dev.off()

