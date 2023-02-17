## Clean data to filter out Carp Lakes ## 
library(lubridate)
library(tidyverse)
setwd("C:/Users/tjbut/Box Sync/Iowa Data/Biology Data/Zooplankton/2018 Zooplankton ALM")

# 2018
zoop.biomass.L.2018 = read_csv('2018 Zooplankton.csv')
zoop.biomass.L.2018

setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change/ALM Zoops/2018 ALM zoop files")

# 2019 - Have log sheet can do normal process # 



## Add 2019 ALM Biomass Data ##========================== 
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change/ALM Zoops/2019 ALM zoop files")


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
alm.zoop.carp<-data.frame(SAMPLE.ID, LAKE.NO, YEAR,  DOY, TAXON, BIOMASS.UG)
alm.zoop.carp

# Replace NAs with 0s 
alm.zoop.carp[is.na(alm.zoop.carp)] <- 0
alm.zoop.carp = as_tibble(alm.zoop.carp)
alm.zoop.carp
alm.zoop.carp$YEAR[alm.zoop.carp$YEAR == 19] <- 2019
alm.zoop.carp = alm.zoop.carp %>% 
  filter(LAKE.NO == '012' | 
           LAKE.NO == '019' | 
           LAKE.NO == '036' |
           LAKE.NO == '090' |
           LAKE.NO == '105' | 
           LAKE.NO == '113' |
           LAKE.NO == '406') %>% 
  mutate(LAKE.NO = as.double(LAKE.NO), 
         DOY = as.double(DOY))
alm.zoop.carp

## Add Season Column ## 
## 2018: Spring = 060 - 181; Summer = 182 - 273
## 2019: Spring = 060 - 181; Summer = 182 - 273
## 2020: Spring = 061 - 182; Summer = 183 - 274

# Example Tidyverse Code to make the zoop data wide and summarize by GROUP
blue19.zoop.biomass = alm.zoop.carp %>%
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
  filter(avg_biomass_ug != 0) %>% 
  filter(lake == '12')
blue19.zoop.biomass

# Add ALM 2019 Areal Density ##=========================
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change/ALM Zoops/2019 ALM zoop files")

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
alm.zoop.carp.dens<-data.frame(SAMPLE.ID, LAKE.NO, YEAR,  DOY, TAXON, COUNT)
alm.zoop.carp.dens
# Replace NAs with 0s 
alm.zoop.carp.dens[is.na(alm.zoop.carp.dens)] <- 0
alm.zoop.carp.dens = as_tibble(alm.zoop.carp.dens)
alm.zoop.carp.dens
alm.zoop.carp.dens$YEAR[alm.zoop.carp.dens$YEAR == 18] <- 2018
alm.zoop.carp.dens$YEAR[alm.zoop.carp.dens$YEAR == 19] <- 2019
alm.zoop.carp.dens$YEAR[alm.zoop.carp.dens$YEAR == 20] <- 2020
alm.zoop.carp.dens$LAKE.NO[alm.zoop.carp.dens$LAKE.NO == '012'] <- '12'
alm.zoop.carp.dens$LAKE.NO[alm.zoop.carp.dens$LAKE.NO == '019'] <- '19'
alm.zoop.carp.dens$LAKE.NO[alm.zoop.carp.dens$LAKE.NO == '036'] <- '36' 
alm.zoop.carp.dens$LAKE.NO[alm.zoop.carp.dens$LAKE.NO == '090'] <- '90' 
alm.zoop.carp.dens

#12, 19, 36, 90

## Add Season Column ## 
## 2018: Spring = 060 - 181; Summer = 182 - 273
## 2019: Spring = 060 - 181; Summer = 182 - 273
## 2020: Spring = 061 - 182; Summer = 183 - 274

# Example Tidyverse Code to make the zoop data wide and summarize by GROUP
alm.zoop.carp.density = alm.zoop.carp.dens %>%
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
  filter(count != 0) %>% 
  filter(lake == '12')
  
alm.zoop.carp.density


# STEP 5: Read in the sample log
# In the parentheses below, put the full path to the sample log file
# A quick way to get this path is to right click on the file, go to "Properties", and copy the path -- NOTE THAT THE SLASHES IN THE PATH MUST BE "/"
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change/ALM Zoops")
alm.zvol<-read_csv("2019ALMZooLog.csv") %>%
  rename(sampleID = SampleID, 
         lake = Lake_No,
         doy = DOY) %>% 
  mutate(lake = as.factor(lake)) %>% 
  as_tibble()
alm.zvol
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
alm.zvol
alm.zvol.clean = alm.zvol %>%
  mutate(doy = as.double(doy)) %>%
  mutate(sampleID = substr(sampleID, 1, 9)) %>% 
  filter(lake == '12')
alm.zvol.clean

# JOIN VOLUME TO DENSITY DATA 
alm.zoop.carp.density
alm.zvol.clean = left_join(alm.zoop.carp.density, alm.zvol.clean, by = c('sampleID', 'doy', 'lake')) %>%
  rename(VOLUMECOUNTED = Subsample_Vol, 
         SAMPLEVOLUME = Sample_Vol, 
         TOW = TOW.DEPTH)
alm.zvol.clean # Missing volume information in here 


# STEP 6: Correct the densities from Step 3 for volume of water towed and sample volume
alm.zvol.clean

WI.NET.AREA<-0.0113 #m^2 ##this is the current diameter of the nets we use##

alm.dens.raw = alm.zvol.clean %>% 
  select(sampleID, lake, year, doy, taxon, group, season, count, SAMPLEVOLUME, VOLUMECOUNTED, TOW) %>%
  mutate(SSVOL = VOLUMECOUNTED/1000, 
         SVOL = SAMPLEVOLUME/1000) %>%
  mutate(TVOLF = (WI.NET.AREA*TOW)*1000) %>% 
  mutate(density.indv.l = (count*SVOL)/(SSVOL*TVOLF))
alm.dens.raw

# Create a CSV file of the data frame -- NOTE: this will be in the working directory
# Will need to remove file from working directory before re-running the analysis
blue19.density = alm.dens.raw %>%
  select(sampleID, lake, year, doy, taxon, group, count, season, density.indv.l, TOW) %>% 
  rename(tow = TOW) %>%
  mutate(density.areal = density.indv.l*1000*tow) %>%  # Key info for this analysis L -> m^3 = *1000
  mutate(lake = as.factor(lake), 
         year = as.factor(year), 
         season = as.factor(season))
blue19.density

# Average weight x density 
blue19.zoop.biomass
blue19.density

blue19.weight.density = left_join( blue19.zoop.biomass, blue19.density) %>% 
  select(sampleID, lake, year, doy, taxon, group, season, count, density.areal, avg_biomass_ug) %>% 
  mutate(biomass_g = avg_biomass_ug*(10^-6))
blue19.weight.density

# Make a dataset # 
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change/ALM Zoops")
blue19 = write_csv(blue19.weight.density, 'blue19.zoop.csv') 
blue19
