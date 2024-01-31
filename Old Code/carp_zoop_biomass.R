# Calculate zooplankton biomass # 

rm(list = ls())

## Calculate Zooplankton Biomass ## 
# ZOOPLANKTON BIOMASS ================================================
# Purpose: this script is designed to convert the length and count data for zooplankton samples in to biomass data, based on the allometric equations provided by the Downing Lab
# Script originally developed by Dr. Eric Moody - June 2017
# Updated by Dr. Grace Wilkinson - Feb 2021

#=============================
# STEP 1: Place the files in a working directory
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files/Count Files")
# Place all of the count files into a folder, no other files should be in this folder
# DO NOT put the sample log into this folder, only count files
# Make sure the files are saved as .csv
# Make sure the file names are consistent (they SHOULD just be the 12 character sample ID)
# Set the working directory in R to this folder

#=============================
# STEP 2: Set up empty vectors that will be filled using a for loop
filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
DOY <- c()
TAXON <- c()
BIOMASS.UG <- c()


#=============================
# STEP 3: Use a for loop to fill the vectors above based on the information in the count files
# Note that this is set up to work with the exact column headers and row order for the
# ZooCountTemplate file used in the lab - another format will not work with this script

############ NOTE: RUN THE ENTIRE FOR-LOOP AT ONCE, LINES 27-296

i<-1
for (i in c(1:numfiles)){  
  filenames[i] <- paste(filenames[i],sep="")  
  file<- read.csv(filenames[i])
  
  #This is the list of taxa that we identify and the order that they are in for the rows
  #in the ZooCountTemplate; there are 45 taxa in this list
  Taxon<-c("Alona","Alonella","Bosmina","Camptocercus","Ceriodaphnia","Chydorus",
           "Daphnia","Daphnia.lumholtzi","Diaphanosoma","Graptoleberis","Leptodora",
           "Moina","Pleuroxus","Scapholeberis","Simocephalus","Calanoida","Cyclopoida",
           "Nauplii","Anuraeopsis","Ascomorpha","Asplanchna","Asplanchnopus",
           "Brachionus","Conochilus","Euchlanis","Filinia","Gastropus","Hexarthra",
           "Kellicottia","Keratella.cochlearis","Keratella.quadrata","Lecane",
           "Lepadella","Macrochaetus","Monostyla","Mytilina","Notholca","Platyias",
           "Ploesoma","Polyarthra","Pompholyx","Synchaeta","Testudinella",
           "Trichocerca","Trichotria")
  
  #Calculate the biomass in micrograms per liter by converting the length and counts
  #for each taxa using the allometric equations below
  #Note that the equations are specific to each taxa, and reference the indexed list of 
  #taxon above (n=1-45)
  
  z<-c()
  y<-c()
  x<-c()
  BiomassSubsample<-c()
  
  #Alona 
  BiomassSubsample[1]<-mean(as.numeric(
    (15.92*(file[1,5:length(names(file))]/1000)^3.84)),na.rm=TRUE)*file$Count[1]
  
  #Alonella
  BiomassSubsample[2]<-mean(as.numeric(
    (15.92*(file[2,5:length(names(file))]/1000)^3.84)),na.rm=T)*file$Count[2]
  
  #Bosmina
  BiomassSubsample[3]<-mean(as.numeric(
    (26.6*(file[3,5:length(names(file))]/1000)^3.13)),na.rm=T)*file$Count[3]
  
  #Camptocercus
  BiomassSubsample[4]<-mean(as.numeric(
    (15.92*(file[4,5:length(names(file))]/1000)^3.84)),na.rm=T)*file$Count[4]
  
  #Ceriodaphnia
  BiomassSubsample[5]<-mean(as.numeric(
    (1.76*10^-6)*(file[5,5:length(names(file))]^2.26)),na.rm=T)*file$Count[5]
  
  #Chydorus
  BiomassSubsample[6]<-mean(as.numeric(
    (89.43*(file[6,5:length(names(file))]/1000)^3.03)),na.rm=T)*file$Count[6]
  
  #Daphnia
  BiomassSubsample[7]<-mean(as.numeric(
    (1.5*10^-8)*(file[7,5:length(names(file))]^2.84)),na.rm=T)*file$Count[7]
  
  #Daphnia.lumholtzi
  BiomassSubsample[8]<-mean(as.numeric(
    (1.5*10^-8)*(file[8,5:length(names(file))]^2.84)),na.rm=T)*file$Count[8]
  
  #Diaphanosoma
  BiomassSubsample[9]<-mean(as.numeric(
    (1.76*10^-6)*(file[9,5:length(names(file))]^2.11)),na.rm=T)*file$Count[9]
  
  #Graptoleberis
  BiomassSubsample[10]<-mean(as.numeric(
    (15.92*(file[10,5:length(names(file))]/1000)^3.84)),na.rm=T)*file$Count[10]
  
  #Leptodora
  BiomassSubsample[11]<-exp(mean(as.numeric(
    (-0.822+2.76*log(file[11,5:length(names(file))]/1000))),na.rm=T)*file$Count[11])
  
  #Moina
  BiomassSubsample[12]<-mean(as.numeric(
    (6.61*(file[12,5:length(names(file))]/1000)^2.57)),na.rm=T)*file$Count[12]
  
  #Pleuroxus
  BiomassSubsample[13]<-mean(as.numeric(
    (35.6*(file[13,5:length(names(file))]/1000)^4.03)),na.rm=T)*file$Count[13]
  
  #Scapholeberis
  BiomassSubsample[14]<-mean(as.numeric(
    (8.9*10^-8)*(file[14,5:length(names(file))]^2.7)),na.rm=T)*file$Count[14]
  
  #Simocephalus
  BiomassSubsample[15]<-mean(as.numeric(
    (7.43*(file[15,5:length(names(file))]/1000)^3.28)),na.rm=T)*file$Count[15]
  
  #Calanoida
  BiomassSubsample[16]<-mean(as.numeric(
    (7.9*10^-7)*(file[16,5:length(names(file))]^2.33)),na.rm=T)*file$Count[16]
  
  #Cyclopoida
  BiomassSubsample[17]<-mean(as.numeric(
    (1.1*10^-7)*(file[17,5:length(names(file))]^2.59)),na.rm=T)*file$Count[17]
  
  #Nauplii
  BiomassSubsample[18]<-mean(as.numeric(
    (1.1*10^-5)*(file[18,5:length(names(file))]^1.89)),na.rm=T)*file$Count[18]
  
  #=================================
  # ROTIFERS
  
  #Anuraeopsis
  BiomassSubsample[19]<-((0.1*(mean(as.numeric(
    (0.03*(file[19,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[19]
  
  #Ascomorpha
  BiomassSubsample[20]<-((0.1*(mean(as.numeric(
    (0.12*(file[20,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[20]
  
  #Asplanchna
  BiomassSubsample[21]<-((0.039*(mean(as.numeric(
    (0.23*(file[21,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[21]
  
  #Asplanchnopus
  BiomassSubsample[22]<-((0.039*(mean(as.numeric(
    (0.23*(file[22,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[22]
  
  #Brachionus
  BiomassSubsample[23]<-((0.1*(mean(as.numeric(
    (0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[23]
  
  #Conochilus
  #NOTE: two measurements for this genus, hence the shift in indexing
  BiomassSubsample[24]<-(sum((0.16*file[24,5:length(names(file))]/1000*
                                (file[25,5:length(names(file))]/1000)^2)/1000,na.rm=T))*10^6
  
  #Euchlanis
  BiomassSubsample[25]<-((0.1*(mean(as.numeric(
    (0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[26]
  
  #Filinia
  BiomassSubsample[26]<-((0.1*(mean(as.numeric(
    (0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.01*(mean(as.numeric((0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[27]
  
  #Gastropus
  BiomassSubsample[27]<-((0.1*(mean(as.numeric(
    (0.2*(file[28,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[28]
  
  #Hexarthra
  BiomassSubsample[28]<-((0.1*(mean(as.numeric(
    (0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.33*(mean(as.numeric((0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[29]
  
  #Kellicotia
  BiomassSubsample[29]<-((0.1*(mean(as.numeric(
    (0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+ 
      0.015*(mean(as.numeric((0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[30]
  
  #Keratella.cochlearis
  BiomassSubsample[30]<-((0.1*(mean(as.numeric(
    (0.02*(file[31,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[31]
  
  #Keratella.quadrata
  BiomassSubsample[31]<-((0.1*(mean(as.numeric(
    (0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[32]
  
  #Lecane
  BiomassSubsample[32]<-((0.1*(mean(as.numeric(
    (0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[33]
  
  #Lepadella
  BiomassSubsample[33]<-((0.1*(mean(as.numeric(
    ((file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.05*(mean(as.numeric((0.1*(file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[34]
  
  #Macrochaetus
  BiomassSubsample[34]<-((0.1*(mean(as.numeric(
    (0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[35]
  
  #Monostyla
  BiomassSubsample[35]<-((0.1*(mean(as.numeric(
    (0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[36]
  
  #Mytilina
  a<-as.numeric(file[37,5:length(names(file))])/1000
  b<-as.numeric(file[38,5:length(names(file))])/1000
  c<-((0.52*a*b^2)+(0.6*0.52*a*b^2))/1000
  BiomassSubsample[36]<-(mean(as.numeric(c),na.rm=T)*10^6)*file$Count[36]
  
  #Notholca
  BiomassSubsample[37]<-((0.1*(mean(as.numeric(
    (0.035*(file[39,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[39]
  
  #Platyias
  BiomassSubsample[38]<-(((mean(as.numeric(
    (0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +(mean(as.numeric((0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[40]
  
  #Ploesoma
  BiomassSubsample[39]<-((0.1*(mean(as.numeric(
    (0.23*(file[41,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[41]
  
  #Polyarthra
  BiomassSubsample[40]<-((0.1*(mean(as.numeric(
    (0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[42]
  
  #Pompholyx
  BiomassSubsample[41]<-((0.1*(mean(as.numeric(
    (0.15*(file[43,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[43]
  
  #Synchaeta
  BiomassSubsample[42]<-((0.1*(mean(as.numeric(
    (0.1*(file[44,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)*file$Count[44]
  
  #Testudinella
  BiomassSubsample[43]<-((0.1*(mean(as.numeric(
    (0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[45]
  
  #Trichocerca
  x<-as.numeric(file[46,5:length(names(file))])/1000
  y<-as.numeric(file[47,5:length(names(file))])/1000
  z<-((0.52*x*y^2)+(0.6*0.52*x*y^2))/1000
  BiomassSubsample[44]<-sum(z,na.rm=T)*10^6
  
  #Trichotria
  BiomassSubsample[45]<-(((mean(as.numeric(
    (0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)*file$Count[48]
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
  # SAMPLE IDS =================
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., A19114204001)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,9),45)) #45 is the number of taxa
  
  #If the file names are saved as the 9-character sampleID (e.g., A19114204) use this:
  # SampleID<-(rep(substr(filenames[i],1,9),45)) #45 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),45)) #lake number
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),45)) #DOY is the 7-9th spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  BIOMASS.UG <- append(BIOMASS.UG, BiomassSubsample)
  
}

#============================================
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

#============================================
# STEP 5: Read in the sample log
# In the parentheses below, put the full path to the sample log file
# A quick way to get this path is to right click on the file, go to "Properties", and copy the path -- NOTE THAT THE SLASHES IN THE PATH MUST BE "/"

#Set zvol as below, but put in the working directory where the files are located 
#zvol<-read.csv('C:/Users/wilkinso/Box/Carp Project Iowa DNR/Carp Zooplankton Files/CarpZoopLOG.csv')
#zvol<-read.csv('J:/Box Sync/Carp Zooplankton Files/CarpZoopLOG_missingsamplesremoved.csv')
zvol<-read.csv('C:/Users/tjbut/Box Sync/Carp Zooplankton Files/CarpZoopLOG_missingsamplesremoved.csv')
names(zvol) #Check that your column headers are correct and match the headers listed above
zvol$SAMPLEID #Spits out a list of all of the sample IDs so you can double check

#============================================
# STEP 6: Correct the biomasses from Step 3 for volume of water towed and sample volume

WI.NET.AREA<-0.0113 #m^2 ##this is the current diameter of the nets we use##
nsample<-length(zvol$SAMPLEID)
SSVOL<-c()
SVOL<-c()
TVOL<-c()
PROCDATE<-c()
PROCBY<-c()
for(i in c(1:nsample)){
  ssvol<-rep((zvol$VOLUMECOUNTED[i]/1000),45) #45 = number of taxa
  SSVOL<-append(SSVOL,ssvol)
  svol<-rep((zvol$SAMPLEVOLUME[i]/1000),45) #45 = number of taxa
  SVOL<-append(SVOL,svol)
  tvol<-rep(1,45) #45 = number of taxa
  TVOL<-append(TVOL,tvol)
}
TVOLF<-(WI.NET.AREA*TVOL)*1000

BIOMASS.UG.L<-c()
for(i in c(1:length(BIOMASS.UG))){
  BIOMASS.UG.L[i]<-(BIOMASS.UG[i]*SVOL[i])/(SSVOL[i]*TVOLF[i])
}

#============================================
# STEP 7: MAKE A USEFUL OUTPUT
library(tidyverse)
# Start by making a data frame of the useful info from above
Zoop.carp<-data.frame(SAMPLE.ID, LAKE.NO, DOY, TAXON, BIOMASS.UG.L)
# Replace NAs with 0s 
Zoop.carp[is.na(Zoop.carp)] <- 0
Zoop.carp = as_tibble(Zoop.carp)
Zoop.carp

# Solely a carp data frame # 
# Create a CSV file of the data frame -- NOTE: this will be in the working directory
# Will need to remove file from working directory before re-running the analysis
# write.csv(Zoop.carp,"2018-2020_Carp_ZoopBiomass_21May2021.csv")

#####===========================
# STEP 8: CLEAN UP DATA FRAME # 
# Example Tidyverse Code to make the zoop data wide and summarize by GROUP
zoop.carp = Zoop.carp %>%
  rename(sampleID = SAMPLE.ID,
         lake = LAKE.NO, 
         taxon = TAXON, 
         doy = DOY, 
         biomass = BIOMASS.UG.L) %>%
  mutate(year = substr(sampleID, # Select the characters of the sampleID that indicate year
                       nchar(sampleID)-8+1, 
                       nchar(sampleID)-7+1)) %>% 
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
                                          "Trichocerca",
                                          "Trichotria") ~ "Rotifer",
                           .$taxon %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$taxon %in% c("Calanoida") ~ "Calanoid",
                           .$taxon %in% c("Nauplii") ~ "Nauplii"))
zoop.carp
zoop.carp.cleaned = zoop.carp %>%
  select(sampleID, lake, doy,year, group, taxon, biomass) %>% 
  mutate(year = as.numeric(year), 
         doy = as.numeric(doy))
zoop.carp.cleaned$year[zoop.carp.cleaned$year == 18] <- 2018
zoop.carp.cleaned$year[zoop.carp.cleaned$year == 19] <- 2019
zoop.carp.cleaned$year[zoop.carp.cleaned$year == 20] <- 2020
zoop.carp.cleaned = zoop.carp.cleaned %>%
  mutate(year = as.numeric(year), 
         doy = as.numeric(doy))

# Add in ALM Biomass data #==================
# STEP 9: ADD IN ALM ZOOPLANKTON DATA POINTS 
library(lubridate)
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files")
alm_carpzoops = read_csv('ALM Zooplankton_Carp Lakes Only.csv')
alm_carpzoops_cleaned = alm_carpzoops %>% 
  mutate(date = mdy(date)) %>% 
  mutate(doy = yday(date), 
         year = year(date)) %>% 
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
                                          "Trichocerca",
                                          "Trichotria") ~ "Rotifer",
                           .$taxon %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$taxon %in% c("Calanoida") ~ "Calanoid",
                           .$taxon %in% c("Nauplii") ~ "Nauplii", 
                           .$taxon %in% c("Ostracod") ~ "Ostracod")) %>%
  select(sampleID, lake, doy, year, group, taxon, biomass)
alm_carpzoops_cleaned

# Add alm zooplankton data to the carp lakes data using rbind 
zoop.carp.cleaned
# Add in lake names
zoop.carp.cleaned$lake[zoop.carp.cleaned$lake == '012'] <- 'Blue'
zoop.carp.cleaned$lake[zoop.carp.cleaned$lake == '019'] <- 'Center' 
zoop.carp.cleaned$lake[zoop.carp.cleaned$lake == '036'] <- 'Five.Island' 
zoop.carp.cleaned$lake[zoop.carp.cleaned$lake == '090'] <- 'North.Twin'
zoop.carp.cleaned$lake[zoop.carp.cleaned$lake == '105'] <- 'Silver'
zoop.carp.cleaned$lake[zoop.carp.cleaned$lake == '113'] <- 'Storm'
zoop.carp.cleaned$lake[zoop.carp.cleaned$lake == '406'] <- 'South.Twin'
zoop.carp.cleaned

alm_carpzoops_cleaned
alm_carpzoops_cleaned$lake = as.character(alm_carpzoops_cleaned$lake)
alm_carpzoops_cleaned$lake[alm_carpzoops_cleaned$lake == '12'] <- 'Blue'
alm_carpzoops_cleaned$lake[alm_carpzoops_cleaned$lake == '19'] <- 'Center' 
alm_carpzoops_cleaned$lake[alm_carpzoops_cleaned$lake == '36'] <- 'Five.Island' 
alm_carpzoops_cleaned$lake[alm_carpzoops_cleaned$lake == '90'] <- 'North.Twin'
alm_carpzoops_cleaned$lake[alm_carpzoops_cleaned$lake == '105'] <- 'Silver'
alm_carpzoops_cleaned$lake[alm_carpzoops_cleaned$lake == '113'] <- 'Storm'
alm_carpzoops_cleaned$lake[alm_carpzoops_cleaned$lake == '406'] <- 'South.Twin'
alm_carpzoops_cleaned

zoop.carp.final = rbind(zoop.carp.cleaned, alm_carpzoops_cleaned) %>% 
  arrange(lake, doy)
final_carpzoops = zoop.carp.final %>% 
  mutate(season = case_when(.$year == '2018' & .$doy <182 & .$doy >59 ~ 'spring', 
                          .$year == '2018' & .$doy >181 ~ 'summer', 
                          .$year == '2019' & .$doy <182 & .$doy >59 ~ 'spring', 
                          .$year == '2019' & .$doy >181 ~ 'summer',
                          .$year == '2020' & .$doy <183 & .$doy >59 ~ 'spring', 
                          .$year == '2020' & .$doy >182 ~ 'summer')) %>%
  mutate(season = as.factor(season)) %>%
  select(sampleID, lake, doy, year, season, group, taxon, biomass)
final_carpzoops

setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")
#write.csv(final_carpzoops, file = "carpzoops_final.csv") 

