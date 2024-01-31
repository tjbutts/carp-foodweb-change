# ZOOPLANKTON DENSITY ================================================
# Purpose: this script is designed to convert the length and count data for zooplankton samples in to biomass data, based on the allometric equations provided by the Downing Lab
# Script originally developed by Dr. Eric Moody - June 2017
# Updated by Dr. Grace Wilkinson - Feb 2021 
# Modified to collect density information on zooplankton 

#=============================
# STEP 1: Place the files in a working directory
# Place all of the count files into a folder, no other files should be in this folder
# DO NOT put the sample log into this folder, only count files
# Make sure the files are saved as .csv
# Make sure the file names are consistent (they SHOULD just be the 12 character sample ID)
# Set the working directory in R to this folder

setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files/Count Files")

#=============================
# STEP 2: Set up empty vectors that will be filled using a for loop
filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
DOY <- c()
TAXON <- c()
COUNT <- c()


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
  
  #=================================
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
  
  #Trichocerca
  DensitySubsample[44]<-file$Count[46]
  
  #Trichotria
  DensitySubsample[45]<-file$Count[48]
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
  # SAMPLE IDS =================
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., P18012243)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,9),45)) #46 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),45)) #lake number is the 4-6th spot
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),45)) #DOY is the 7-9th spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  COUNT <- append(COUNT, DensitySubsample)
}

#============================================
# STEP 7: MAKE A USEFUL OUTPUT

# Start by making a data frame of the useful info from above
Zoop.carp.dens<-data.frame(SAMPLE.ID, LAKE.NO, DOY, TAXON, COUNT)

# Use the tidyverse to add more columns
library(tidyverse)
Zoop.carp.dens = Zoop.carp.dens %>%
  # Create a new column ("GROUP") that creates the broader taxonomic groups for analysis
  mutate(GROUP = case_when(.$TAXON %in% c("Alona",
                                          "Alonella",
                                          "Bosmina",
                                          "Chydorus",
                                          "Pleuroxus") ~ "SmCladocera",
                           .$TAXON %in% c("Camptocercus",
                                          "Ceriodaphnia",
                                          "Daphnia",
                                          "Daphnia.lumholtzi",
                                          "Diaphanosoma",
                                          "Graptoleberis",
                                          "Leptodora",
                                          "Moina",
                                          "Scapholeberis",
                                          "Simocephalus") ~ "LgCladocera",
                           .$TAXON %in% c("Anuraeopsis",
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
                           .$TAXON %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$TAXON %in% c("Calanoida") ~ "Calanoid",
                           .$TAXON %in% c("Nauplii") ~ "Nauplii"))

# Correct for volume 
#============================================
# STEP 5: Read in the sample log
# In the parentheses below, put the full path to the sample log file
# A quick way to get this path is to right click on the file, go to "Properties", and copy the path -- NOTE THAT THE SLASHES IN THE PATH MUST BE "/"
setwd("C:/Users/tjbut/Box Sync/Carp Zooplankton Files")
zvol<-read.csv('CarpZoopLOG_missingsamplesremoved.csv')

names(zvol) #Check that your column headers are correct and match the headers listed above
zvol$SAMPLEID #Spits out a list of all of the sample IDs so you can double check

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
# JOIN VOLUME TO DENSITY DATA 
Zoop.dens = Zoop.carp.dens %>%
  rename(SAMPLEID = SAMPLE.ID, doy = DOY, count = COUNT) %>%
  mutate(doy = as.integer(doy)) %>%
  mutate(count = replace_na(COUNT, 0)) %>%
  as_tibble()
Zoop.dens
zvol = rename(.data = zvol, doy = DOY)
carp_dens_join = left_join(Zoop.dens, zvol, by = c('SAMPLEID', 'doy'))
carp_dens_join

#============================================
# STEP 6: Correct the densities from Step 3 for volume of water towed and sample volume

WI.NET.AREA<-0.0113 #m^2 ##this is the current diameter of the nets we use##
nsample<-length(zvol$SAMPLEID)
SSVOL<-c()
SVOL<-c()
TVOL<-c()
PROCDATE<-c()
PROCBY<-c()
for(i in c(1:nsample)){
  ssvol<-rep((zvol$VOLUMECOUNTED[i]/1000),46) #46 = number of taxa
  SSVOL<-append(SSVOL,ssvol)
  svol<-rep((zvol$SAMPLEVOLUME[i]/1000),46) #46 = number of taxa
  SVOL<-append(SVOL,svol)
  tvol<-rep(zvol$TOW[i], 46) #46 = number of taxa
  TVOL<-append(TVOL,tvol)
}
TVOLF<-(WI.NET.AREA*TVOL)*1000

density.indv.l<-c()
for(i in c(1:length(COUNT))){
  density.indv.l[i]<-(COUNT[i]*SVOL[i])/(SSVOL[i]*TVOLF[i])
}

# Create a CSV file of the data frame -- NOTE: this will be in the working directory
# Will need to remove file from working directory before re-running the analysis
carp.zoop.density = carp_dens_join %>%
  select(SAMPLEID, LAKE.NO, doy, TAXON, count) %>%
  mutate(density = density.indv.l) %>%
  mutate(density = replace_na(density, 0))
carp.zoop.density

#write.csv(Zoop.gv.dens,"2019_site4_gv_ZoopDensity_25Feb2021.csv", row.names = F)




