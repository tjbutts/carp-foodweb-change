#As a rule databases are all lowercase [example], columns are all upper case [EXAMPLE], 
#and parameters are uppercase proceeded by a period [.EXAMPLE]
#WORKS WITH R 3.6.0 AND EARLIER!!

# Control - Shift - O brings up an overview in RStudio that allows you to jump to any section of code 

#Because of the complexity of modern R programs I strongly recommend closing and reopening RStudio or at least clearing the cache before you run this code
#Running this code after other things have been run could result in errors, also remember order matters.

#### Libraries to load ####
library(readxl)
library(plyr)
library(tidyr)
library(dplyr)  #This will throw errors because tidyverse masks standard cmds, ignore them. The tidy cmds are better than base cmds.
library(ggplot2)

#### File extension locations ####
.DATA_INPUT        <- "C:/Users/bottl/Box/CIGLR Size structure project/Data for analysis/"
.DATA_OUTPUT       <- "C:/Users/bottl/Box/CIGLR Size structure project/Manuscripts/Manuscript Pelagic_benthic/Analysis/Data output/"
.FIGURE_OUTPUT_LOC <- "C:/Users/bottl/Box/CIGLR Size structure project/Manuscripts/Manuscript Pelagic_benthic/Analysis/Figures/"

#### Equations to load from Edwards ####
source("C:\\Users\\bottl\\Box\\CIGLR Size structure project\\Data for analysis\\fitting-size-spectra\\code\\PLBfunctions.r")

#### Phyto data ####
phyt.data <- read_excel( paste ( sep="", .DATA_INPUT, "MASTER_01-16phytodata 05_04_2019.xlsx"), sheet = "01-13" ) %>% select_all ( toupper )

##### Station data ####
depth.data <- read_excel( paste ( sep="", .DATA_INPUT, "WQSstns_Masterlist.xlsx"), sheet = "All") %>% 
  select (STATION2, DEPTH) %>% 
  mutate ( STATION = gsub(" ", "", STATION2, fixed = TRUE) ) %>%
  select (-STATION2)

#### Annie Chl data ####
chla.data <- read_excel ( paste ( sep="", .DATA_INPUT,"chlorophyll_forSizeSpectra_AES.xlsx"), sheet = 'chlorophyll_forSizeSpectra', na="NA") %>% 
  select_all ( toupper ) %>%
  separate( "STATION", c("STATION", "DISCARD"), sep=4) %>%  #get rid of those dumb m's
  select ( -STATION_YEAR, -DISCARD ) %>%
  mutate ( YEARSTATION = paste (YEAR, STATION, sep="") )

#### Process phyto data ####
phyt.work <- phyt.data %>%
  select ( -NUM ) %>%
  mutate ( STATION =  gsub ( " ", "", STATION ) ) %>%
  separate( "STATION", c("STATION", "DISCARD"), sep=4) %>%  #get rid of those dumb m's
  select ( -DISCARD ) %>% 
  filter ( !grepl ( "FE|FO", STATION ) ) %>%
  filter ( !grepl ( "SUME|SUSE", STATION ) ) %>% #This maybe temporary as these sites might not be in final file
  select ( "YEAR", "LAKE", "SEASON", "DIV", "SAMPLE", "STATION", "SAMPLETYPE", "CELLTALLY", "CELLSPERML", "BIOVOL", "BIOVOLPERIND", "CELLSMEASURED", "COLONYTALLY" ) %>%
  mutate ( YEARSTATION = paste (YEAR, STATION, sep="") ) %>%
  filter ( YEAR > 2005 ) %>%
  filter ( CELLSPERML != 0 ) #there are a number of zeroes that need to be removed

phyt.work$LAKE <- ifelse ( phyt.work$STATION %in% c("ER09", "ER10", "ER15", "ER15", "ER63"), "Erie-East", 
                           ifelse ( phyt.work$STATION %in% c("ER30", "ER31", "ER32", "ER36", "ER37", "ER38", "ER42", "ER43", "ER73", "ER78", "ER78"), "Erie-Central",
                                    ifelse ( phyt.work$STATION %in% c("ER58", "ER59", "ER60", "ER61", "ER91", "ER92"), "Erie-Western", 
                                             phyt.work$LAKE ) ) ) #this is for Lake Erie

phyt.work$LAKE <- ifelse ( phyt.work$LAKE == "HU", "Huron", 
                           ifelse ( phyt.work$LAKE == "MI", "Michigan",
                                    ifelse ( phyt.work$LAKE == "ON", "Ontario",
                                             ifelse ( phyt.work$LAKE == "SU", "Superior", phyt.work$LAKE ) ) ) ) 

#### Combine datasets ####
phyt.work <- left_join( phyt.work, depth.data, by="STATION" )

#### Calculate biovolume and combine ####
bio.vol <- setNames( aggregate ( BIOVOL ~ YEAR + LAKE + STATION , data=phyt.work, sum ),
                     c("YEAR", "LAKE", "STATION", "BIOVOLSUM") )
bio.vol$YEARSTATION <- paste( bio.vol$YEAR, bio.vol$STATION, sep="" )

chla.work <- chla.data %>% 
  select ( -LAKE, -STATION, -YEAR ) %>%
  left_join( phyt.work, ., "YEARSTATION" )

chla.work <- bio.vol %>% 
  select ( -YEAR, -LAKE, -STATION ) %>% 
  left_join ( chla.work, ., by = "YEARSTATION")

#### Calculate the biovolumes and cell densities ####
chla.work <- chla.work %>%
  mutate( EST_BELOWTHERM_AVG = (CHL_BELOWTHERM_TOTAL / CHL_ABOVETHERM_TOTAL) * CHL_ABOVETHERM_AVG ) %>%
  mutate( EST_BIOVOL = (BIOVOL / BIOVOLSUM) * (EST_BELOWTHERM_AVG / 1000) ) %>% #in ug/mL after adjusting below avg down to mL from L, calculates for all types, but really only important for DCL, we will deal with this
  mutate( CALC_CELLSPERML = EST_BIOVOL / BIOVOLPERIND ) %>%
  mutate( EST_BIOMASS = ifelse ( SAMPLETYPE == "DCL", EST_BIOVOL * 10^6 * (DEPTH - THERM_M), 
                                 ifelse ( SEASON == "SUMMER" & SAMPLETYPE == "INT", BIOVOL * 10^6 * THERM_M, BIOVOL * 10^6 * DEPTH ) ) ) %>% #convert mL to m^3 and the boimass to m^2
  mutate ( BIOVOLPERIND_UG = BIOVOLPERIND / 10^6 ) %>% #assume 1 um^3/cell = 1g/mL , which means divide by 10^12 um^3 and multiply by 10^6 g
  mutate( CELLSPERSQM = ifelse ( SAMPLETYPE == "DCL", CALC_CELLSPERML * 10^6 * (DEPTH - THERM_M), 
                                 ifelse ( SEASON == "SUMMER" & SAMPLETYPE == "INT" & !is.na( THERM_M ), CELLSPERML * 10^6 * THERM_M, CELLSPERML * 10^6 * DEPTH ) ) ) %>% # I assume perfect mixture of water column in spring
  select ( YEAR, LAKE, SEASON, STATION, CELLSPERSQM, BIOVOLPERIND_UG )

#### Generate ABUN and WT_UG ####
sum.abun    <- setNames ( aggregate( CELLSPERSQM ~ YEAR + LAKE + SEASON + STATION + BIOVOLPERIND_UG, data=chla.work, sum ),
                          c("YEAR", "LAKE", "SEASON", "STATION", "WT_UG", "ABUN") )
output.phyt <- setNames ( aggregate( ABUN ~ YEAR + LAKE + SEASON + WT_UG, data=sum.abun, mean ),
                          c("YEAR", "LAKE", "SEASON", "WT", "AVGABUN") ) %>%
  select ( "YEAR", "SEASON", "LAKE", "WT", "AVGABUN") %>%
  mutate ( SEASON = tolower(SEASON) )# order it like the rest of the data

#### D100 data ####
#### Parameters for D100 ####
.DRYTOWETCONVERT <- 0.2
.CRITICALDEPTH <- 15 #Depth at which we consider the water column sufficent to account for all biomass, there is a large break between the teens and the twenties, so at 15 this is only applies to western basin Lake Erie

#### Import D100 data ####
zoop.data <- read.table( paste ( sep="", .DATA_INPUT,"Size Spectrum Query TME 2019.txt"), sep=",", header=T )
names(zoop.data) <- toupper ( names(zoop.data) )

#### Refine D100 data ####
zoop.work <- zoop.data %>% 
  mutate ( UNIQUEGROUP = paste ( SAMPLENUM, EPA_COMBONAME ) ) %>%
  mutate ( DRYWTGROUP = ave ( DRYWT, UNIQUEGROUP, FUN = seq_along ) ) %>%
  spread ( DRYWTGROUP, DRYWT ) %>%
  rename ( "DEPTH" = "SITE.DEPTH" )

names ( zoop.work )[15:(15+19)] <- paste ( "DRYWT", names(zoop.work[,15:(15+19)]), sep="" ) #Here I clip off any weights >20, because rarely were these measured and GLNPO only takes the first 20 anyway

zoop.work <- zoop.work %>%   
  filter ( EPA_GROUPNAME %in% c("IMM", "COP", "CLA") ) %>%
  filter ( !grepl ( "FE|FO", STATION ) ) %>%
  filter ( YEAR > 2005 ) %>%
  separate( "SAMPLENUM", c("YEAR2", "ZONE", "LAKE2", "SAMPLEID"), sep=c(2,3,4)) %>%
  select( "YEAR", "LAKE", "SEASON", "EPA_COMBONAME", "EPA_GROUPNAME", "SAMPLEID", "STATION", "TOWDEPTH", "DEPTH", "NUM_M3", 
          starts_with("DRYWT") ) %>%
  filter ( grepl( "I", SAMPLEID) ) %>% #the filter here gets rid of quality control checks
  mutate ( NUM_M2 = ifelse ( DEPTH < .CRITICALDEPTH, (NUM_M3 * DEPTH^2)/ TOWDEPTH, NUM_M3 * DEPTH ) )  #I have simplified the equation here to limit the length of this statement in the first part, the second we assume zoops are zero from 100 down

zoop.work$LAKE <- ifelse ( zoop.work$STATION %in% c("ER 09", "ER 10", "ER 15", "ER 63"), "Erie-East", 
                           ifelse ( zoop.work$STATION %in% c("ER 30", "ER 31", "ER 32", "ER 36", "ER 37", "ER 38", "ER 42", "ER 43", "ER 73", "ER 78"), "Erie-Central",
                                    ifelse ( zoop.work$STATION %in% c("ER 58", "ER 59", "ER 60", "ER 61", "ER 91", "ER 92"), "Erie-Western", 
                                             ifelse( grepl("HU", zoop.work$STATION), "Huron",
                                                     ifelse ( grepl("MI", zoop.work$STATION), "Michigan",
                                                              ifelse ( grepl("ON", zoop.work$STATION), "Ontario", 
                                                                       ifelse ( grepl("SU", zoop.work$STATION), "Superior", "Unknown" ) ) ) ) ) ) )

#### Counts of D100 ####
zoop.work$WT_COUNT <- apply ( zoop.work[,which( colnames(zoop.work)=="DRYWT1" ):which( colnames(zoop.work)=="DRYWT20" )], 
                              MARGIN=1, function (x) length( which(x>0) ) ) #count the number of measured animals in each row, note we are going to have rows with zero
zoop.work$DRYWT1 <- ifelse ( zoop.work$WT_COUNT == 0, zoop.data$BIO_IND, zoop.work$DRYWT1) #for estimated weight simply replace the NA with the estimated value
zoop.work$WT_COUNT <- ifelse ( zoop.work$WT_COUNT == 0, 1, zoop.work$WT_COUNT ) #now change the zero's to ones so we can calculate the percentage contribution by group

#### Calculate abundances D100 ####
#The density here is in m2, which requires us to multiple the density by the depth values
zoop.work <- zoop.work %>% 
  mutate    ( PERCBIOCNTR = 1/WT_COUNT      , ABUN  = NUM_M2 * PERCBIOCNTR )   %>%
  mutate_at ( vars( starts_with( "DRYWT" ) ),  funs( ./.DRYTOWETCONVERT ) )    %>%
  rename_at ( vars( starts_with( "DRYWT" ) ),  funs( sub( "DRY", "WET", .) ) ) %>% 
  gather ( starts_with( "WETWT" ), key = "NAMEWT",   value = "WT" )            %>%
  select ( -one_of("NAMEWT") ) 
#I expand all the dryweights, then I expand all the abundances you can omit the last pipe which drops the column names

#### Analysis of data ####
#Now to analyze the data we will need to expand the series into the tall form because we are going to want to aggregate and plot these values
#To generate the number of sites per year run the following code, we will use this to generate means per size group
samplenum <- setNames ( aggregate( SAMPLEID ~ YEAR + SEASON + LAKE, data=zoop.work, function(x) length(unique(x)) ),
                        c("YEAR", "SEASON", "LAKE", "SAMPLECOUNT") )
sumsamp   <- setNames ( aggregate( ABUN ~ YEAR + SEASON + LAKE + WT, data=zoop.work, sum, na.rm=T ), 
                        c("YEAR", "SEASON", "LAKE", "WT", "SUMABUN") )

# calculate the average densities now
output.d100 <- left_join ( samplenum, sumsamp, by = c("YEAR", "SEASON", "LAKE"), all.x=T ) %>% 
  mutate ( AVGABUN = SUMABUN / SAMPLECOUNT ) %>%
  select ( -one_of ( "SAMPLECOUNT", "SUMABUN" ) ) %>%
  mutate ( SEASON = tolower(SEASON) )


#### Rotifer data ####
#### Parameters####
.DRYTOWETCONVERTMOSTROTIFERS <- 0.1
.DRYTOWETCONVERTSOMEROTIFERS <- 0.039

#### Equations for wet calculation ####
rotieq1 <- function ( L, f, B ) { (L^3 * f + B * L^3 * f) * 10^-6  }
rotieq2 <- function ( W, f ) { W^3 * f * 10^-6 }
rotieq3 <- function ( L, W, f, B ) { (L * W^2 * f + B * L * W^2 * f) * 10^-6 }
velieq  <- function ( L ) { 0.25 * exp (17.5 * L) }
naupeq  <- function ( lna, b, L ) { ( exp ( lna + b*log(L * 0.001) ) ) /.DRYTOWETCONVERT  } #length is converted to mm

#### Bring in rotifer dataset from early years ####
#This is extracted from GLNPO Zooplankton Database v3.02
roti.data <- read.table( paste ( sep="", .DATA_INPUT,"SSA Rotifer Query TME 2019.csv"), header=T, sep="," )
roti.work <- roti.data %>% mutate ( DATE = as.Date(DATE, "%m/%d/%y") ) %>% 
  filter ( DATE > as.Date("2006-01-01") & DATE < as.Date("2011-12-31") ) %>%
  mutate ( STATION = gsub(" ", "", STATION, fixed = TRUE) )

#### Bring in additional datasets ####
#two other datasets will be necessary, they are developed below
depth.data <- read_excel( paste ( sep="", .DATA_INPUT,"WQSstns_Masterlist.xlsx"), sheet = "All") %>% 
  select (STATION2, DEPTH) %>% 
  mutate ( STATION = gsub(" ", "", STATION2, fixed = TRUE) ) %>%
  select (-STATION2)
equa.parm <- read_excel ( paste ( sep="", .DATA_INPUT,"Parameters for Rotifer equations 2019_20_03.xlsx"), sheet = "Sheet1", na="NA" ) %>%
  select (SPECCODE, PAR1, PAR2, EQUATION) %>%
  mutate (SPECCODE = as.factor(SPECCODE))

#### Join datasets ####  
#to bring in the zooplankton data, you merge it with the site data, and then filter out the FE or FO stations
roti.work <- left_join( roti.work, equa.parm, by="SPECCODE" ) %>% mutate (SPECCODE = as.factor(SPECCODE) ) #throws errors you should ignore them, R is nervous not all rows in the first dataset are used.

#### Clean ####
#For some reason a few rotifers do not have any measurement made, these are removed
roti.work <- roti.work %>% mutate ( DROP = ifelse( !is.na(roti.work$L1um | roti.work$W1um), TRUE, FALSE ) ) %>%
  subset( DROP == TRUE ) %>% 
  select (-DROP)

#### Calculate weights ####
#now you can calculate their wet weights
for (i in which( colnames(roti.work)=="L1um"):which( colnames(roti.work)=="L20um") ) {
  roti.work[,ncol(roti.work)+1] <- ifelse ( roti.work$EQUATION == "naupeq", naupeq( lna=roti.work$PAR1, b=roti.work$PAR2, L=roti.work[,i] ),
                                            ifelse ( roti.work$EQUATION == "velieq", velieq( L=roti.work[,i]/1000 ),
                                                     ifelse ( roti.work$EQUATION == "rotieq1", rotieq1( L=roti.work[,i], f=roti.work$PAR1, B=roti.work$PAR2 ),
                                                              ifelse ( roti.work$EQUATION == "rotieq2", rotieq2( W=roti.work[,i+20], f=roti.work$PAR1 ),
                                                                       ifelse ( roti.work$EQUATION == "rotieq3", rotieq3( L=roti.work[,i], W=roti.work[,i+20], f=roti.work$PAR1, B=roti.work$PAR2 ), 
                                                                                NA ) ) ) ) )
  names(roti.work)[length(names(roti.work))] <- paste("WETWT", i-8, sep="") 
}

roti.work <- roti.work[ ,-(which( colnames(roti.work)=="L1um" ):which( colnames(roti.work)=="EQUATION" ) )] #drop the lengths, weights, parameters, and equation references in dataset

#### Add season categories ####
#seperate the date data so we can make some categories
roti.work <- roti.work %>% mutate ( YEAR = format(DATE, "%Y") ) %>%
  mutate ( MONTHDAY = format(DATE, "%m-%d") ) %>%
  mutate ( SEASON = ifelse( MONTHDAY < "06-01", "spring", "summer") )

#### Add depth data ####
#bring the depth data into the dataset
roti.work <- left_join( roti.work, depth.data, by="STATION" ) #NA's may appear but we are ok with this since they are FE and FO stations which get dropped, also benthic stations don't have a depth these will be dropped

#### Refine dataset ####
#refine the dataset now we will work on
roti.work <- roti.work %>% 
  separate( "SAMPLENUM", c("YEAR_SHORT", "ZONE", "LAKE", "SAMPLEID"), sep=c(2,3,4)) %>%           
  filter ( GROUP %in% c("MOL", "NAU", "ROT") ) %>%
  select( -YEAR_SHORT, -ZONE  ) %>%
  mutate ( YEAR = as.numeric(YEAR) )  %>%
  filter ( grepl( "I", SAMPLEID) )    %>%
  filter ( !grepl ( "FE", STATION ) ) %>%
  filter ( !grepl ( "FO", STATION ) ) %>%
  drop_na ( DEPTH ) %>%
  mutate ( LAKE = recode (LAKE, "B" ="Huron", "A"= "Michigan", "E"="Ontario", "S"="Superior") ) %>% 
  mutate ( LAKE = ifelse ( STATION %in% c("ER09", "ER10", "ER15", "ER63"), "Erie-East", 
                           ifelse ( STATION %in% c("ER30", "ER31", "ER32", "ER36", "ER37", "ER38", "ER42", "ER43", "ER73", "ER78"), "Erie-Central",
                                    ifelse ( STATION %in% c("ER58", "ER59", "ER60", "ER61", "ER91", "ER92"), "Erie-Western", 
                                             LAKE ) ) ) )

#### Prep to calculate densities ####
roti.work$WT_COUNT <- apply ( roti.work[,which( colnames(roti.work)=="WETWT1" ):which( colnames(roti.work)=="WETWT20" )], 
                              MARGIN=1, function (x) length( which(x>0) ) ) #count the number of measured animals in each row, note we are going to have rows with zero

#### Calculate areal density ####
roti.work <- roti.work %>%
  mutate ( NUM_M2 = ifelse ( DEPTH < .CRITICALDEPTH, (NUM_M3 * DEPTH^2)/ TOWDEPTH, NUM_M3 * TOWDEPTH ) ) %>%
  mutate ( PERCBIOCNTR = 1/WT_COUNT , ABUN = NUM_M2 * PERCBIOCNTR ) %>%
  select (-PERCBIOCNTR, -NUM_M2, -NUM_M3, -WT_COUNT)


#### Prep data for merging ####
roti.work <- roti.work %>%
  mutate ( STATION = ifelse ( STATION %in% c( "ER15", "ER78", "ER91", "HU15", "HU45", "HU54", "MI18", "MI27", "MI41", "ON33", "ON55", "SU01", "SU08" ), paste (STATION, "M", sep=""),
                              ifelse ( STATION == "ON64", "ON64B", STATION ) ) ) %>%
  mutate ( YEAR = as.numeric(YEAR) ) %>%
  select ("YEAR", "LAKE", "STATION", "SAMPLEID", "TOWDEPTH", "DEPTH", "SEASON", "SPECCODE", "GROUP", "ABUN",
          "WETWT1", "WETWT2", "WETWT3", "WETWT4", "WETWT5", "WETWT6", "WETWT7", "WETWT8", "WETWT9", "WETWT10",
          "WETWT11", "WETWT12", "WETWT13", "WETWT14", "WETWT15", "WETWT16", "WETWT17", "WETWT18", "WETWT19", "WETWT20")

#### Bring in modern datasets ####
#After 2011 changes were made to sampling so these are brought in differently
roti.data.2012    <- merge ( read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_2012_160920.xlsx"), sheet = "zoocnt12" ),
                             read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_2012_160920.xlsx"), sheet = "zoosam12" ) %>% 
                               select ("SAMPLENUM", "STATION", "TOWDEPTH", "DATECOLL"), 
                             by="SAMPLENUM" )
roti.data.2013    <- merge ( read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_2013_161014.xlsx"), sheet = "zoocnt13" ),
                             read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_2013_161014.xlsx"), sheet = "zoosam13" ) %>% 
                               select ("SAMPLENUM", "STATION", "TOWDEPTH", "DATECOLL"), 
                             by="SAMPLENUM" )
roti.data.2014    <- merge ( read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_2014_170508.xlsx"), sheet = "zoocnt14" ),
                             read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_2014_170508.xlsx"), sheet = "zoosam14" ) %>% 
                               select ("SAMPLENUM", "STATION", "TOWDEPTH", "DATECOLL"), 
                             by="SAMPLENUM" )
roti.data.2015_16 <- merge ( read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_201516_D20s_181105.xlsx"), sheet = "zoocnt1516" ),
                             read_excel( paste ( sep="", .DATA_INPUT,"ZOOP_DATA_201516_D20s_181105.xlsx"), sheet = "zoosam1516" ) %>% 
                               select ("SAMPLENUM", "STATION", "TOWDEPTH", "DATECOLL"), 
                             by="SAMPLENUM" )

#### Combine modern D20 data into single dataset ####
roti.data.new <- rbind (roti.data.2012, roti.data.2013, roti.data.2014, roti.data.2015_16) %>% mutate (DATE = DATECOLL)

#### Depth data ####
depth.data <- read_excel( paste ( sep="", .DATA_INPUT,"WQSstns_Masterlist.xlsx"), sheet = "All") %>% 
  select (STATION, DEPTH) %>% 
  mutate ( STATION = gsub(" ", "", STATION, fixed = TRUE) ) # I know have added depth data over and over, but I want to be able to execute in chunks without searching it out, so it I keep putting it back in, please note sometimes I take STATION or STATION2, it depends on how GLNPO stored station data that year in the zoops

#### Combine, refine modern D20 datasets ####
roti.work.new <- roti.data.new %>%
  merge (., depth.data, by = "STATION") %>%
  select ( "SAMPLENUM", "STATION", "SPECCODE", "GROUP", "NUM_M3", "BIO_IND", "TOWDEPTH", "DEPTH", "DATE",
           "DRYWT1", "DRYWT2", "DRYWT3", "DRYWT4", "DRYWT5", 
           "DRYWT6", "DRYWT7", "DRYWT8", "DRYWT9", "DRYWT10", 
           "DRYWT11", "DRYWT12", "DRYWT13", "DRYWT14", "DRYWT15",
           "DRYWT16", "DRYWT17", "DRYWT18", "DRYWT19", "DRYWT20" ) %>%
  filter ( !grepl ( "FE", STATION ) ) %>%
  filter ( !grepl ( "FO", STATION ) ) %>%
  separate( "SAMPLENUM", c("YEAR", "ZONE", "LAKE", "QUALITY", "TYPE"), sep=c(2,3,4,8) ) %>%
  mutate ( SAMPLEID = paste (QUALITY, TYPE, sep="") ) %>%
  filter ( TYPE == 4 )               %>%
  filter ( grepl( "I", QUALITY) )    %>%
  select ( -ZONE, -QUALITY, -TYPE )  %>%
  mutate ( YEAR = as.numeric(YEAR) + 2000 ) %>% 
  mutate ( LAKE = ifelse (YEAR == 2012, recode (LAKE, "B" ="Huron", "A"= "Michigan", "E"="Ontario", "S"="Superior"),
                          recode (LAKE, "H" ="Huron", "M"= "Michigan", "O"="Ontario", "S"="Superior", "E"="Erie") ) ) %>%
  mutate ( LAKE = ifelse ( STATION %in% c("ER09", "ER10", "ER15M", "ER63"), "Erie-East", 
                           ifelse ( STATION %in% c("ER30", "ER31", "ER32", "ER36", "ER37", "ER38", "ER42", "ER43", "ER73", "ER78M"), "Erie-Central",
                                    ifelse ( STATION %in% c("ER58", "ER59", "ER60", "ER61", "ER91M", "ER92"), "Erie-Western", 
                                             LAKE ) ) ) )

#### Add season categories ####
#seperate the date data so we can make some categories
roti.work.new <- roti.work.new %>%
  mutate ( MONTHDAY = format(DATE, "%m-%d") ) %>%
  mutate ( SEASON = ifelse( MONTHDAY < "06-01", "spring", "summer") )
#Data should now be ready to begin abundance/density calculation

#### Prep to calculate densities ####
roti.work.new$WT_COUNT <- apply ( roti.work.new[,which( colnames(roti.work.new)=="DRYWT1" ):which( colnames(roti.work.new)=="DRYWT20" )], 
                                  MARGIN=1, function (x) length( which(x>0) ) ) #count the number of measured animals in each row, note we are going to have rows with zero

roti.work.new$DRYWT1   <- ifelse ( roti.work.new$WT_COUNT == 0, roti.data.new$BIO_IND, roti.work.new$DRYWT1) #for estimated weight simply replace the NA with the estimated value
roti.work.new$WT_COUNT <- ifelse ( roti.work.new$WT_COUNT == 0, 1, roti.work.new$WT_COUNT ) #now change the zero's to ones so we can calculate the percentage contribution by group

#### Calculate areal density ####
roti.work.new <- roti.work.new %>%
  mutate ( NUM_M2 = ifelse ( DEPTH < .CRITICALDEPTH, (NUM_M3 * DEPTH^2)/ TOWDEPTH, NUM_M3 * TOWDEPTH ) ) %>%
  mutate ( PERCBIOCNTR = 1/WT_COUNT , ABUN = NUM_M2 * PERCBIOCNTR ) %>%
  mutate ( TEMP = ifelse ( SPECCODE %in% c( "ASPBRIG", "ASPHERR", "ASPPRIO" ), 
                           .DRYTOWETCONVERTSOMEROTIFERS, ifelse ( GROUP %in% c("MOL", "NAU"), 
                                                                  .DRYTOWETCONVERT, 
                                                                  .DRYTOWETCONVERTMOSTROTIFERS ) ) ) %>%
  mutate_at ( vars( starts_with( "DRYWT" ) ),  funs( ./TEMP ) )                %>%
  rename_at ( vars( starts_with( "DRYWT" ) ),  funs( sub( "DRY", "WET", .) ) ) %>%
  select (-TEMP, -PERCBIOCNTR, -NUM_M2, -NUM_M3, -WT_COUNT, -BIO_IND)

#### Prep modern D20 for merge ####
roti.work.new <- roti.work.new %>%
  select ("YEAR", "LAKE", "STATION", "SAMPLEID", "TOWDEPTH", "DEPTH", "SEASON", "SPECCODE", "GROUP", "ABUN",
          "WETWT1", "WETWT2", "WETWT3", "WETWT4", "WETWT5", "WETWT6", "WETWT7", "WETWT8", "WETWT9", "WETWT10",
          "WETWT11", "WETWT12", "WETWT13", "WETWT14", "WETWT15", "WETWT16", "WETWT17", "WETWT18", "WETWT19", "WETWT20")

#### Combine D20 datasets and convert to long ####
roti.work.full <- rbind (roti.work, roti.work.new) %>% 
  gather ( starts_with( "WETWT" ), key = "NAMEWT",   value = "WT" ) %>%
  select ( -one_of("NAMEWT") ) 

#### Analysis of data ####
#To generate the number of sites per year run the following code, we will use this to generate means per size group
samplenum <- setNames ( aggregate( SAMPLEID ~ YEAR + SEASON + LAKE,  data=roti.work.full, function(x) length(unique(x)) ),
                        c("YEAR", "SEASON", "LAKE", "SAMPLECOUNT") )
sumsamp   <- setNames ( aggregate( ABUN ~ YEAR + SEASON + LAKE + WT, data=roti.work.full, sum, na.rm=T ), 
                        c("YEAR", "SEASON", "LAKE", "WT", "SUMABUN") )

# calculate the average abundances now
output.rot <- left_join ( samplenum, sumsamp, by = c("YEAR", "SEASON", "LAKE"), all.x=T ) %>% 
  mutate ( AVGABUN = SUMABUN / SAMPLECOUNT ) %>%
  select ( -one_of ( "SAMPLECOUNT", "SUMABUN" ) )

#### Add Mysis Data ####
mysis.data <- read_excel( paste ( sep="", .DATA_INPUT,"Mysis_QSizeSpectra_1v3.xlsx"), sheet = "QSizeSpectra_1" )

#### Mysis Parameters ####
.NETDIAM = 1.0 # net diameter

#### Dataset manipulation ####
names(mysis.data) <- toupper ( names(mysis.data) )
mysis.work <- mysis.data %>% 
  filter ( !grepl ( "FE|FO", STATIONID ) ) %>%
  dplyr::rename ( WT_UG = WEIGHT ) %>% filter ( !is.na(WT_UG) ) %>%
  mutate ( WT_UG = WT_UG/.DRYTOWETCONVERT ) %>%
  mutate ( SEASON = tolower(SEASON) ) %>%
  mutate ( ABUN = ifelse ( YEAR > 2011, CNTFACTOR / ( pi * (.NETDIAM/2)^2 ), CNTFACTOR ) ) %>% #Account for circular net in 2012 onwards, initially it was 1m^2
  mutate ( LAKE = ifelse ( STATIONID %in% c("ER09", "ER10", "ER15M", "ER63", "ER93b"), "Erie-East", 
                           ifelse (STATIONID %in% c("ER30", "ER31", "ER32", "ER36", "ER37", "ER38", "ER42", "ER43", "ER73", "ER78M"), "Erie-Central",
                                   ifelse ( STATIONID %in% c("ER58", "ER59", "ER60", "ER61", "ER91M", "ER92"), "Erie-Western", 
                                            LAKE ) ) ) ) #ER93b is a benthic sample, and we may need to discard it, there should be no central or western from summer samples

#### Analysis of Mysis data ####
samplenum <- setNames ( aggregate( REPLICATE ~ YEAR + SEASON + LAKE + STATIONID, data=mysis.work,max),
                        c("YEAR", "SEASON", "LAKE", "STATIONID", "SITECOUNT") )
sumsamp   <- setNames ( aggregate( ABUN ~ YEAR + SEASON + LAKE + STATIONID + WT_UG, data=mysis.work, sum, na.rm=T ), 
                        c("YEAR", "SEASON", "LAKE", "STATIONID", "WT", "RAWABUN") )

#### Calculate the average abundances of mysis ####
temp.mys <- left_join ( samplenum, sumsamp, by = c("YEAR", "SEASON", "LAKE", "STATIONID"), all.x=T ) %>% 
  mutate ( AVGSITEABUN = RAWABUN / SITECOUNT )

#### Convert to standard output format ####
samplenum <- setNames ( aggregate( STATIONID ~ YEAR + SEASON + LAKE, data=temp.mys, function(x) length(unique(x)) ),
                        c("YEAR", "SEASON", "LAKE", "SAMPLECOUNT") )
sumsamp   <- setNames ( aggregate( AVGSITEABUN ~ YEAR + SEASON + LAKE + WT, data=temp.mys, sum, na.rm=T ), 
                        c("YEAR", "SEASON", "LAKE", "WT", "SUMABUN") )

output.mys <- left_join ( samplenum, sumsamp, by = c("YEAR", "SEASON", "LAKE"), all.x=T ) %>% 
  mutate ( AVGABUN = SUMABUN / SAMPLECOUNT ) %>%
  select ( -one_of ( "SAMPLECOUNT", "SUMABUN" ) ) %>%
  mutate ( SEASON = tolower(SEASON) )

#### Bring data together ####
output <- rbind (output.mys, output.rot, output.d100, output.phyt) #this is the code if you don't want to include benthic

#### Convert data to grams ####
output$WT <- output$WT*10^-6

output.mys$WT <- output.mys$WT*10^-6
output.rot$WT <- output.rot$WT*10^-6
output.d100$WT <- output.d100$WT*10^-6
output.phyt$WT <- output.phyt$WT*10^-6

#### Drop NA's #####
output <- output %>% drop_na ( )

#calculate the number of log2 bins, taken from Edwards et al. 2017 and assign the bins
log2bins.list <- log2bins(output$WT) #ignore the warning, we don't have counts we have abundance
output$BINMID <- log2bins.list$indiv$binMid
output$BINMIN <- log2bins.list$indiv$binMin
output$BINMAX <- log2bins.list$indiv$binMax

output <- setNames ( aggregate ( AVGABUN ~ YEAR + LAKE + SEASON + as.factor(output$BINMID) + BINMIN + BINMAX, sum, data=output ),
                     c("YEAR", "LAKE", "SEASON", "BINMID", "BINMIN", "BINMAX", "SUMAVGABUN") )
#BINMIN and BINMAX are included here but will not do anything, but by including they will be carried through to the summary table and will be usuable for relative density
output$BINMID <- as.numeric ( as.character (output$BINMID) )
output <- as_tibble (output) #save this in tiddyverse form

output <- output %>% 
  group_by ( YEAR, LAKE, SEASON, BINMID, BINMIN, BINMAX ) %>%
  mutate ( BINMID_LOG = log2(BINMID), BINMIN_LOG = log2(BINMIN), BINMAX_LOG = log2(BINMAX), ABUN_LOG = log2(SUMAVGABUN) )

output$LAKE <- factor(output$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario" ))

#### Output the SSA data ####
write.table(output, paste ( sep="", .DATA_OUTPUT,"output_phyto to mysis.csv"), sep=",", row.names=FALSE )

#### Benthic data ####
bent.data <- read_excel( paste ( sep="", .DATA_INPUT, "GLNPO Benthos density biomass 2012 2017.xlsx" ), 
                         sheet = "Sheet1" )

#### Rename and clean benthic data ####
names(bent.data) <- toupper ( names(bent.data) )
bent.work <- bent.data %>%
  select( "YEAR", "LAKE", "STATION", "SAMPLEID", "NUM_SAM", "DENSITY", "BIO_SAM", "DENSITY GROUP" ) %>%
  rename ( "DENSITY_GROUP" = "DENSITY GROUP" ) %>% 
  filter ( !grepl ( "FE|FO", STATION ) ) %>%
  mutate ( STATION = toupper(STATION) )  %>%
  mutate ( AVGSIZE = BIO_SAM / NUM_SAM ) %>%
  filter ( AVGSIZE != 0 )  %>%
  separate( "SAMPLEID", c("FRONT", "TYPE", "TAIL"), sep=c(6,7) , remove=FALSE ) %>%
  select ( -FRONT, -TAIL ) %>% 
  filter ( DENSITY_GROUP != "Dreissenidae" ) %>% #drop dresen data we are going to add it back in below with more size resolution
  filter ( DENSITY_GROUP != "Mysis" ) #Remove mysis as they are not effectively sampled by ponar

bent.work$LAKE <- ifelse ( bent.work$STATION %in% c("ER09", "ER10", "ER15M", "ER63", "ER93B"), "Erie-East", 
                           ifelse ( bent.work$STATION %in% c("ER30", "ER31", "ER32", "ER36", "ER37", "ER38", "ER42", "ER43", "ER73", "ER78M", "ER95B"), "Erie-Central",
                                    ifelse ( bent.work$STATION %in% c("ER58", "ER59", "ER60", "ER61", "ER91M", "ER92"), "Erie-Western", 
                                             bent.work$LAKE ) ) )
#### Dresen data ####
dress.data <- read_excel( paste ( sep="", .DATA_INPUT, "Dreissena GLNPO size classes 2008-2018.xls"), 
                          sheet = "2012-2018" )
#### Dresen parameters ####
.PONARAREA <- 0.0523 #ponar grab per m^2

#### Rename and clean Dresen data ####
names(dress.data) <- toupper ( names(dress.data) )
dress.work <- dress.data %>%
  select( "YEAR", "LAKE", "STATION", "SAMPLENUM", "SIZE_GROUP", "NUM_SIZE", "BIO_SIZE" ) %>%
  rename ( SAMPLEID = SAMPLENUM, NUM_SAM = NUM_SIZE, BIO_SAM = BIO_SIZE ) %>%
  filter ( !grepl ( "FE|FO", STATION ) ) %>%
  mutate ( STATION = toupper(STATION) )  %>%
  separate( "SAMPLEID", c("FRONT", "TYPE", "TAIL"), sep=c(6,7), remove=FALSE ) %>%
  select ( -FRONT, -TAIL ) %>%
  drop_na ( NUM_SAM )      %>%
  filter ( NUM_SAM != 0 )  %>% #for book keeping they report zeros for size classes, but we don't need to keep track of those
  mutate ( DENSITY = NUM_SAM / .PONARAREA ) %>% #calculate animals per m^2 (also I know I can change in mutate but it makes for easier reading if I break it up)
  mutate ( DENSITY_GROUP = "Dreissenidae" ) %>% #all of these are bivalves
  mutate ( AVGSIZE_WITHSHELL = BIO_SAM / NUM_SAM ) %>%
  filter ( AVGSIZE_WITHSHELL != 0 ) %>%
  select ( -SIZE_GROUP ) #Get rid of size group so we can combine

dress.work$LAKE <- ifelse ( dress.work$STATION %in% c("ER09", "ER10", "ER15M", "ER63", "ER93B"), "Erie-East", 
                            ifelse ( dress.work$STATION %in% c("ER30", "ER31", "ER32", "ER36", "ER37", "ER38", "ER42", "ER43", "ER73", "ER78M", "ER95B"), "Erie-Central",
                                     ifelse ( dress.work$STATION %in% c("ER58", "ER59", "ER60", "ER61", "ER91M", "ER92"), "Erie-Western", 
                                              dress.work$LAKE ) ) )

#### Dres shell weight dataset ####
shell.data <- read.table ( paste ( sep="", .DATA_INPUT, "LkHuron 2017-18 GLERL Regressions.csv"), sep=",", header=TRUE )

#### Rename and clean shell data ####
names ( shell.data ) <- toupper ( names (shell.data) )
shell.work <- shell.data %>%
  mutate ( TISSUE.WET.WT = WHOLE.WET.WT.MG - SHELL.WT.MG)

#### Create linear model ####               
shell.lm <- lm ( TISSUE.WET.WT ~ WHOLE.WET.WT.MG, data=shell.work ) #this linear model suggest intercept is zero, so we only need to extract the slope coefficient

#### Calculate the tissue weight in Dress data #####
dress.work <- mutate ( dress.work, AVGSIZE = shell.lm$coefficients[[2]] * AVGSIZE_WITHSHELL  )

#### Prepare to combine datasets ####
dress.work <- dress.work[,c( "YEAR", "LAKE", "STATION", "SAMPLEID", "TYPE", "NUM_SAM", "DENSITY", "BIO_SAM", "DENSITY_GROUP", "AVGSIZE")]

#### Combine the Benthic and Dresen data #### 
bent.work <- rbind ( bent.work, dress.work )

#### Control for replicates ####
bent.rep <- setNames ( aggregate ( TYPE ~ YEAR + STATION , data=bent.work, function(x) length(unique(x)) ),
                       c("YEAR", "STATION", "REPLICATES") ) #report number of replicates per station in each year, lake info is included in station
bent.work <- left_join ( bent.work, bent.rep, by=c("YEAR", "STATION") ) # bind the number of replicates to the dataframe

#### Analysis of benthic data ####
samplenum <- setNames ( aggregate( SAMPLEID ~ YEAR + LAKE , data=bent.work, function(x) length(unique(x)) ),
                        c("YEAR", "LAKE", "SAMPLECOUNT") )
sumsamp   <- setNames ( aggregate( DENSITY ~ YEAR + LAKE + AVGSIZE + REPLICATES, data=bent.work, sum, na.rm=T ), 
                        c("YEAR", "LAKE", "WT", "REPLICATES", "SUMABUN") )
#sumsamp$WT <- ( ifelse ( sumsamp$WT > 0, sumsamp$WT * 10^6, 0.1 ) ) #convert from g to ug and turn zero's into a real value

#### Calculate the average benthic densities ####
output.bent <- left_join ( samplenum, sumsamp, by = c("YEAR", "LAKE"), all.x=T ) %>%
  mutate ( AVGABUN = SUMABUN / (SAMPLECOUNT*REPLICATES) ) %>%
  select ( -one_of ( "SAMPLECOUNT", "SUMABUN", "REPLICATES" ) ) %>%
  mutate ( SEASON = "summer")

#### Reorder columns to follow convention ####
output.bent <- output.bent[c("YEAR", "SEASON", "LAKE", "WT", "AVGABUN")]

#### Store output.bent for latter use ####
output.bent.store <- output.bent

#### Drop NA's #####
#These are introduced in benthic data and can be removed here
output.bent <- output.bent %>% drop_na ( )

#calculate the number of log2 bins, taken from Edwards et al. 2017 and assign the bins
log2bins.list <- log2bins(output.bent$WT) #ignore the warning, we don't have counts we have abundance
output.bent$BINMID <- log2bins.list$indiv$binMid
output.bent$BINMIN <- log2bins.list$indiv$binMin
output.bent$BINMAX <- log2bins.list$indiv$binMax

output.bent <- setNames ( aggregate ( AVGABUN ~ YEAR + LAKE + SEASON + as.factor(output.bent$BINMID) + BINMIN + BINMAX, sum, data=output.bent ),
                          c("YEAR", "LAKE", "SEASON", "BINMID", "BINMIN", "BINMAX", "SUMAVGABUN") )
#BINMIN and BINMAX are included here but will not do anything, but by including they will be carried through to the summary table and will be usuable for relative density
output.bent$BINMID <- as.numeric ( as.character (output.bent$BINMID) )

output.bent <- output.bent %>% 
  group_by ( YEAR, LAKE, SEASON, BINMID, BINMIN, BINMAX ) %>%
  mutate ( BINMID_LOG = log2(BINMID), BINMIN_LOG = log2(BINMIN), BINMAX_LOG = log2(BINMAX), ABUN_LOG = log2(SUMAVGABUN) ) %>% ungroup()

output.bent$LAKE <- factor(output.bent$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario"))
output.bent$YEAR <- as.numeric (output.bent$YEAR)

#### Output the SSA data ####
write.table(output.bent, paste ( sep="", .DATA_OUTPUT, "output_benthos womysis.csv"), sep=",", row.names=FALSE)

#### Linear fits of pelagic size spectrum ####
#### Set up fits dataframe ####
fits.yr.lk = data.frame ( matrix ( NA, ncol = 16, nrow = length( unique (output$LAKE) ) * (length( unique (output$YEAR) )-1) * (length( unique (output$SEASON) ) + 1) ) )
colnames ( fits.yr.lk ) = c( "YEAR", "LAKE", "SEASON", "OUTLIERPRES" ,"N", "FITMIN", "FITMAX", "SLOPE", "SLP_SE", "SLP_L95CI", "SLP_U95CI", "INTCPT", "INT_SE", "INT_L95CI", "INT_U95CI", "HEIGHT")

fits.yr.lk$YEAR = rep( sort( unique (output$YEAR) )[-12], each = length(unique(output$LAKE) ) * ( length(unique(output$SEASON) ) + 1 ) )
fits.yr.lk$LAKE = rep( as.factor ( unique (output$LAKE) ), each = 3 )
fits.yr.lk$SEASON = factor ( rep( c(as.character ( unique (output$SEASON) ), "both"), 1 ), levels=c("spring", "summer", "both") )
fits.yr.lk$OUTLIERPRES = factor ( fits.yr.lk$OUTLIERPRES, levels = c( "Y","N" ) )

#### Set up short dataframe ####
short.output = setNames ( data.frame ( matrix ( NA, ncol=6, nrow=0 ) ),  c( "YEAR", "LAKE", "SEASON", "ABUN_LOG", "BINMID_LOG", "DATAUSEDINFIT" ) ) #These data will be easier to export as well since it only has 6 columns

short.output$LAKE = factor ( short.output$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )
short.output$SEASON = factor ( short.output$SEASON, levels=c("spring", "summer", "both") )

#### Loop for filling with breaks ####
for ( i in 1:nrow(fits.yr.lk) ) {
  
  
  if (fits.yr.lk$SEASON[i] == "spring" || fits.yr.lk$SEASON[i] == "summer") {
    tempdata = output  %>% 
      filter ( YEAR == fits.yr.lk$YEAR[i] )%>%
      filter ( LAKE == fits.yr.lk$LAKE[i] )%>%
      filter ( SEASON == fits.yr.lk$SEASON[i] )
    
    templin = lm ( ABUN_LOG ~ BINMID_LOG , data = tempdata )
    
    cookcutoff = 10/length(templin$residuals) #set your Cook's value, the general rule is 4/n for detection of influence, 1 is a rough cutoff for points with too much leverage
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, "dropped", "used" )
    tempjoin  = data.frame ( "YEAR" = tempdata$YEAR, "LAKE" = tempdata$LAKE, "SEASON" = tempdata$SEASON, "ABUN_LOG" = tempdata$ABUN_LOG, "BINMID_LOG" = tempdata$BINMID_LOG, "DATAUSEDINFIT"=tempcooks ) 
    short.output = rbind ( short.output, tempjoin )
    
    if ( any ( cooks.distance(templin) > cookcutoff ) ) { #remove points with high leverage or just save the lm as it is
      fits.yr.lk$OUTLIERPRES[i] = "Y"
      templin = lm ( ABUN_LOG [-which (cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data=tempdata ) 
      } else { fits.yr.lk$OUTLIERPRES[i] = "N" }  #report that no high leverage points were found
    
    
  } else { #this will be for the "both" in season, but if additional seasons are added we need to change this line to be more specific
    tempdata = output  %>% 
      filter ( YEAR == fits.yr.lk$YEAR[i] ) %>%
      filter ( LAKE == fits.yr.lk$LAKE[i] )
    
    templin = lm ( ABUN_LOG ~ BINMID_LOG , data = tempdata )
    
    cookcutoff = 10/length(templin$residuals)
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, "dropped", "used" )
    tempjoin  = data.frame ( "YEAR" = tempdata$YEAR, "LAKE" = tempdata$LAKE, "SEASON" = "both", "ABUN_LOG" = tempdata$ABUN_LOG, "BINMID_LOG" = tempdata$BINMID_LOG, "DATAUSEDINFIT"=tempcooks ) 
    short.output = rbind ( short.output, tempjoin )
    
    if ( any ( cooks.distance(templin) > cookcutoff ) ) { #remove points with high leverage or just save the lm as it is
      fits.yr.lk$OUTLIERPRES[i] = "Y"
      templin = lm ( ABUN_LOG [-which (cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data=tempdata ) 
      } else { fits.yr.lk$OUTLIERPRES[i] = "N" }  #report that no high leverage points were found
    
  }
  
  fits.yr.lk$N[i]         = length(templin$residuals)
  fits.yr.lk$FITMIN[i]    = min(templin$model$BINMID_LOG, na.rm=T)
  fits.yr.lk$FITMAX[i]    = max(templin$model$BINMID_LOG, na.rm=T)
  fits.yr.lk$SLOPE[i]     = coefficients(templin)[[2]]
  fits.yr.lk$SLP_SE[i]    = coef(summary(templin))[2,2]
  fits.yr.lk$SLP_L95CI[i] = confint(templin,level=0.95)[2,1] 
  fits.yr.lk$SLP_U95CI[i] = confint(templin,level=0.95)[2,2]
  fits.yr.lk$INTCPT[i]    = coefficients(templin)[[1]]
  fits.yr.lk$INT_SE[i]    = coef(summary(templin))[1,2]
  fits.yr.lk$INT_L95CI[i] = confint(templin,level=0.95)[1,1]
  fits.yr.lk$INT_U95CI[i] = confint(templin,level=0.95)[1,2]
  
  temp.short <- tempjoin %>%
                filter ( DATAUSEDINFIT == "used" )
  
  fits.yr.lk$HEIGHT[i]    = fits.yr.lk$SLOPE[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lk$INTCPT[i]
  
}

#### fits of height from overall ####
fits.yr.lk$HEIGHT_OVERALL = fits.yr.lk$SLOPE * ( floor(min(fits.yr.lk$FITMIN)) + ceiling(max(fits.yr.lk$FITMAX)) ) / 2 + fits.yr.lk$INTCPT

#### Deal with missing phyto data ####
miss.data <- setNames ( adply ( table (output.phyt$YEAR, output.phyt$LAKE, output.phyt$SEASON ), c(1,2,3) ), c( "YEAR", "LAKE", "SEASON", "OBSERVATIONS" ) ) #report from phyto when data is missing
miss.data$YEAR = as.numeric ( as.character (miss.data$YEAR) ) #quick conversion issue removal, otherwise year is a factor, direct conversion will tell R to assume it starts at 1 and count up for each level
miss.data$MISSINGPHYTO <- factor ( ifelse ( miss.data$OBSERVATIONS == 0, "Y", "N") ) #characterize missing data

#now we need to control for the "both" season
both.data <- miss.data[miss.data$SEASON == "spring",]
both.data$SEASON <- "both"
temp.vec.spr <- miss.data$MISSINGPHYTO[ miss.data$SEASON=="spring" ] #extract spring vector for missing
temp.vec.sum <- miss.data$MISSINGPHYTO[ miss.data$SEASON=="summer" ] #extract summer vector for missing
both.data$MISSINGPHYTO <- factor ( ifelse ( temp.vec.spr == "N" & temp.vec.sum == "N", "N", "Y" ) ) #logical test for both, fails if either spring or summer is missing
miss.data <- rbind (miss.data, both.data) #finally done here, but now we can move on to the
miss.data$LAKE = factor ( miss.data$LAKE , levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

# combine it with output data
short.output <- miss.data %>%
  select ( YEAR, LAKE, SEASON, MISSINGPHYTO ) %>%
  left_join( short.output , ., by = c("YEAR", "LAKE", "SEASON"), all.x=T )

#and with the fits database
fits.yr.lk <- miss.data %>%
  select ( YEAR, LAKE, SEASON, MISSINGPHYTO ) %>%
  left_join( fits.yr.lk , ., by = c("YEAR", "LAKE", "SEASON"), all.x=T )

#### Output the table with fits ####
write.table ( fits.yr.lk, paste (sep="", .DATA_OUTPUT,"fitsdata_phtyo to mysis.csv"), sep=",", row.names=FALSE )

write.table ( short.output, paste (sep="", .DATA_OUTPUT,"shortoutput_phtyo to mysis.csv"), sep=",", row.names=FALSE )

#### Size spectra and benthos spectra ####
output_for_plot <- short.output %>%
  filter ( YEAR >2011 & YEAR < 2016 ) %>% 
  filter ( SEASON == "summer" ) %>%
  mutate ( SIZESPECTYPE = "pelagic" )
output_for_plot$LAKE <- factor( output_for_plot$LAKE , levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

output_benthic_for_plot <- output.bent %>%
  filter ( YEAR < 2016 ) %>%
  select ( YEAR, LAKE, SEASON, ABUN_LOG, BINMID_LOG ) %>%
  mutate ( DATAUSEDINFIT = "used", MISSINGPHYTO = "N", SIZESPECTYPE = "benthic" )

#combine for plots
output_for_plot <- rbind ( output_for_plot, output_benthic_for_plot )

#### Color palette ####
color_custom <- c("limegreen", "royalblue2", "gray50")
color_custom2 <- c( "purple", "royalblue2")

#### Size spectrum analysis with different spectra plotted ####
plot_ssa <-  
  ggplot(output_for_plot,
         aes(BINMID_LOG, ABUN_LOG, color=SIZESPECTYPE) ) +
  geom_point ( alpha = 1/2, size=3 ) +
  ylim (-5 , 45) +
  xlim (-42, 5) +
  facet_grid ( YEAR ~ LAKE ) +
  
  scale_color_manual(values= color_custom2, name="Size Spectra", labels=c("Benthic", "Pelagic") ) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          strip.text.x = element_text(size = 12 ),
          strip.text.y = element_text(size = 12 ) )

ggsave( "ssa_pelagic and benthic.pdf", path=.FIGURE_OUTPUT_LOC, 
        plot_ssa, width = 9, height = 5, units="cm", scale=3.5, dpi=400 )

#### Size spectrum analysis with benthic and pelagic combined ####
temp_output <- setNames ( aggregate ( ABUN_LOG ~ YEAR + LAKE + SEASON + as.factor(output_for_plot$BINMID_LOG) + 
                                        DATAUSEDINFIT + MISSINGPHYTO, sum, data=output_for_plot ),
                          c("YEAR", "LAKE", "SEASON", "BINMID_LOG", "DATAUSEDINFIT", "MISSINGPHYTO", "SUMABUN_LOG") )
temp_output$BINMID_LOG <- as.numeric(as.character(temp_output$BINMID_LOG))

####create small fits dataframe
comb.fits = data.frame ( matrix ( NA, ncol = 4, nrow = length( unique (output$LAKE) ) * (length( 2012:2015 ) ) ) )
colnames ( comb.fits ) = c( "YEAR", "LAKE", "N", "SLOPE")

comb.fits$YEAR = rep( 2012:2015,7 )
comb.fits$LAKE = rep( as.factor ( unique (output$LAKE) ), each = 4 )

for ( i in 1:nrow(comb.fits) ) {
  
  
  tempdata = temp_output  %>% 
    filter ( YEAR == comb.fits$YEAR[i] )%>%
    filter ( LAKE == comb.fits$LAKE[i] )%>%
    filter ( DATAUSEDINFIT == "used" )
  
  templin = lm ( SUMABUN_LOG ~ BINMID_LOG, data=tempdata ) 
  
  comb.fits$N[i]         = length(templin$residuals)
  comb.fits$SLOPE[i]     = coefficients(templin)[[2]]
}

#### Plot combined data spectrum  
plot_ssa <-  
  ggplot(temp_output,
         aes(BINMID_LOG, SUMABUN_LOG, shape=DATAUSEDINFIT) ) +
  geom_point ( alpha = 1, size=1.5 ) + #was alpha = 1/2, size=3
  ylim (-5 , 45) +
  xlim (-42, 5) +
  facet_grid ( YEAR ~ LAKE ) +
  
  scale_shape_manual( name="Data used in fit", labels=c("Used", "Dropped"), values=c(19,21) ) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  geom_smooth( data = temp_output %>% subset ( DATAUSEDINFIT == "used" ), 
               aes (BINMID_LOG, SUMABUN_LOG ),
               method = lm, se=FALSE, inherit.aes = FALSE ) +
  geom_label( data = comb.fits, 
              aes( x=-5, y=40, label = format ( round( SLOPE, 2 ), 2 ) ) , 
              show.legend = FALSE, inherit.aes = FALSE ) +
  
  theme_bw() + 
  theme ( text = element_text( color="black" ),
          panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          strip.text.x = element_text(size = 12, color="black" ),
          strip.text.y = element_text(size = 12, color="black" ),
          axis.text = element_text( color="black" ) )

ggsave( "ssa_combined.pdf", path=.FIGURE_OUTPUT_LOC, 
        plot_ssa, width = 9, height = 5, units="cm", scale=3.5, dpi=400 )

#### Output graphs with linear fits ####
output_for_plot <- short.output %>%
  filter ( YEAR < 2016 ) %>% 
  filter (SEASON != "both") %>%
  select ( -MISSINGPHYTO ) %>%
  mutate ( PLACEMENT = ifelse ( SEASON == "spring", 35, ifelse ( SEASON == "summer", 24, 13 ) ) ) %>%  #this will allow fits to appear
  left_join( . , fits.yr.lk, by = c("YEAR", "SEASON", "LAKE"), all.x=T ) #this join makes it easier for ggplot to take the data, of course it creates many rep values
output_for_plot$LAKE <- factor( output_for_plot$LAKE , levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

#### Size spectrum analysis of all lakes with slopes ####
plot_ssa <-  
  ggplot(output_for_plot,
         aes(BINMID_LOG, ABUN_LOG, color=SEASON, shape=DATAUSEDINFIT) ) +
  geom_point ( alpha = 1/2, size=3 ) +
  ylim (-5 , 45) +
  xlim (-40, 0) +
  facet_grid ( YEAR ~ LAKE ) +
  
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer") ) +
  scale_shape_manual( name="Data used in fit", labels=c("Used", "Dropped"), values=c(19,21) ) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          strip.text.x = element_text(size = 12 ),
          strip.text.y = element_text(size = 12 ) ) +
  
  geom_smooth( data = output_for_plot %>% subset ( FITMAX & FITMIN ) %>% subset ( DATAUSEDINFIT == "used" & MISSINGPHYTO=="N" ), 
               method = lm, se=FALSE ) +
  geom_label( data=output_for_plot%>%filter(MISSINGPHYTO=="N"), aes( x=-7, y=PLACEMENT, label = format ( round( SLOPE, 2 ), 2 ) ) , show.legend = FALSE )

ggsave( "ssa_phyto to mysis.pdf", path=.FIGURE_OUTPUT_LOC, 
        plot_ssa, width = 9, height = 11, units="cm", scale=3.5, dpi=400 )

#### Size spectra example data ####
output_for_plot <- short.output %>%
  select ( -MISSINGPHYTO ) %>%
  filter ( LAKE == "Superior" ) %>%
  filter ( YEAR == 2007 ) %>% 
  filter ( SEASON != "both" ) %>%
  mutate ( PLACEMENT = ifelse ( SEASON == "spring", 35, ifelse ( SEASON == "summer", 24, 13 ) ) ) %>%
  left_join( . , fits.yr.lk, by = c("YEAR", "SEASON", "LAKE"), all.x=T ) #this join makes it easier for ggplot to take the data, of couse it creates many rep values

phyt_for_plot <- output.phyt %>%
  filter ( LAKE == "Superior" ) %>%
  filter ( YEAR == 2007 )

roti_for_plot <- output.rot %>%
  filter ( LAKE == "Superior" ) %>%
  filter ( YEAR == 2007 )

zoop_for_plot <- output.d100 %>%
  filter ( LAKE == "Superior" ) %>%
  filter ( YEAR == 2007 )

mysis_for_plot <- output.mys %>%
  filter ( LAKE == "Superior" ) %>%
  filter ( YEAR == 2007 )

#### Pelagic size spectra plot example ####
plot_ssa <-  
  ggplot(output_for_plot,
         aes(BINMID_LOG, ABUN_LOG, color=SEASON, shape=DATAUSEDINFIT) ) +
  geom_point ( alpha = 1/2, size=5, stroke = 2 ) +
  geom_point ( x=-23, y=22.5, color="black", fill="transparent", shape=22, size=8, stroke=2 ) +
  ylim (-5 , 45) +
  xlim (-40, 0) +
  facet_grid ( YEAR ~ LAKE ) +
  
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer"), na.value="grey30" ) +
  scale_shape_manual( name="Data used in fit", labels=c("Used", "Dropped"), values=c(19,21) ) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key=element_blank(),
          strip.text.x = element_text(size = 18 ),
          strip.text.y = element_text(size = 18 ),
          axis.text = element_text(size = 18 ),
          text = element_text ( size =18 ) ) +
  
  geom_smooth( data = output_for_plot %>% subset ( FITMAX & FITMIN ) %>% subset ( DATAUSEDINFIT == "used" & MISSINGPHYTO=="N" ), 
               method = lm, show.legend=FALSE, se=FALSE ) +
  geom_label( data=output_for_plot%>%filter(MISSINGPHYTO=="N"), aes( x=-3, y=PLACEMENT+2, label = format ( round( SLOPE, 2 ), 2 ) ) , show.legend = FALSE, size=7 ) +
  
  #Phytoplankton line
  geom_segment( data=phyt_for_plot, aes ( x=log2( min ( WT, na.rm=T ) ) , y=43, 
                                      xend=log2( max ( WT, na.rm=T ) ) , yend=43 ), inherit.aes = FALSE, 
                size = 1, color="Grey50") +
#  geom_segment( data=phyt_for_plot, aes ( x=quantile(log2( WT ))[[2]] , y=43, 
#                   xend=quantile(log2( WT ))[[4]] , yend=43 ), inherit.aes = FALSE, 
#                size = 4, color="grey40") +
  geom_text ( x =mean ( c(log2( min ( phyt_for_plot$WT, na.rm=T ) ), log2( max ( phyt_for_plot$WT, na.rm=T ) )) ),
             y=45, label = "Phytoplankton", size=5, inherit.aes = FALSE, color="Grey50" ) +
  
  #Rotifer line
  geom_segment( data=roti_for_plot, aes ( x=log2( min ( WT, na.rm=T ) ) , y=32, 
                                          xend=log2( max ( WT, na.rm=T ) ) , yend=32 ), inherit.aes = FALSE,
                size = 1, color="Grey50") +
#  geom_segment( data=roti_for_plot, aes ( x=quantile(log2( WT ))[[2]] , y=32, 
#                                          xend=quantile(log2( WT ))[[4]] , yend=32 ), inherit.aes = FALSE, 
#                size = 4, color="grey40") +
  geom_text ( x =mean ( c(log2( min ( roti_for_plot$WT, na.rm=T ) ), log2( max ( roti_for_plot$WT, na.rm=T ) )) ),
              y=34, label = "Nauplii, Rotifers, & Veligers", size=4, inherit.aes = FALSE, color="Grey50" ) +
  
  #Cladoceran/Copepod line
  geom_segment( data=zoop_for_plot, aes ( x=log2( min ( WT, na.rm=T ) ) , y=25, 
                                          xend=log2( max ( WT, na.rm=T ) ) , yend=25 ), inherit.aes = FALSE,
                size = 1, color="Grey50") +
#  geom_segment( data=zoop_for_plot, aes ( x=quantile(log2( WT ))[[2]] , y=25, 
#                                          xend=quantile(log2( WT ))[[4]] , yend=25 ), inherit.aes = FALSE, 
#                size = 4, color="grey40") +
  geom_text ( x =mean ( c(log2( min ( zoop_for_plot$WT, na.rm=T ) ), log2( max ( zoop_for_plot$WT, na.rm=T ) )) ),
              y=27, label = "Cladocerans & Copepods", size=4, inherit.aes = FALSE, color="Grey50" ) +
  
  #Add in Bythotrephes
  geom_segment( data=zoop.work %>% filter (EPA_COMBONAME == "Bythotrephes cederstroemi" & LAKE == "Superior" & YEAR == 2007), 
                aes ( x=log2( min ( WT*10^-6, na.rm=T ) ) , y=24, 
                      xend=log2( max ( WT*10^-6, na.rm=T ) ) , yend=24 ), inherit.aes = FALSE,
                size = 1, color="black") +
  
  geom_text ( x = -10.5,
              y=23, label = "Bythotrephes", size=4, color="Black" ) +
  
  #Mysid line
  geom_segment( data=mysis_for_plot, aes ( x=log2( min ( WT, na.rm=T ) ) , y=17, 
                                          xend=log2( max ( WT, na.rm=T ) ) , yend=17 ), inherit.aes = FALSE,
                size = 1, color="Grey50" ) +
#  geom_segment( data=mysis_for_plot, aes ( x=quantile(log2( WT ))[[2]] , y=18, 
#                                          xend=quantile(log2( WT ))[[4]] , yend=18 ), inherit.aes = FALSE, 
#                size = 4, color="grey40") +
  geom_text ( x =mean ( c(log2( min ( mysis_for_plot$WT, na.rm=T ) ), log2( max ( mysis_for_plot$WT, na.rm=T ) )) ),
              y=19, label = "Mysis", size=4, inherit.aes = FALSE, color="Grey50" )
  
ggsave( "size spec example.pdf", path=.FIGURE_OUTPUT_LOC, 
        plot_ssa, width = 9, height = 6, units="cm", scale=2.5, dpi=400 )

#### Benthic data for plot ####
output_benthic_for_plot <- bent.work %>%
  filter ( YEAR < 2016 )

output_benthic_for_plot$LAKE <- factor( output_benthic_for_plot$LAKE , levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

#### Benthic hist plot ####
plot_ssa <-  
  ggplot(output_benthic_for_plot,
         aes(log2(AVGSIZE), fill=DENSITY_GROUP) ) +
  geom_histogram ( alpha = 1/2, bins=30 ) +
  facet_grid ( YEAR ~ LAKE ) +

  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = "Number of samples" ) +
  
  theme_bw() + 
  theme ( panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key=element_blank(),
          strip.text.x = element_text(size = 10 ),
          strip.text.y = element_text(size = 10 ),
          axis.text = element_text(size = 12, color="black" ),
          axis.text.x = element_text(angle = 270, vjust=0.4),
          text = element_text ( size = 12 ) ) +
  
  scale_fill_brewer(palette="Paired" )
  
ggsave( "benthic hist.pdf", path=.FIGURE_OUTPUT_LOC, 
        plot_ssa, width = 10, height = 6, units="cm", scale=2.5, dpi=400 )

#### Evaluate whether slopes and heights are correlated ####
output_for_plot <- fits.yr.lk %>%
  filter ( YEAR < 2016 ) %>% 
  filter ( SEASON != "both" ) %>%
  filter ( MISSINGPHYTO == "N" )

#### Slope vs Height graph ####
slpvht <- 
  ggplot ( output_for_plot, aes(SLOPE, INTCPT, color=SEASON, shape=LAKE) ) +
  geom_point( alpha = 0.9, size=3 ) +
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer")) +
  scale_shape_manual(values=c("S", "M", "H","W","C","E","O"))+
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12 ) ) +
  guides ( fill = guide_legend(override.aes = list(linetype = 0) ),
           color = guide_legend(override.aes = list(linetype = 0) ) ) 

ggsave( "slope_intcept_cor_seasons.pdf", path=.FIGURE_OUTPUT_LOC, 
        slpvht, width = 4.5, height = 4.5, units="cm", scale=2.5, dpi=400 )

slpvht <- 
  ggplot ( output_for_plot, aes(SLOPE, HEIGHT, color=SEASON, shape=LAKE) ) +
  geom_point( alpha = 0.9, size=3 ) +
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer")) +
  scale_shape_manual(values=c("S", "M", "H","W","C","E","O"))+
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12 ) ) +
  guides ( fill = guide_legend(override.aes = list(linetype = 0) ),
           color = guide_legend(override.aes = list(linetype = 0) ) )

ggsave( "slope_height_cor_seasons_mid float.pdf", path=.FIGURE_OUTPUT_LOC, 
        slpvht, width = 4.5, height = 4.5, units="cm", scale=2.5, dpi=150 )

slpvht <- 
  ggplot ( output_for_plot, aes(SLOPE, HEIGHT_OVERALL, color=SEASON, shape=LAKE) ) +
  geom_point( alpha = 0.9, size=3 ) +
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer")) +
  scale_shape_manual(values=c("S", "M", "H","W","C","E","O"))+
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12 ) ) +
  guides ( fill = guide_legend(override.aes = list(linetype = 0) ),
           color = guide_legend(override.aes = list(linetype = 0) ) ) +
  ylab ("Height") + xlab ( "Slope")

ggsave( "slope_height_cor_seasons_mid overall.pdf", path=.FIGURE_OUTPUT_LOC, 
        slpvht, width = 4.5, height = 4.5, units="cm", scale=2.5, dpi=400 )

#### Height boxplots ####
output_for_plot$SEASON = ifelse( output_for_plot$SEASON == "spring", "Spring",
                                 ifelse( output_for_plot$SEASON == "summer", "Summer", "Both") )

ht <-
  ggplot ( output_for_plot , aes(LAKE, HEIGHT_OVERALL, fill=SEASON) ) +
  geom_boxplot ( alpha = 0.9, size=0.5 ) +
  facet_grid ( . ~ SEASON ) +
  scale_fill_manual(values=color_custom, name="Season" ) +
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4, color="black" ),
          axis.text.y = element_text( color="black" ), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 14 ),
          text = element_text ( size = 18, color="black" ),
          legend.position = "none") +
  ylab ( expression ( paste ( "Height (Log"[2], "[Individuals/m"^2,"])" ) ) )  +
  xlab ( "Lake" )

ggsave( "height_seasons_mid overall_Boxplot.pdf", path=.FIGURE_OUTPUT_LOC, 
        ht, width = 4.5, height = 3, units="cm", scale=5, dpi=400 )

#### Slope boxplots ####
sl <-
  ggplot ( output_for_plot , aes(LAKE, SLOPE, fill=SEASON) ) +
  geom_boxplot ( alpha = 0.9, size=0.5 ) +
  facet_grid ( . ~ SEASON ) +
  scale_fill_manual(values=color_custom, name="Season" ) +
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4, color="black"),
          axis.text.y = element_text(color="black"),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 14 ),
          text = element_text ( size = 18 ),
          legend.position = "none") +
  ylab ( "Slope" )  +
  xlab ( "Lake" )

ggsave( "slope_seasons_Boxplot.pdf", path=.FIGURE_OUTPUT_LOC, 
        sl, width = 4.5, height = 3, units="cm", scale=5, dpi=400 )

#### Linear fits of benthic size spectrum ###
#### Set up fits dataframe ####
fits.yr.lk.be = data.frame ( matrix ( NA, ncol = 16, nrow = length( unique (output.bent$LAKE) ) * (length( unique( (output.bent$YEAR[output.bent$YEAR<2016] ) ) ) ) ) )
colnames ( fits.yr.lk.be ) = c( "YEAR", "LAKE", "SEASON", "OUTLIERPRES" ,"N", "FITMIN", "FITMAX", "SLOPE", "SLP_SE", "SLP_L95CI", "SLP_U95CI", "INTCPT", "INT_SE", "INT_L95CI", "INT_U95CI", "HEIGHT")

fits.yr.lk.be$YEAR = rep ( unique( (output.bent$YEAR[output.bent$YEAR<2016] ) ) , each=length(unique (output.bent$LAKE)) )
fits.yr.lk.be$OUTLIERPRES = factor ( fits.yr.lk.be$OUTLIERPRES, levels = c( "Y","N" ) )

fits.yr.lk.be$LAKE = as.factor ( unique (output.bent$LAKE) )
fits.yr.lk.be$LAKE = factor ( fits.yr.lk.be$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

fits.yr.lk.be$SEASON = "summer"
fits.yr.lk.be$SEASON = factor (  fits.yr.lk.be$SEASON, levels=c("spring", "summer", "both") )

#### Set up short dataframe for benthic data fits ####
short.output.ben = setNames ( data.frame ( matrix ( NA, ncol=6, nrow=0 ) ),  c( "YEAR", "LAKE", "SEASON", "ABUN_LOG", "BINMID_LOG", "DATAUSEDINFIT" ) ) #These data will be easier to export as well since it only has 6 columns

short.output.ben$LAKE = factor ( short.output.ben$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )
short.output.ben$SEASON = factor ( short.output.ben$SEASON, levels=c("spring", "summer", "both") )
short.output.ben$DATAUSEDINFIT = factor ( short.output.ben$DATAUSEDINFIT, levels=c("used", "dropped") )


#### Loop for filling benthic fits with breaks ####
for ( i in 1:nrow(fits.yr.lk.be) ) {
  
  tempdata = output.bent  %>% 
    filter ( YEAR == fits.yr.lk.be$YEAR[i] )%>%
    filter ( LAKE == fits.yr.lk.be$LAKE[i] )
  
  templin = lm ( ABUN_LOG ~ BINMID_LOG , data = tempdata )
  
  tempjoin  = data.frame ( "YEAR" = tempdata$YEAR, "LAKE" = tempdata$LAKE, "SEASON" = tempdata$SEASON, "ABUN_LOG" = tempdata$ABUN_LOG, "BINMID_LOG" = tempdata$BINMID_LOG, "DATAUSEDINFIT"="used" ) 
  short.output.ben = rbind ( short.output.ben, tempjoin )
  
  fits.yr.lk.be$N[i]         = length(templin$residuals)
  fits.yr.lk.be$FITMIN[i]    = min(templin$model$BINMID_LOG, na.rm=T)
  fits.yr.lk.be$FITMAX[i]    = max(templin$model$BINMID_LOG, na.rm=T)
  fits.yr.lk.be$SLOPE[i]     = coefficients(templin)[[2]]
  fits.yr.lk.be$SLP_SE[i]    = coef(summary(templin))[2,2]
  fits.yr.lk.be$SLP_L95CI[i] = confint(templin,level=0.95)[2,1] 
  fits.yr.lk.be$SLP_U95CI[i] = confint(templin,level=0.95)[2,2]
  fits.yr.lk.be$INTCPT[i]    = coefficients(templin)[[1]]
  fits.yr.lk.be$INT_SE[i]    = coef(summary(templin))[1,2]
  fits.yr.lk.be$INT_L95CI[i] = confint(templin,level=0.95)[1,1]
  fits.yr.lk.be$INT_U95CI[i] = confint(templin,level=0.95)[1,2]
  
  fits.yr.lk.be$HEIGHT[i]    = fits.yr.lk.be$SLOPE[i] * ( fits.yr.lk.be$FITMIN[i] + fits.yr.lk.be$FITMAX[i] ) / 2 + fits.yr.lk.be$INTCPT[i]
  
}

fits.yr.lk.be$OUTLIERPRES = "N"
fits.yr.lk.be$DATAUSEDINFITS = "used"

#### fits of height from overall ####
fits.yr.lk.be$HEIGHT_OVERALL = fits.yr.lk.be$SLOPE * ( floor(min(fits.yr.lk.be$FITMIN)) + ceiling(max(fits.yr.lk.be$FITMAX)) ) / 2 + fits.yr.lk.be$INTCPT

#### Output the benthic table with fits ####
write.table ( fits.yr.lk.be, paste (sep="", .DATA_OUTPUT,"fitsdata_benthic_with mid overall_no drop.csv"), sep=",", row.names=FALSE )

#### Size spectra and benthos spectra ####
output_for_plot <- short.output.ben %>%
  filter ( YEAR < 2016 ) %>% 
  mutate ( PLACEMENT = 7.5 ) %>%  #this will allow fits to appear
  left_join( . , fits.yr.lk.be, by = c("YEAR", "SEASON", "LAKE"), all.x=T ) #this join makes it easier for ggplot to take the data, of couse it creates many rep values
output_for_plot$LAKE <- factor( output_for_plot$LAKE , levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

#### Benthic size spectra ####
plot_ssa <-  
  ggplot(output_for_plot,
         aes(BINMID_LOG, ABUN_LOG, shape=DATAUSEDINFIT ) ) +
  geom_point ( alpha = 1/2, size=3, color="purple" ) +
  ylim (-5 , 11) +
  xlim (-25, 5) +
  facet_grid ( YEAR ~ LAKE ) +
  
  geom_smooth( method = lm, se=FALSE ) +
  geom_label( data=output_for_plot, 
              aes( x=-20.5, y=PLACEMENT+2.5, label = format ( round( SLOPE, 2 ), 2 ) ) , show.legend = FALSE ) +
  
  scale_shape_manual( name="Data used in fit", labels=c("Used", "Dropped"), values=c(19,21) ) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          strip.text.x = element_text(size = 12 ),
          strip.text.y = element_text(size = 12 ) )

ggsave( "BASS.pdf", path=.FIGURE_OUTPUT_LOC, 
        plot_ssa, width = 11, height = 6, units="cm", scale=3, dpi=400 )

#### Benthic size spectra slopes ####
plot_ssa <-  
ggplot ( fits.yr.lk.be %>% filter( YEAR <2016 ) , aes(YEAR, SLOPE) ) +
  geom_point( alpha = 0.9, size=3, color="royalblue2" ) +
  geom_line ( alpha = 0.7, size=1.2, color="royalblue2" ) +
  scale_x_discrete(limits=c(2012:2015)) +
  geom_errorbar( aes(ymin=SLP_L95CI, ymax=SLP_U95CI), color="royalblue2" ) +
  facet_grid ( . ~ LAKE ) +
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12 ) )

ggsave( "BASS slopes.pdf", path=.FIGURE_OUTPUT_LOC, 
        plot_ssa, width = 8, height = 2.5, units="cm", scale=3, dpi=400 )


#### Read in table with fits ####
#fits.yr.lk <- read.table (paste (sep="", .DATA_OUTPUT,"fitsdata_phtyo to mysis_mid overall.csv"), sep=",", header=T) #not necessary if you are executing from top but can be used if output is reliable

#these are needed if reading in because it assigns types to the data, if you do not import data this can be skipped
#fits.yr.lk$LAKE <- factor( fits.yr.lk$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East",  "Ontario") )
#fits.yr.lk$YEAR <- as.integer ( sort (fits.yr.lk$YEAR) )
#fits.yr.lk$SEASON <- factor ( fits.yr.lk$SEASON, levels=c("spring", "summer", "both") )
#fits.yr.lk$OUTLIERPRES <- factor ( fits.yr.lk$OUTLIERPRES, levels = c( "Y","N" ) )

#### Create database for slope data ####
summary.fits.slope <- as.data.frame ( matrix (NA, ncol = 7, nrow = length( unique(fits.yr.lk$LAKE) )*3 ) )
colnames ( summary.fits.slope ) <-  c( "LAKE", "SLOPE", "SLP_SE", "SLP_PVALUE", "RSQR", "INTERCEPT", "INT_PVALUE" )

#### Summarize fit slope data ####
for (i in 1:length( unique(fits.yr.lk$LAKE) ) ) {
  
  summary.fits.slope$LAKE[c((3*i-2):(3*i))] <- as.character ( unique (fits.yr.lk$LAKE)[i] )
  
  for (j in 1:length(unique(fits.yr.lk$SEASON) ) ) {
    
    summary.fits.slope$SEASON[3*i-3 + j] <- as.character ( unique(fits.yr.lk$SEASON)[j] )
    
    y = fits.yr.lk$SLOPE [ fits.yr.lk$LAKE == summary.fits.slope$LAKE[3*i-3 + j] & fits.yr.lk$MISSINGPHYTO == "N" & fits.yr.lk$SEASON == summary.fits.slope$SEASON[3*i-3 + j] & fits.yr.lk$YEAR < 2016 ]
    x = fits.yr.lk$YEAR  [ fits.yr.lk$LAKE == summary.fits.slope$LAKE[3*i-3 + j] & fits.yr.lk$MISSINGPHYTO == "N" & fits.yr.lk$SEASON == summary.fits.slope$SEASON[3*i-3 + j] & fits.yr.lk$YEAR < 2016 ]
    linmodel <- lm ( y ~ as.numeric( x ) )
    
    summary.fits.slope$SLOPE[3*i-3 + j]      <- linmodel$coefficients[[2]]
    summary.fits.slope$SLP_SE[3*i-3 + j]     <- coef(summary(linmodel))[2,2]
    summary.fits.slope$SLP_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[2,4]
    summary.fits.slope$RSQR[3*i-3 + j]       <- summary(linmodel)$r.squared
    summary.fits.slope$INTERCEPT[3*i-3 + j]  <- linmodel$coefficients[[1]]
    summary.fits.slope$INT_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[1,4]
    
  }
}

summary.fits.slope$LAKE <- factor(summary.fits.slope$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario"))
summary.fits.slope$SEASON <- factor ( summary.fits.slope$SEASON, levels = c("spring", "summer", "both") ) 

#### Output linear relationships of slope ####
write.table ( summary.fits.slope, paste (sep="", .DATA_OUTPUT,"slope fits_PASS.csv"), sep=",", row.names=FALSE )

#### Plot slopes ####
slope_output <- 
  ggplot ( fits.yr.lk %>% filter( MISSINGPHYTO == "N" & SEASON != "both" & YEAR <2016 ) , aes(YEAR, SLOPE, color=SEASON) ) +
  geom_point( alpha = 0.9, size=3 ) +
  geom_path ( alpha = 0.7, size=1.2) +
  scale_x_discrete(limits=c(2006:2015)) +
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer")) +
  geom_errorbar( aes(ymin=SLP_L95CI, ymax=SLP_U95CI) ) +
  facet_grid ( . ~ LAKE ) +
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12 ) ) +
  #geom_abline ( data=summary.fits.slope %>% filter(SEASON != "both" ) %>% filter(SLP_PVALUE < 0.05) , 
  #              aes ( intercept = INTERCEPT , slope = SLOPE, color=SEASON),
  #              size=1.5, alpha=1/2 ) +
  #geom_hline ( data=fits.yr.lk %>% filter(SEASON != "both" & YEAR < 2016 & MISSINGPHYTO == "N") %>% group_by(SEASON, LAKE) %>% summarise( SLP = mean(SLOPE) ) , 
  #             aes ( yintercept = SLP , color=SEASON),
  #             size=1.5, alpha=1/2, lty=4) +
  #geom_hline ( aes(yintercept=-1), size=1.5, alpha=1/2, lty=1, col="black" ) +
  guides ( fill = guide_legend(override.aes = list(linetype = 0) ),
           color = guide_legend(override.aes = list(linetype = 0) ) )

ggsave( "slope_comparisions_seasons_mid overall.pdf", path=.FIGURE_OUTPUT_LOC, 
        slope_output, width = 11, height = 4.5, units="cm", scale=2.5, dpi=400 )

#### Summarize fit height data ####
summary.fits.height <- as.data.frame ( matrix (NA, ncol = 7, nrow = length( unique(fits.yr.lk$LAKE) )*3 ) )
colnames ( summary.fits.height ) <-  c( "LAKE", "SLOPE", "SLP_SE", "SLP_PVALUE", "RSQR", "INTERCEPT", "INT_PVALUE" )

for (i in 1:length( unique(fits.yr.lk$LAKE) ) ) {
  
  summary.fits.height$LAKE[c((3*i-2):(3*i))] <- as.character ( unique (fits.yr.lk$LAKE)[i] )
  
  for (j in 1:length(unique(fits.yr.lk$SEASON) ) ) {
    
    summary.fits.height$SEASON[3*i-3 + j] <- as.character ( unique(fits.yr.lk$SEASON)[j] )
    
    y = fits.yr.lk$HEIGHT_OVERALL[ fits.yr.lk$LAKE == summary.fits.height$LAKE[3*i-3 + j] & fits.yr.lk$MISSINGPHYTO == "N" & fits.yr.lk$SEASON == summary.fits.height$SEASON[3*i-3 + j] & fits.yr.lk$YEAR < 2016 ]
    x = fits.yr.lk$YEAR  [ fits.yr.lk$LAKE == summary.fits.height$LAKE[3*i-3 + j] & fits.yr.lk$MISSINGPHYTO == "N" & fits.yr.lk$SEASON == summary.fits.height$SEASON[3*i-3 + j] & fits.yr.lk$YEAR < 2016 ]
    linmodel <- lm ( y ~ as.numeric( x ) )
    
    summary.fits.height$SLOPE[3*i-3 + j]      <- linmodel$coefficients[[2]]
    summary.fits.height$SLP_SE[3*i-3 + j]     <- coef(summary(linmodel))[2,2]
    summary.fits.height$SLP_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[2,4]
    summary.fits.height$RSQR[3*i-3 + j]       <- summary(linmodel)$r.squared
    summary.fits.height$INTERCEPT[3*i-3 + j]  <- linmodel$coefficients[[1]]
    summary.fits.height$INT_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[1,4]
    
  }
}
summary.fits.height$HEIGHT_TYPE <- "overall"

#### Output linear relationships of slope ####
write.table ( summary.fits.height, paste (sep="", .DATA_OUTPUT,"height overall fits_PASS.csv"), sep=",", row.names=FALSE )

#### Create a new table for height float ####
summary.fits.height.temp <- as.data.frame ( matrix (NA, ncol = 7, nrow = length( unique(fits.yr.lk$LAKE) )*3 ) )

#### Heights fits with float ####
colnames ( summary.fits.height.temp ) <-  c( "LAKE", "SLOPE", "SLP_SE", "SLP_PVALUE", "RSQR", "INTERCEPT", "INT_PVALUE" )

for (i in 1:length( unique(fits.yr.lk$LAKE) ) ) {
  
  summary.fits.height.temp$LAKE[c((3*i-2):(3*i))] <- as.character ( unique (fits.yr.lk$LAKE)[i] )
  
  for (j in 1:length(unique(fits.yr.lk$SEASON) ) ) {
    
    summary.fits.height.temp$SEASON[3*i-3 + j] <- as.character ( unique(fits.yr.lk$SEASON)[j] )
    
    y = fits.yr.lk$HEIGHT[ fits.yr.lk$LAKE == summary.fits.height.temp$LAKE[3*i-3 + j] & fits.yr.lk$MISSINGPHYTO == "N" & fits.yr.lk$SEASON == summary.fits.height.temp$SEASON[3*i-3 + j] & fits.yr.lk$YEAR < 2016 ]
    x = fits.yr.lk$YEAR  [ fits.yr.lk$LAKE == summary.fits.height.temp$LAKE[3*i-3 + j] & fits.yr.lk$MISSINGPHYTO == "N" & fits.yr.lk$SEASON == summary.fits.height.temp$SEASON[3*i-3 + j] & fits.yr.lk$YEAR < 2016 ]
    linmodel <- lm ( y ~ as.numeric( x ) )
    
    summary.fits.height.temp$SLOPE[3*i-3 + j]      <- linmodel$coefficients[[2]]
    summary.fits.height.temp$SLP_SE[3*i-3 + j]     <- coef(summary(linmodel))[2,2]
    summary.fits.height.temp$SLP_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[2,4]
    summary.fits.height.temp$RSQR[3*i-3 + j]       <- summary(linmodel)$r.squared
    summary.fits.height.temp$INTERCEPT[3*i-3 + j]  <- linmodel$coefficients[[1]]
    summary.fits.height.temp$INT_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[1,4]
    
  }
}
summary.fits.height.temp$HEIGHT_TYPE <- "float"

#### Combine the two types of height fits ####
summary.fits.height <- rbind( summary.fits.height, summary.fits.height.temp )

summary.fits.height$LAKE <- factor(summary.fits.height$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario"))
summary.fits.height <- summary.fits.height[order(summary.fits.height$LAKE),] 

#### Plot heights ####
height_output <-
  ggplot ( fits.yr.lk %>% filter( MISSINGPHYTO == "N" & SEASON != "both" & YEAR < 2016 ) , aes(YEAR, HEIGHT_OVERALL, color=SEASON) ) +
  geom_point( alpha = 0.9, size=3 ) +
  geom_path ( alpha = 0.7, size=1.2) +
  scale_x_discrete(limits=c(2006:2015)) +
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer")) +
  geom_errorbar( aes(ymin=HEIGHT_OVERALL - (INTCPT-INT_L95CI), ymax=HEIGHT_OVERALL + (INT_U95CI-INTCPT) ) ) +
  facet_grid ( . ~ LAKE ) +
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(), 
          strip.text.x = element_text(size = 12 ) ) +
#  geom_abline ( data=summary.fits.height %>%
#                  filter ( HEIGHT_TYPE == "overall" ) %>%
#                  filter ( SEASON != "both" ) %>%
#                  filter(SLP_PVALUE < 0.05) , 
#                aes ( intercept = INTERCEPT , slope = SLOPE, color=SEASON),
#                size=1.5, alpha=1/2 ) +
  geom_hline ( data = fits.yr.lk %>% 
                 filter(SEASON != "both" & YEAR < 2016 & MISSINGPHYTO == "N") %>% 
                 group_by(SEASON, LAKE) %>%  
                 summarise(HEIGHT = mean(HEIGHT_OVERALL, na.rm=T) ),
               aes ( yintercept=HEIGHT, color=SEASON ), lty=4, alpha=0.5, size=1.5 ) +
  guides ( fill = guide_legend(override.aes = list(linetype = 0) ),
           color = guide_legend(override.aes = list(linetype = 0) ) ) +
  labs ( x="Year", y="Height")

ggsave( "height_comparisions_seasons_height overall.pdf", path=.FIGURE_OUTPUT_LOC, 
        height_output, width = 11, height = 4.5, units="cm", scale=2.5, dpi=150 )

height_output <-
  ggplot ( fits.yr.lk %>% filter( MISSINGPHYTO == "N" & SEASON != "both" & YEAR < 2016 ) , aes(YEAR, HEIGHT, color=SEASON) ) +
  geom_point( alpha = 0.9, size=3 ) +
  geom_path ( alpha = 0.7, size=1.2) +
  scale_x_discrete(limits=c(2006:2015)) +
  scale_color_manual(values=color_custom, name="Season", labels=c("Spring", "Summer")) +
  geom_errorbar( aes(ymin=HEIGHT - (INTCPT-INT_L95CI), ymax=HEIGHT + (INT_U95CI-INTCPT) ) ) +
  facet_grid ( . ~ LAKE ) +
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(), 
          strip.text.x = element_text(size = 12 ) ) +
  geom_abline ( data=summary.fits.height %>%
                  filter ( HEIGHT_TYPE == "float" ) %>%
                  filter ( SEASON != "both" ) %>%
                  filter ( SLP_PVALUE < 0.05 ) , 
                aes ( intercept = INTERCEPT , slope = SLOPE, color=SEASON ),
                size=1.5, alpha=1/2 ) +
  geom_hline ( data = fits.yr.lk %>% 
                 filter(SEASON != "both" & YEAR < 2016 & MISSINGPHYTO == "N") %>% 
                 group_by(SEASON, LAKE) %>%  
                 summarise(HEIGHT = mean(HEIGHT, na.rm=T) ),
               aes ( yintercept=HEIGHT, color=SEASON ), lty=4, alpha=0.5, size=1.5 ) +
  guides ( fill = guide_legend(override.aes = list(linetype = 0) ),
           color = guide_legend(override.aes = list(linetype = 0) ) ) +
  labs ( x="YEAR", y="HEIGHT (FLOAT)")

ggsave( "height_comparisions_seasons_mid float.pdf", path=.FIGURE_OUTPUT_LOC, 
        height_output, width = 11, height = 4.5, units="cm", scale=2.5, dpi=150 )

#### Bring data together ####
output <- rbind (output.mys, output.rot, output.d100, output.phyt, output.bent.store) #this is the whole enchilada

#### Drop NA's #####
#These are introduced in benthic data and can be removed here
output <- output %>% drop_na ( )

#calculate the number of log2 bins, taken from Edwards et al. 2017 and assign the bins
log2bins.list <- log2bins(output$WT) #ignore the warning, we don't have counts we have abundance
output$BINMID <- log2bins.list$indiv$binMid
output$BINMIN <- log2bins.list$indiv$binMin
output$BINMAX <- log2bins.list$indiv$binMax

output <- setNames ( aggregate ( AVGABUN ~ YEAR + LAKE + SEASON + as.factor(output$BINMID) + BINMIN + BINMAX, sum, data=output ),
                     c("YEAR", "LAKE", "SEASON", "BINMID", "BINMIN", "BINMAX", "SUMAVGABUN") )
#BINMIN and BINMAX are included here but will not do anything, but by including they will be carried through to the summary table and will be usuable for relative density
output$BINMID <- as.numeric ( as.character (output$BINMID) )
output <- as_tibble (output) #save this in tiddyverse form

output <- output %>% 
  group_by ( YEAR, LAKE, SEASON, BINMID, BINMIN, BINMAX ) %>%
  mutate ( BINMID_LOG = log2(BINMID), BINMIN_LOG = log2(BINMIN), BINMAX_LOG = log2(BINMAX), ABUN_LOG = log2(SUMAVGABUN) )

output$LAKE <- factor(output$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario"))

#### Output the SSA data ####
write.table(output, paste ( sep="", .DATA_OUTPUT, "output_phyto to mysis_benthos womysis.csv"), sep=",", row.names = FALSE)

#### Linear fits of size spectrum ####
#### Set up fits dataframe ####
fits.yr.lk.bent = data.frame ( matrix ( NA, ncol = 16, nrow = length( unique (output$LAKE) ) * (length( unique (output$YEAR) )-1) * (length( unique (output$SEASON) ) + 1) ) )
colnames ( fits.yr.lk.bent ) = c( "YEAR", "LAKE", "SEASON", "OUTLIERPRES" ,"N", "FITMIN", "FITMAX", "SLOPE", "SLP_SE", "SLP_L95CI", "SLP_U95CI", "INTCPT", "INT_SE", "INT_L95CI", "INT_U95CI", "HEIGHT")

fits.yr.lk.bent$YEAR = rep( sort( unique (output$YEAR) )[-12], each = length(unique(output$LAKE) ) * ( length(unique(output$SEASON) ) + 1 ) )
fits.yr.lk.bent$LAKE = rep( as.factor ( unique (output$LAKE) ), each = 3 )
fits.yr.lk.bent$SEASON = factor ( rep( c(as.character ( unique (output$SEASON) ), "both"), 1 ), levels=c("spring", "summer", "both") )
fits.yr.lk.bent$OUTLIERPRES = factor ( fits.yr.lk.bent$OUTLIERPRES, levels = c( "Y","N" ) )

#### Set up short dataframe ####
short.output.bent = setNames ( data.frame ( matrix ( NA, ncol=6, nrow=0 ) ),  c( "YEAR", "LAKE", "SEASON", "ABUN_LOG", "BINMID_LOG", "DATAUSEDINFIT" ) ) #These data will be easier to export as well since it only has 6 columns

short.output.bent$LAKE = factor ( short.output.bent$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )
short.output.bent$SEASON = factor ( short.output.bent$SEASON, levels=c("spring", "summer", "both") )

#### Loop for filling with breaks ####
for ( i in 1:nrow(fits.yr.lk.bent) ) {
  
  
  if (fits.yr.lk.bent$SEASON[i] == "spring" || fits.yr.lk.bent$SEASON[i] == "summer") {
    tempdata = output  %>% 
      filter ( YEAR == fits.yr.lk.bent$YEAR[i] )%>%
      filter ( LAKE == fits.yr.lk.bent$LAKE[i] )%>%
      filter ( SEASON == fits.yr.lk.bent$SEASON[i] )
    
    templin = lm ( ABUN_LOG ~ BINMID_LOG , data = tempdata )
    
    cookcutoff = 10/length(templin$residuals) #set your Cook's value, the general rule is 4/n for detection of influence, 1 is a rough cutoff for points with too much leverage
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, "dropped", "used" )
    tempjoin  = data.frame ( "YEAR" = tempdata$YEAR, "LAKE" = tempdata$LAKE, "SEASON" = tempdata$SEASON, "ABUN_LOG" = tempdata$ABUN_LOG, "BINMID_LOG" = tempdata$BINMID_LOG, "DATAUSEDINFIT"=tempcooks ) 
    short.output.bent = rbind ( short.output.bent, tempjoin )
    
    if ( any ( cooks.distance(templin) > cookcutoff ) ) { #remove points with high leverage or just save the lm as it is
      fits.yr.lk.bent$OUTLIERPRES = "Y"
      templin = lm ( ABUN_LOG [-which (cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data=tempdata ) }
    else { fits.yr.lk.bent$OUTLIERPRES = "N" }  #report that no high leverage points were found
    
    
  } else { #this will be for the "both" in season, but if additional seasons are added we need to change this line to be more specific
    tempdata = output  %>% 
      filter ( YEAR == fits.yr.lk.bent$YEAR[i] ) %>%
      filter ( LAKE == fits.yr.lk.bent$LAKE[i] )
    
    templin = lm ( ABUN_LOG ~ BINMID_LOG , data = tempdata )
    
    cookcutoff = 10/length(templin$residuals)
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, "dropped", "used" )
    tempjoin  = data.frame ( "YEAR" = tempdata$YEAR, "LAKE" = tempdata$LAKE, "SEASON" = "both", "ABUN_LOG" = tempdata$ABUN_LOG, "BINMID_LOG" = tempdata$BINMID_LOG, "DATAUSEDINFIT"=tempcooks ) 
    short.output.bent = rbind ( short.output.bent, tempjoin )
    
    if ( any ( cooks.distance(templin) > cookcutoff ) ) { #remove points with high leverage or just save the lm as it is
      fits.yr.lk.bent$OUTLIERPRES = "Y"
      templin = lm ( ABUN_LOG [-which (cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data=tempdata ) }
    else { fits.yr.lk.bent$OUTLIERPRES = "N" }  #report that no high leverage points were found
    
  }
  
  fits.yr.lk.bent$N[i]         = length(templin$residuals)
  fits.yr.lk.bent$FITMIN[i]    = min(templin$model$BINMID_LOG, na.rm=T)
  fits.yr.lk.bent$FITMAX[i]    = max(templin$model$BINMID_LOG, na.rm=T)
  fits.yr.lk.bent$SLOPE[i]     = coefficients(templin)[[2]]
  fits.yr.lk.bent$SLP_SE[i]    = coef(summary(templin))[2,2]
  fits.yr.lk.bent$SLP_L95CI[i] = confint(templin,level=0.95)[2,1] 
  fits.yr.lk.bent$SLP_U95CI[i] = confint(templin,level=0.95)[2,2]
  fits.yr.lk.bent$INTCPT[i]    = coefficients(templin)[[1]]
  fits.yr.lk.bent$INT_SE[i]    = coef(summary(templin))[1,2]
  fits.yr.lk.bent$INT_L95CI[i] = confint(templin,level=0.95)[1,1]
  fits.yr.lk.bent$INT_U95CI[i] = confint(templin,level=0.95)[1,2]
  
  temp.short <- tempjoin %>%
    filter ( DATAUSEDINFIT == "used" )
  
  fits.yr.lk.bent$HEIGHT[i]    = fits.yr.lk.bent$SLOPE[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lk.bent$INTCPT[i]
  
}

#### fits of height from overall ####
fits.yr.lk.bent$HEIGHT_OVERALL = fits.yr.lk.bent$SLOPE * ( min(fits.yr.lk.bent$FITMIN) + max(fits.yr.lk.bent$FITMAX) ) / 2 + fits.yr.lk.bent$INTCPT

#### Deal with missing phyto data ####
miss.data <- setNames ( adply ( table (output.phyt$YEAR, output.phyt$LAKE, output.phyt$SEASON ), c(1,2,3) ), c( "YEAR", "LAKE", "SEASON", "OBSERVATIONS" ) ) #report from phyto when data is missing
miss.data$YEAR = as.numeric ( as.character (miss.data$YEAR) ) #quick conversion issue removal, otherwise year is a factor, direct conversion will tell R to assume it starts at 1 and count up for each level
miss.data$MISSINGPHYTO <- factor ( ifelse ( miss.data$OBSERVATIONS == 0, "Y", "N") ) #characterize missing data

#now we need to control for the "both" season
both.data <- miss.data[miss.data$SEASON == "spring",]
both.data$SEASON <- "both"
temp.vec.spr <- miss.data$MISSINGPHYTO[ miss.data$SEASON=="spring" ] #extract spring vector for missing
temp.vec.sum <- miss.data$MISSINGPHYTO[ miss.data$SEASON=="summer" ] #extract summer vector for missing
both.data$MISSINGPHYTO <- factor ( ifelse ( temp.vec.spr == "N" & temp.vec.sum == "N", "N", "Y" ) ) #logical test for both, fails if either spring or summer is missing
miss.data <- rbind (miss.data, both.data) #finally done here, but now we can move on to the
miss.data$LAKE = factor ( miss.data$LAKE , levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

# combine it with output data
short.output.bent$YEAR <- as.numeric( as.character (short.output.bent$YEAR) )

short.output.bent <- miss.data %>%
  select ( YEAR, LAKE, SEASON, MISSINGPHYTO ) %>%
  left_join( short.output.bent , ., by = c("YEAR", "LAKE", "SEASON"), all.x=T )

short.output.bent$LAKE = factor ( short.output.bent$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

#and with the fits database
fits.yr.lk.bent$YEAR <- as.numeric( as.character (fits.yr.lk.bent$YEAR) )

fits.yr.lk.bent <- miss.data %>%
  select ( YEAR, LAKE, SEASON, MISSINGPHYTO ) %>%
  left_join( fits.yr.lk.bent , ., by = c("YEAR", "LAKE", "SEASON"), all.x=T )

fits.yr.lk.bent$LAKE = factor ( fits.yr.lk.bent$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario") )

#### Output the table with fits ####
write.table(fits.yr.lk.bent, paste ( sep="", .DATA_OUTPUT, "fitsdata_mid overall_PBASS.csv"), sep=",", row.names=FALSE )

#### Create database for slope data ####
summary.fits.slope.bent <- as.data.frame ( matrix (NA, ncol = 7, nrow = length( unique(fits.yr.lk.bent$LAKE) )*3 ) )
colnames ( summary.fits.slope.bent ) <-  c( "LAKE", "SLOPE", "SLP_SE", "SLP_PVALUE", "RSQR", "INTERCEPT", "INT_PVALUE" )

#### Summarize fit slope data ####
for (i in 1:length( unique(fits.yr.lk.bent$LAKE) ) ) {
  
  summary.fits.slope.bent$LAKE[c((3*i-2):(3*i))] <- as.character ( unique (fits.yr.lk.bent$LAKE)[i] )
  
  for (j in 1:length(unique(fits.yr.lk.bent$SEASON) ) ) {
    
    summary.fits.slope.bent$SEASON[3*i-3 + j] <- as.character ( unique(fits.yr.lk.bent$SEASON)[j] )
    
    y = fits.yr.lk.bent$SLOPE [ fits.yr.lk.bent$LAKE == summary.fits.slope.bent$LAKE[3*i-3 + j] & fits.yr.lk.bent$MISSINGPHYTO == "N" & fits.yr.lk.bent$SEASON == summary.fits.slope.bent$SEASON[3*i-3 + j] & fits.yr.lk.bent$YEAR < 2016 ]
    x = fits.yr.lk.bent$YEAR  [ fits.yr.lk.bent$LAKE == summary.fits.slope.bent$LAKE[3*i-3 + j] & fits.yr.lk.bent$MISSINGPHYTO == "N" & fits.yr.lk.bent$SEASON == summary.fits.slope.bent$SEASON[3*i-3 + j] & fits.yr.lk.bent$YEAR < 2016 ]
    linmodel <- lm ( y ~ as.numeric( x ) )
    
    summary.fits.slope.bent$SLOPE[3*i-3 + j]      <- linmodel$coefficients[[2]]
    summary.fits.slope.bent$SLP_SE[3*i-3 + j]     <- coef(summary(linmodel))[2,2]
    summary.fits.slope.bent$SLP_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[2,4]
    summary.fits.slope.bent$RSQR[3*i-3 + j]       <- summary(linmodel)$r.squared
    summary.fits.slope.bent$INTERCEPT[3*i-3 + j]  <- linmodel$coefficients[[1]]
    summary.fits.slope.bent$INT_PVALUE[3*i-3 + j] <- coef(summary(linmodel))[1,4]
    
  }
}

summary.fits.slope.bent$LAKE <- factor(summary.fits.slope.bent$LAKE, levels = c("Superior", "Michigan", "Huron", "Erie-Western", "Erie-Central", "Erie-East", "Ontario"))
summary.fits.slope.bent$SEASON <- factor ( summary.fits.slope.bent$SEASON, levels = c("spring", "summer", "both") ) 

#### Temporary output for plot ####
temp.1 <- fits.yr.lk.bent %>% filter( MISSINGPHYTO == "N" & SEASON == "summer" & YEAR <2016 & YEAR > 2011 ) %>% mutate ( SIZESPECTYPE = "benthic")
temp.2 <- fits.yr.lk      %>% filter( MISSINGPHYTO == "N" & SEASON == "summer" & YEAR <2016 & YEAR > 2011 ) %>% mutate ( SIZESPECTYPE = "pelagic")
temp   <- rbind (temp.1, temp.2)

#### Plot slopes ####
slope_output <- 
  ggplot ( temp , aes(YEAR, SLOPE, color=SIZESPECTYPE) ) +
  geom_point( alpha = 0.9, size=3 ) +
  geom_path ( alpha = 0.7, size=1.2) +
  scale_x_discrete(limits=c(2012:2015)) +
  scale_color_manual(values=color_custom2, name="Size Spectra includes", labels=c("Benthic and Pelagic", "Pelagic only")) +
  geom_errorbar( aes(ymin=SLP_L95CI, ymax=SLP_U95CI) ) +
  facet_grid ( . ~ LAKE ) +
  theme_bw() + 
  theme ( axis.text.x = element_text(angle = 90, vjust=0.4), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12 ) ) +
  #geom_abline ( data=summary.fits.slope.bent %>% filter(SEASON == "summer" ) %>% filter(SLP_PVALUE < 0.05) , 
  #              aes ( intercept = INTERCEPT , slope = SLOPE, color="purple"),
  #              size=1.5, alpha=1/2 ) + #doesn't work but we may not want to plot slopes here as we only have 4 points for slope to fit
  #geom_abline ( data=summary.fits.slope %>% filter(SEASON == "summer" ) %>% filter(SLP_PVALUE < 0.05) , 
  #              aes ( intercept = INTERCEPT , slope = SLOPE, color="limegreen"),
  #              size=1.5, alpha=1/2 ) +
  #geom_hline ( data=temp %>% group_by(SEASON, LAKE, SIZESPECTYPE) %>% summarise( SLP = mean(SLOPE) ) , 
  #             aes ( yintercept = SLP , color=SIZESPECTYPE),
  #             size=1.5, alpha=1/2, lty=4) +
  guides ( fill = guide_legend(override.aes = list(linetype = 0) ),
           color = guide_legend(override.aes = list(linetype = 0) ) )

ggsave( "slope_comparisions_size spec type.pdf", path=.FIGURE_OUTPUT_LOC, 
        slope_output, width = 11, height = 4.5, units="cm", scale=2.5, dpi=400 )