## Benthic Size Spectra ## 
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

## Final MIV Biomass-Density data 
miv.output = miv.comb

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
log2bins.list = log2bins_butts(miv.output$avg_drymass_g)
miv.output$BINMID <- as.factor(log2bins.list$indiv$binMid)
miv.output$BINMIN <- log2bins.list$indiv$binMin
miv.output$BINMAX <- log2bins.list$indiv$binMax
miv.output

### Plot Raw Data with Bins added ###===================
# Reference - no removal # Blue, South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

miv.output$lake = factor(miv.output$lake, levels = c('12','19', '36', '90', '105', '113', '406'))
miv.output = miv.output %>% arrange(lake)

# Replace lake number with lake name #
levels(miv.output$lake) = c(levels(miv.output$lake), 'Blue','Center', 
                             'Five.Island', 'North.Twin', 
                             'Silver', 'Storm', 'South.Twin')

miv.output.plot = miv.output
miv.output.plot$lake[miv.output.plot$lake == '12'] <- 'Blue'
miv.output.plot$lake[miv.output.plot$lake == '19'] <- 'Center' 
miv.output.plot$lake[miv.output.plot$lake == '36'] <- 'Five.Island' 
miv.output.plot$lake[miv.output.plot$lake == '90'] <- 'North.Twin'
miv.output.plot$lake[miv.output.plot$lake == '105'] <- 'Silver'
miv.output.plot$lake[miv.output.plot$lake == '113'] <- 'Storm'
miv.output.plot$lake[miv.output.plot$lake == '406'] <- 'South.Twin'
miv.output.plot$lake
miv.output.plot$BINMID = as.numeric(as.character(miv.output.plot$BINMID))

# Plot MIV data alone #===========================
windows(width = 8, height = 8)

colfunc = c('#01665e', '#80cdc1', '#c7eae5' ,'#f5f5f5', '#dfc27d', '#bf812d', '#8c510a')

miv = as.data.frame(miv.output.plot)
miv

plot(log2(miv$density)~jitter(log2(miv$BINMID), 1), data = miv, xaxt = 'n', pch = 21, col = 'black',  yaxt = 'n', xlim = c(log2(6.985e-10), log2(1)),
     bg = 'forestgreen', cex = 1.5, lwd = 2, ylim = c(log2(1), log2(4000000)), 
     xlab = '', ylab = '')
axis(side = 1, 
     at = c(log2(6.985e-10), log2(8.94e-08), log2(2.29e-5), log2(1)), 
     labels = T, 
     las = 1)

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

# Run code from Size Spectra Analysis - Diagnostics to get to this point 
points(log2(rot.y[rot.y$taxon == 'Brachionus', 'density.areal'])~jitter(log2(rot.x[rot.x$taxon == 'Brachionus', 'BINMID']), 1), yaxt = 'n', 
     xaxt = 'n', pch = 21, col = 'black', bg = ref_col_18, cex = 1.5, lwd = 2,
     xlab = '', ylab = '')


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

output.plot2
# mean = output.plot2 %>% 
#   group_by(BINMID) %>%
#   summarize(midpoint = mean(density.areal)) %>%
#   ungroup()
# mean
# 
# points(log2(midpoint)~log2(BINMID), data = mean, yaxt = 'n', xaxt = 'n', pch = 23, 
#        col = 'black', bg = 'white', lwd = 2, cex = 2.5)
# 
# miv.mean = miv %>% 
#   group_by(BINMID) %>% 
#   summarize(midpoint = mean(density)) %>% 
#   ungroup
# miv.mean
# 
# points(log2(midpoint)~log2(BINMID), data = miv.mean, yaxt = 'n', xaxt = 'n', pch = 23, 
#         col = 'black', bg = 'white', lwd = 2, cex = 1.5)

#output.plot2 = as.data.frame(output.plot2)
#points(log2(output.plot2[output.plot2$group == 'Cladocera', 'density.areal']) ~ 
#  jitter(log2(output.plot2[output.plot2$group == 'Cladocera', 'BINMID']), 1), 
#  yaxt = 'n', xaxt = 'n', pch = 21, col = colfunc[1], cex = 1.5, lwd = 2,
#  xlab = '', ylab = '')

# legend('bottomright', legend = c('K.cochlearis', 'Polyarthra', 'Brachionus', 'Filinia', 'Asplanchna', 'Pompholyx', 'K.Quadrata', 'Ascomorpha', 'Rare Rotifers' ,'Avg. Density'), 
#        pch = c(rep(19, 9), 23), col = c(colfunc[1], colfunc[2], ref_col_18, colfunc[4], colfunc[5], 'steelblue', colfunc[6], colfunc[7], ref_col_20 , 'gray40'))
# mtext(side=1, line =2, 'Log2(Individual Biomass)')
# mtext(side=2, line =2, 'Log2(Areal Density)')
# mtext(side=3, line=1, 'Rotifers Only', cex = 2, font =2)

clads = as.data.frame(output.plot2)

points(log2(clads[clads$group == 'Bosmina', 'density.areal'])~
       jitter(log2(clads[clads$group == 'Bosmina', 'BINMID']), 1), data = clads, yaxt = 'n', 
     xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
     xlab = '', ylab = '')
points(log2(clads[clads$group == 'Chydorid', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Chydorid', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Daphnia', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Daphnia', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Ceriodaphnia', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Ceriodaphnia', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Lg.Cladocera', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Lg.Cladocera', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Calanoid', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Calanoid', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Cyclopoid', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Cyclopoid', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
points(log2(clads[clads$group == 'Nauplii', 'density.areal'])~
         jitter(log2(clads[clads$group == 'Nauplii', 'BINMID']), 1), 
       data = output.plot2, yaxt = 'n',  xaxt = 'n', pch = 21, col = 'black', bg = 'gray40', cex = 1.5, lwd = 2,
       xlab = '', ylab = '')
lightblack = rgb(0, 0, 0, max = 255, alpha = 100) 

legend('bottomleft', legend = c('K.cochlearis', 'Polyarthra', 'Brachionus', 'Filinia', 'Asplanchna', 'Pompholyx', 'K.Quadrata', 'Ascomorpha', 'Rare Rotifers' ,'Cladocerans - Copepods', 'MIVs'), 
       pch = 19, col = c(colfunc[1], colfunc[2], ref_col_18, colfunc[4], colfunc[5], 'steelblue', colfunc[6], colfunc[7], ref_col_20 , 'gray40', 'forestgreen'))
mtext(side=1, line =2, 'Log2(Individual Biomass)')
mtext(side=2, line =2, 'Log2(Areal Density)')
mtext(side=3, line=1, 'All Data Combined', cex = 2, font =2)
