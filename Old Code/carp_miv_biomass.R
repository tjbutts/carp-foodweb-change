## MIV Community - Stacked Bar graph ## 

# Generate MIV Data ## =================================
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Carp Lakes/carp-foodweb-change")

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

## Calculate MIV Biomass ## ==============================
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

t1 = t1 %>% mutate(log10_drymass_mg = (-1.12 + 2.9*(log10(SL)))) %>%
  mutate(drymass_mg = 10^log10_drymass_mg) %>% 
  mutate(drymass_g = drymass_mg/1000) %>%
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

## Uncomment below section to replace NAs by sample site within lake $$ 
# # average weight per taxa per lake per year
# miv.avg.ply = miv.biomass %>%
#   mutate(lake = substr(sampleid, 4,6)) %>%
#   group_by(lake, taxa) %>%
#   summarize(avg_drymass_g = mean(avg_drymass_g, na.rm = T),
#             ln2_drymass_g = mean(ln2_drymass_g, na.rm = T)) %>%
#   ungroup()
# miv.avg.ply
# 
# # Replace NAs with either the lake's average value Or the global value of that taxa across the study lakes #
# miv.join.NA[miv.join.NA$taxa == 'Sphaeriidae' & miv.join.NA$year == '2018' & miv.join.NA$lake == '12', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Sphaeriidae' & miv.avg.ply$lake == '012', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Diptera' & miv.join.NA$year == '2019' & miv.join.NA$lake == '19', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Diptera' & miv.avg.ply$lake == '019', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Trichoptera' & miv.join.NA$year == '2018' & miv.join.NA$lake == '36', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Trichoptera' & miv.avg.ply$lake == '036', 'avg_drymass_g']
# global.trichoptera = miv.avg.ply[miv.avg.ply$taxa == 'Trichoptera', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Trichoptera' & miv.join.NA$year == '2018' & miv.join.NA$lake == '105', 'avg_drymass_g'] <- mean(global.trichoptera$avg_drymass_g)
# miv.join.NA[miv.join.NA$taxa == 'Trichoptera' & miv.join.NA$year == '2019' & miv.join.NA$lake == '105', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Trichoptera' & miv.avg.ply$lake == '105', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Ephydridae' & miv.join.NA$year == '2018' & miv.join.NA$lake == '113', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Ephyridae' & miv.avg.ply$lake == '113', 'avg_drymass_g'] # Correcting a spelling error
# global.phys = miv.avg.ply[miv.avg.ply$taxa == 'Physidae' , 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Physidae' & miv.join.NA$year == '2018' & miv.join.NA$lake == '113', 'avg_drymass_g'] <- mean(global.phys$avg_drymass_g)
# miv.join.NA[miv.join.NA$taxa == 'Sphaeriidae' & miv.join.NA$year == '2018' & miv.join.NA$lake == '113', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Sphaeriidae' & miv.avg.ply$lake == '113', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Physidae' & miv.join.NA$year == '2019' & miv.join.NA$lake == '113', 'avg_drymass_g'] <- mean(global.phys$avg_drymass_g)
# global.planorb = miv.avg.ply[miv.avg.ply$taxa == 'Planorbidae' , 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Planorbidae' & miv.join.NA$year == '2019' & miv.join.NA$lake == '113', 'avg_drymass_g'] <- mean(global.planorb$avg_drymass_g)
# miv.join.NA[miv.join.NA$taxa == 'Dreissenidae' & miv.join.NA$year == '2020' & miv.join.NA$lake == '113', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Dreissendiae' & miv.avg.ply$lake == '113', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Hirudinea' & miv.join.NA$year == '2020' & miv.join.NA$lake == '113', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Hirudinea' & miv.avg.ply$lake == '113', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Sphaeriidae' & miv.join.NA$year == '2019' & miv.join.NA$lake == '406', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Sphaeriidae' & miv.avg.ply$lake == '406', 'avg_drymass_g']
# miv.join.NA[miv.join.NA$taxa == 'Planorbidae' & miv.join.NA$year == '2019' & miv.join.NA$lake == '105', 'avg_drymass_g'] <-
#   miv.avg.ply[miv.avg.ply$taxa == 'Planorbidae' & miv.avg.ply$lake == '105', 'avg_drymass_g']
# 
# miv.join.NA

# average weight per taxa per lake per year 
miv.avg.ply = miv.biomass %>% 
  mutate(lake = substr(sampleid, 4,6)) %>%
  group_by(lake, taxa) %>% 
  summarize(avg_drymass_g = mean(avg_drymass_g, na.rm = T), 
            ln2_drymass_g = mean(ln2_drymass_g, na.rm = T)) %>% 
  ungroup()
miv.avg.ply

# Average across sample to get the lake value # 
# That way the density and weight of a taxa is the average value of the whole lake made up by the spatial samples within # 
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
miv.comb

# Make .csv # 
miv.biomass = miv.comb %>% 
  mutate(avg_drymass_mg = avg_drymass_g*1000) %>% 
  mutate(miv_arealbiomass_mg.m2 = avg_drymass_mg*density)
miv.biomass  

write_csv(miv.biomass, 'MIV_biomass.csv')
