## Step4_Analyze Zooplankton Community Composition - Spring/summer combined ##====================

# Zooplankton Data # =========================
# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# Load in data # 
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

# Find sampling frequency # 
d = full_zoops %>% 
  group_by(lake, doy, year) %>%
  summarize(sum = sum(biomass)) %>%
  ungroup()
d

d2 = d %>% 
  group_by(lake, year) %>% 
  summarize(n = n())
d2

levels(full_zoops$group) = c(levels(full_zoops$group), 
                             'Daphnia', 'Lg.Cladocera','Bosmina', 'Chydorid',  
                             'Cyclopoid', 'Calanoid', 'Nauplii', 
                             'Rotifer')

dap = '#40004b'
clad = '#9970ab'
bos = '#c2A5CF'
chy = '#ffdcdc'
cyc = '#194b4f'
cal = '#2d676f'
nau = 'aquamarine4'
rot = '#6D6C70'
                
ramp = c(dap, clad, bos, chy, cyc, cal, nau, rot)
        



# Combined Spring Summer Percent Barplot #=========================

south.twin18 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2018')
  
south.twin19 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2019')
  
south.twin20 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2020')
  

storm18 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2018')
  
storm19 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2019')
  
storm20 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2020')
  

north.twin18 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2018')
  
north.twin19 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2019')
  
north.twin20 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2020')
  

silver18 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2018')
  
silver19 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2019')
  
silver20 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2020')
  

center18 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2018')
  
center19 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2019')
  
center20 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2020')
  

five.island18 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2018')
  
five.island19 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2019')
  
five.island20 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2020')
  

set.seed(55)

# create barplot data frames # 

# Storm # 
storm18

storm18.sum = storm18 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2018 = mean(sum_biom), 
            sd.2018 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
storm18.sum

storm19

storm19.sum = storm19 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2019 = mean(sum_biom), 
            sd.2019 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
storm19.sum

storm20

storm20.sum = storm20 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2020 = mean(sum_biom), 
            sd.2020 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
storm20.sum

storm.bar = cbind(storm18.sum, storm19.sum, storm20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
storm.bar

storm.perc = storm.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
storm.perc



# South Twin # 
south.twin18

south.twin18.sum = south.twin18 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2018 = mean(sum_biom), 
            sd.2018 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
south.twin18.sum

south.twin19

south.twin19.sum = south.twin19 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2019 = mean(sum_biom), 
            sd.2019 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
south.twin19.sum

south.twin20

south.twin20.sum = south.twin20 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2020 = mean(sum_biom), 
            sd.2020 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
south.twin20.sum

south.twin.bar = cbind(south.twin18.sum, south.twin19.sum, south.twin20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
south.twin.bar

south.twin.perc = south.twin.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
south.twin.perc


# Center # 
center18

center18.sum = center18 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2018 = mean(sum_biom), 
            sd.2018 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
center18.sum

center19

center19.sum = center19 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2019 = mean(sum_biom), 
            sd.2019 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
center19.sum

center20

center20.sum = center20 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2020 = mean(sum_biom), 
            sd.2020 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
center20.sum

center.bar = cbind(center18.sum, center19.sum, center20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
center.bar

center.perc = center.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
center.perc



# Five Island # 
five.island18

five.island18.sum = five.island18 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2018 = mean(sum_biom), 
            sd.2018 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
five.island18.sum

five.island19

five.island19.sum = five.island19 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2019 = mean(sum_biom), 
            sd.2019 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
five.island19.sum

five.island20

five.island20.sum = five.island20 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2020 = mean(sum_biom), 
            sd.2020 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
five.island20.sum

five.island.bar = cbind(five.island18.sum, five.island19.sum, five.island20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
five.island.bar

five.island.perc = five.island.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
five.island.perc




# North Twin # 
north.twin18

north.twin18.sum = north.twin18 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2018 = mean(sum_biom), 
            sd.2018 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
north.twin18.sum

north.twin19

north.twin19.sum = north.twin19 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2019 = mean(sum_biom), 
            sd.2019 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
north.twin19.sum

north.twin20

north.twin20.sum = north.twin20 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2020 = mean(sum_biom), 
            sd.2020 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
north.twin20.sum

north.twin.bar = cbind(north.twin18.sum, north.twin19.sum, north.twin20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
north.twin.bar

north.twin.perc = north.twin.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
north.twin.perc

# Silver # 
silver18

silver18.sum = silver18 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2018 = mean(sum_biom), 
            sd.2018 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
silver18.sum

silver19

silver19.sum = silver19 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2019 = mean(sum_biom), 
            sd.2019 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
silver19.sum

silver20

silver20.sum = silver20 %>%
  group_by(lake, doy, year, group) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group) %>%
  summarize(b.2020 = mean(sum_biom), 
            sd.2020 = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Daphnia') ~ "1",
                           .$group %in% c("Lg.Cladocera") ~ '2',
                           .$group %in% c('Bosmina') ~ '3', 
                           .$group %in% c('Chydorid') ~ '4', 
                           .$group %in% c('Cyclopoid') ~ '5', 
                           .$group %in% c('Calanoid') ~ '6', 
                           .$group %in% c('Nauplii') ~ '7', 
                           .$group %in% c('Rotifer') ~ '8')) %>% 
  arrange(order)
silver20.sum

silver.bar = cbind(silver18.sum, silver19.sum, silver20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
silver.bar

silver.perc = silver.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
silver.perc

## Carp Stacked Barplots ##
windows(height = 6, width = 8) 
# 


# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 

barplot(as.matrix(storm.perc[1:8,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'Storm')
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp, col.axis = transparent, 
        names.arg = c('2018', '2019', '2020'), border=T)
box()
mtext(side = 3, 'South Twin')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# RRN # 
barplot(as.matrix(center.perc[1:8,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1)
mtext(side = 3, 'Center')

barplot(as.matrix(five.island.perc[1:8,2:4]), col=ramp, col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))


# NRR
barplot(as.matrix(north.twin.perc[1:8,2:4]), col=ramp,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =3, 'North Twin')
mtext(side = 1, line=2, 'Year')

barplot(as.matrix(silver.perc[1:8,2:4]), col=ramp, col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')
mtext(side = 1, line=2, 'Year')

# Combined Spring Summer Biomass Barplot #=========================
## Carp Stacked Barplots ##
windows(height = 6, width = 8) 

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 
ramp= c(dap, clad, bos, chy, cyc, cal, nau, rot)

barplot(as.matrix(storm.bar[1:8,2:4]), col=ramp, ylim = c(0, 1500),
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side=2, line=3, cex = 1, expression('ZP Biomass'~"("*mu*g~L^-1*")"))
mtext(side =2, line=1.8, 'Reference', cex =1)
box()
mtext(side = 3, 'Storm')

barplot(as.matrix(south.twin.bar[1:8,2:4]), col=ramp, col.axis = transparent, ylim = c(0,1500),
        names.arg = c('2018', '2019', '2020'), border=T)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'South Twin')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# RRN # 
barplot(as.matrix(center.bar[1:8,2:4]), col=ramp, ylim = c(0, 1500),
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side=2, line=3, cex = 1, expression('ZP Biomass'~"("*mu*g~L^-1*")"))
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1)
mtext(side = 3, 'Center')

barplot(as.matrix(five.island.bar[1:8,2:4]), col=ramp, col.axis = transparent,
       ylim = c(0, 1500), names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))

# NRR
barplot(as.matrix(north.twin.bar[1:8,2:4]), col=ramp, ylim = c(0,1500),
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side=2, line=3, cex = 1, expression('ZP Biomass'~"("*mu*g~L^-1*")"))
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
mtext(side =3, 'North Twin')
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side = 1, line =2, 'Year')

barplot(as.matrix(silver.bar[1:8,2:4]), col=ramp, col.axis = transparent, ylim = c(0,1500),
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')
mtext(side =1, line =2, 'Year')



