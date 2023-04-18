## Zooplankton Community - Stacked Bar graph ##

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
        

# Present by lake and year # 

# ========= PLOTTING COLORS ===== # 
# Color gradation # 
black18 = rgb(0,0,0, max = 255, alpha = 100)
black19 = rgb(0,0,0, max = 255, alpha = 180)
black20 = rgb(0,0,0, max = 255, alpha = 255)

# Reference - no removal # Blue, South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

# null, removal, removal # North Twin, Silver
nrr_col_18 = rgb(43, 73, 112, max = 255, alpha = 100) 
nrr_col_19 = rgb(43, 73, 112, max = 255, alpha = 180)
nrr_col_20 = rgb(43, 73, 112, max = 255, alpha = 255)

# removal, removal, null # Center, Five Island 
rrn_col_18 = rgb(37, 111, 92, max = 255, alpha = 100) 
rrn_col_19 = rgb(37, 111, 92, max = 255, alpha = 180)
rrn_col_20 = rgb(37, 111, 92, max = 255, alpha = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

# Summer Zooplankton Community - Annual Percent# ==========================
blue18 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2018') %>% 
  filter(season == 'summer')
blue19 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
blue20 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

south.twin18 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
south.twin19 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
south.twin20 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

storm18 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
storm19 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
storm20 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

north.twin18 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
north.twin19 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
north.twin20 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

silver18 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
silver19 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
silver20 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

center18 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
center19 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
center20 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

five.island18 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
five.island19 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
five.island20 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

set.seed(55)

# create barplot data frames # 
# Blue # 
blue18

blue18.sum = blue18 %>%
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
blue18.sum 

blue19

blue19.sum = blue19 %>%
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
blue19.sum


blue20

blue20.sum = blue20 %>%
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
blue20.sum

blue.bar = cbind(blue18.sum, blue19.sum, blue20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
blue.bar

blue.perc = blue.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
blue.perc



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

## Carp Stacked Barplots ## ==========================
windows(height = 6, width = 8) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

xax = barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp, col.axis = transparent, 
              names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

ramp= c(dap, clad, bos, chy, cyc, cal, nau, rot)
barplot(as.matrix(blue.perc[1:8,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border= T, xaxt = 'n')
box()
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Blue')

barplot(as.matrix(storm.perc[1:8,2:4]), col=ramp,  col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'Storm')

barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp,  col.axis = transparent, 
        names.arg = c('2018', '2019', '2020'), border=T)
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
box()
mtext(side = 3, 'South Twin')


# RRN # 
barplot(as.matrix(center.perc[1:8,2:4]), col=ramp,  
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1)
mtext(side = 3, 'Center')

barplot(as.matrix(five.island.perc[1:8,2:4]), col=ramp,  col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# NRR
barplot(as.matrix(north.twin.perc[1:8,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
mtext(side = 3, 'North Twin')

barplot(as.matrix(silver.perc[1:8,2:4]), col=ramp,  col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')
mtext(side = 1, line = 2, 'Year (Summer Community)')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))

# Spring Zooplankton Community - Annual Percent##=============================
blue18 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2018') %>% 
  filter(season == 'spring')
blue19 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
blue20 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

south.twin18 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
south.twin19 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
south.twin20 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

storm18 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
storm19 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
storm20 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

north.twin18 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
north.twin19 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
north.twin20 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

silver18 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
silver19 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
silver20 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

center18 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
center19 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
center20 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

five.island18 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
five.island19 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
five.island20 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

set.seed(55)

# create barplot data frames # 
# Blue # 
blue18

blue18.sum = blue18 %>%
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
blue18.sum

group = 'Lg.Cladocera'

fill18 = as.data.frame(group)
fill18 = fill18 %>% 
  mutate(b.2018 = 0,
         sd.2018 = NA, 
         order = 2
  )
blue18.sum = rbind(blue18.sum, fill18) %>% 
  arrange(order)
blue18.sum

blue19

blue19.sum = blue19 %>%
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
blue19.sum

group = c('Lg.Cladocera', 'Chydorid', 'Calanoid')

fill19 = as.data.frame(group)
fill19 = fill19 %>% 
  mutate(b.2019 = c(0,0,0),
         sd.2019 = c(NA,NA,NA),
         order = c(2,4,6))
fill19

blue19.sum = rbind(blue19.sum, fill19) %>% 
  arrange(order)
blue19.sum

blue.bar = cbind(blue18.sum, blue19.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, sd.2018, sd.2019)
blue.bar

blue.perc = blue.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100)) %>%
  select(group, p.2018, p.2019)
blue.perc



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

## Carp Stacked Barplots ## ==========================
windows(height = 6, width = 8) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

xax = barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp, col.axis = transparent, 
              names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

ramp= c(dap, clad, bos, chy, cyc, cal, nau, rot)

blue.perc
p.2020 = c(0,0,0,0,0,0,0,0)
blue.perc = cbind(blue.perc, p.2020)
blue.perc
barplot(as.matrix(blue.perc[1:8,2:4]), col = ramp,
        names.arg = c('2018', '2019', '2020'), border = T, xaxt = 'n')
box()
axis(side = 1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
mtext(side = 3, 'Blue')
text(3, 50, 'NA', font = 2, cex = 1.5)

barplot(as.matrix(storm.perc[1:8,2:4]), col=ramp,  col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'Storm')

barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp,  col.axis = transparent, 
        names.arg = c('2018', '2019', '2020'), border=T)
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
box()
mtext(side = 3, 'South Twin')


# RRN # 
barplot(as.matrix(center.perc[1:8,2:4]), col=ramp,  
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1)
mtext(side = 3, 'Center')

barplot(as.matrix(five.island.perc[1:8,2:4]), col=ramp,  col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# NRR
barplot(as.matrix(north.twin.perc[1:8,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'ZP Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
mtext(side = 3, 'North Twin')

barplot(as.matrix(silver.perc[1:8,2:4]), col=ramp,  col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')
mtext(side = 1, line = 2, 'Year (Spring Community)')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))


# Summer Zooplankton Community - Annual Biomass # ==========================
blue18 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2018') %>% 
  filter(season == 'summer')
blue19 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
blue20 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

south.twin18 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
south.twin19 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
south.twin20 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

storm18 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
storm19 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
storm20 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

north.twin18 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
north.twin19 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
north.twin20 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

silver18 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
silver19 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
silver20 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

center18 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
center19 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
center20 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

five.island18 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2018')%>% 
  filter(season == 'summer')
five.island19 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2019')%>% 
  filter(season == 'summer')
five.island20 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2020')%>% 
  filter(season == 'summer')

set.seed(55)

# create barplot data frames # 
# Blue # 
blue18

blue18.sum = blue18 %>%
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
blue18.sum 

blue19

blue19.sum = blue19 %>%
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
blue19.sum


blue20

blue20.sum = blue20 %>%
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
blue20.sum

blue.bar = cbind(blue18.sum, blue19.sum, blue20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
blue.bar

blue.perc = blue.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
blue.perc



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

## Carp Stacked Barplots - raw biomass ## ==========================
windows(height = 6, width = 8) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

xax = barplot(as.matrix(south.twin.bar[1:8,2:4]), col=ramp, ylim = c(0,2000), col.axis = transparent, 
              names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 
ramp= c(dap, clad, bos, chy, cyc, cal, nau, rot)
barplot(as.matrix(blue.bar[1:8,2:4]), col=ramp, ylim = c(0,2000),
        names.arg = c('2018', '2019', '2020'), border= T, xaxt = 'n')
box()
mtext(side =2, line=3, 'Zooplankton Biomass', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Blue')

barplot(as.matrix(storm.bar[1:8,2:4]), col=ramp, ylim = c(0,2000), col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'Storm')

barplot(as.matrix(south.twin.bar[1:8,2:4]), col=ramp, ylim = c(0,2000), col.axis = transparent, 
        names.arg = c('2018', '2019', '2020'), border=T)
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
box()
mtext(side = 3, 'South Twin')


# RRN # 
barplot(as.matrix(center.bar[1:8,2:4]), col=ramp, ylim = c(0,2000), 
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'Zooplankton Biomass', cex=1)
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1)
mtext(side = 3, 'Center')

barplot(as.matrix(five.island.bar[1:8,2:4]), col=ramp, ylim = c(0,2000), col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# NRR
barplot(as.matrix(north.twin.bar[1:8,2:4]), col=ramp, ylim = c(0,2000),
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'Zooplankton Biomass', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
mtext(side = 3, 'North Twin')

barplot(as.matrix(silver.bar[1:8,2:4]), col=ramp, ylim = c(0,2000), col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')
mtext(side = 1, line = 2, 'Year (Summer Community)')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))


# Spring Zooplankton Community - Annual Biomass ##=============================
blue18 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2018') %>% 
  filter(season == 'spring')
blue19 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
blue20 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

south.twin18 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
south.twin19 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
south.twin20 = full_zoops %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

storm18 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
storm19 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
storm20 = full_zoops %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

north.twin18 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
north.twin19 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
north.twin20 = full_zoops %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

silver18 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
silver19 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
silver20 = full_zoops %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

center18 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
center19 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
center20 = full_zoops %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

five.island18 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2018')%>% 
  filter(season == 'spring')
five.island19 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2019')%>% 
  filter(season == 'spring')
five.island20 = full_zoops %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2020')%>% 
  filter(season == 'spring')

set.seed(55)

# create barplot data frames # 
# Blue # 
blue18

blue18.sum = blue18 %>%
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
blue18.sum

group = 'Lg.Cladocera'

fill18 = as.data.frame(group)
fill18 = fill18 %>% 
  mutate(b.2018 = 0,
         sd.2018 = NA, 
         order = 2
  )
blue18.sum = rbind(blue18.sum, fill18) %>% 
  arrange(order)
blue18.sum

blue19

blue19.sum = blue19 %>%
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
blue19.sum

group = c('Lg.Cladocera', 'Chydorid', 'Calanoid')

fill19 = as.data.frame(group)
fill19 = fill19 %>% 
  mutate(b.2019 = c(0,0,0),
         sd.2019 = c(NA,NA,NA),
         order = c(2,4,6))
fill19

blue19.sum = rbind(blue19.sum, fill19) %>% 
  arrange(order)
blue19.sum

blue.bar = cbind(blue18.sum, blue19.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, sd.2018, sd.2019)
blue.bar




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



## Carp Stacked Barplots ## ==========================
windows(height = 6, width = 8) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

xax = barplot(as.matrix(south.twin.bar[1:8,2:4]), col=ramp, col.axis = transparent, 
              names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 
# Earliest data in 2020 was DOY 195 which is classified as summer
#empty plot 
blue.bar
p.2020 = c(0,0,0,0,0,0,0,0)
blue.bar = cbind(blue.bar, p.2020)
blue.bar
barplot(as.matrix(blue.bar[1:8,2:4]), col = ramp, ylim=c(0,2000),
        names.arg = c('2018', '2019', '2020'), border = T, xaxt = 'n')
box()
axis(side = 1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'Zooplankton Biomass', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
mtext(side = 3, 'Blue')
text(3, 1000, 'NA', font = 2, cex = 1.5)

barplot(as.matrix(storm.bar[1:8,2:4]), col=ramp, ylim=c(0,2000), col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)

box()
mtext(side = 3, 'Storm')

barplot(as.matrix(south.twin.bar[1:8,2:4]), col=ramp, ylim=c(0,2000), col.axis = transparent, 
        names.arg = c('2018', '2019', '2020'), border=T)
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
box()
mtext(side = 3, 'South Twin')

# RRN # 
barplot(as.matrix(center.bar[1:8,2:4]), col=ramp, ylim=c(0,2000), 
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'Zooplankton Biomass', cex=1)
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1)
mtext(side = 3, 'Center')

barplot(as.matrix(five.island.bar[1:8,2:4]), col=ramp, ylim=c(0,2000), col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# NRR
barplot(as.matrix(north.twin.bar[1:8,2:4]), col=ramp, ylim=c(0,2000),
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'Zooplankton Biomass', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
mtext(side =3, 'North Twin')

barplot(as.matrix(silver.bar[1:8,2:4]), col=ramp, ylim=c(0,2000), col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')
mtext(side = 1, line = 2, 'Year (Spring Community)')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5,  legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))

# Combined Spring Summer Percent Barplot #=========================

blue18 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2018')  
  
blue19 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2019')
  
blue20 = full_zoops %>% 
  filter(lake == 'Blue') %>% 
  filter(year == '2020')
  

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
# Blue # 
blue18

blue18.sum = blue18 %>%
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
blue18.sum 

blue19

blue19.sum = blue19 %>%
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
blue19.sum


blue20

blue20.sum = blue20 %>%
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
blue20.sum

blue.bar = cbind(blue18.sum, blue19.sum, blue20.sum) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020, sd.2018, sd.2019, sd.2020)
blue.bar

blue.perc = blue.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
blue.perc



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

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

xax = barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp, col.axis = transparent, 
              names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 
ramp= c(dap, clad, bos, chy, cyc, cal, nau, rot)
barplot(as.matrix(blue.perc[1:8,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border= T, xaxt = 'n')
box()
mtext(side =2, line=3, 'Percent ZP Biomass', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Blue')

barplot(as.matrix(storm.perc[1:8,2:4]), col=ramp, col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'Storm')
barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp, col.axis = transparent, 
        names.arg = c('2018', '2019', '2020'), border=T)
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
box()
mtext(side = 3, 'South Twin')

# RRN # 
barplot(as.matrix(center.perc[1:8,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'Percent ZP Biomass', cex=1)
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
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# NRR
barplot(as.matrix(north.twin.perc[1:8,2:4]), col=ramp,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'Percent ZP Biomass', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
mtext(side =3, 'North Twin')

barplot(as.matrix(silver.perc[1:8,2:4]), col=ramp, col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))

# Combined Spring Summer Biomass Barplot #=========================
## Carp Stacked Barplots ##
windows(height = 6, width = 8) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

xax = barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp, col.axis = transparent, 
              names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 
ramp= c(dap, clad, bos, chy, cyc, cal, nau, rot)
barplot(as.matrix(blue.bar[1:8,2:4]), col=ramp, ylim = c(0, 1500),
        names.arg = c('2018', '2019', '2020'), border= T, xaxt = 'n')
box()
mtext(side =2, line=3, 'Percent ZP Biomass', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Blue')

barplot(as.matrix(storm.bar[1:8,2:4]), col=ramp, col.axis = transparent, ylim = c(0, 1500),
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'Storm')

barplot(as.matrix(south.twin.bar[1:8,2:4]), col=ramp, col.axis = transparent, ylim = c(0,1500),
        names.arg = c('2018', '2019', '2020'), border=T)
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
box()
mtext(side = 3, 'South Twin')

# RRN # 
barplot(as.matrix(center.bar[1:8,2:4]), col=ramp, ylim = c(0, 1500),
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'Percent ZP Biomass', cex=1)
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
taxa2 = rev(c('Cyclopoid', 'Calanoid','Nauplii', 'Rotifera'))
ramp2 = c(cyc, cal, nau, rot)
legend("bottomleft", legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2), title = 'Zooplankton Taxa')

# NRR
barplot(as.matrix(north.twin.bar[1:8,2:4]), col=ramp, ylim = c(0,1500),
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'Percent ZP Biomass', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
mtext(side =3, 'North Twin')

barplot(as.matrix(silver.bar[1:8,2:4]), col=ramp, col.axis = transparent, ylim = c(0,1500),
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side =3, 'Silver')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c(expression(italic('Daphnia')), 'Misc. Cladocerans',
              expression(italic('Bosmina')), 'Chydorids'))
ramp1= c(dap, clad, bos, chy)
legend(0.1,5.5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp1))

