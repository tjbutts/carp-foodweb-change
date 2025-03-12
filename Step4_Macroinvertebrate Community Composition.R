##Step 5 _ Macroinvertebrate Community - Stacked Bar graph ##

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

miv = get_data("edi.1926.1", filenum = 5) %>% 
  select(lake, year, taxa, density, miv_arealbiomass_mg.m2) %>% 
  filter(taxa != 'Chaoboridae')
miv

unique(miv$taxa)

miv.dat = miv %>% mutate(group = case_when(.$taxa %in% c('Chironomidae') ~ "Chironomidae",
                               .$taxa %in% c("Diptera",
                                              'Ephydridae',
                                              'Ceratopogonidae',
                                              'Simuliidae') ~ 'Misc.Diptera', # Lumping chaoborids here because these weren't night samples
                               .$taxa %in% c("Hirudinea",
                                              'Oligochaeta') ~ 'Worms',
                               .$taxa %in% c('Physidae', 
                                              'Planorbidae') ~ "Gastropoda",
                               .$taxa %in% c("Sphaeriidae",
                                              'Corbiculidae',
                                              'Unionidae', 
                                              'Bivalvia') ~ 'Bivalvia', 
                               .$taxa %in% c("Ephemeroptera", 
                                             'Ephemeridae') ~ 'Ephemeroptera',
                               .$taxa %in% c('Trichoptera',
                                              'Molannidae', 
                                              'Hydropsychidae') ~ 'Trichoptera',
                               .$taxa %in% c('Sialidae') ~ "Megaloptera", 
                               .$taxa %in% c('Dreissenidae') ~ 'Dreissenidae', 
                               .$taxa %in% c('Gammaridae') ~ 'Gammaridae')) %>%
  mutate(group = as.factor(group)) %>% 
  rename(biomass = miv_arealbiomass_mg.m2)
miv.dat


miv.dat$lake[miv.dat$lake == '19'] <- 'Center' 
miv.dat$lake[miv.dat$lake == '36'] <- 'Five.Island' 
miv.dat$lake[miv.dat$lake == '90'] <- 'North.Twin'
miv.dat$lake[miv.dat$lake == '105'] <- 'Silver'
miv.dat$lake[miv.dat$lake == '113'] <- 'Storm'
miv.dat$lake[miv.dat$lake == '406'] <- 'South.Twin'
miv.dat 

total.miv = miv.dat %>% 
  group_by(lake, year) %>% 
  summarize(sum = sum(biomass))
total.miv


# MIV Community - Annual # ==========================
south.twin18 = miv.dat %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2018') 
  
south.twin19 = miv.dat %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2019') 
  
south.twin20 = miv.dat %>% 
  filter(lake == 'South.Twin') %>% 
  filter(year == '2020') 
  

storm18 = miv.dat %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2018') 
  
storm19 = miv.dat %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2019') 
  
storm20 = miv.dat %>% 
  filter(lake == 'Storm') %>% 
  filter(year == '2020') 
  

north.twin18 = miv.dat %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2018') 
  
north.twin19 = miv.dat %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2019') 
  
north.twin20 = miv.dat %>% 
  filter(lake == 'North.Twin') %>% 
  filter(year == '2020') 
  

silver18 = miv.dat %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2018') 
  
silver19 = miv.dat %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2019') 
  
silver20 = miv.dat %>% 
  filter(lake == 'Silver') %>% 
  filter(year == '2020') 
  

center18 = miv.dat %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2018') 
  
center19 = miv.dat %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2019') 
  
center20 = miv.dat %>% 
  filter(lake == 'Center') %>% 
  filter(year == '2020') 
  

five.island18 = miv.dat %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2018') 
  
five.island19 = miv.dat %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2019') 
  
five.island20 = miv.dat %>% 
  filter(lake == 'Five.Island') %>% 
  filter(year == '2020') 
  

set.seed(55)

# create barplot data frames # 
# Empty taxa fill # 
group = c('Chironomidae',
          'Misc.Diptera',
          'Worms',
          'Gastropoda',
          'Bivalvia', 
          'Dreissenidae',
          'Ephemeroptera',
          'Trichoptera',
          'Megaloptera',
          'Gammaridae', 
          'Chironomidae',
          'Misc.Diptera',
          'Worms',
          'Gastropoda',
          'Bivalvia', 
          'Dreissenidae',
          'Ephemeroptera',
          'Trichoptera',
          'Megaloptera',
          'Gammaridae', 
          'Chironomidae',
          'Misc.Diptera',
          'Worms',
          'Gastropoda',
          'Bivalvia', 
          'Dreissenidae',
          'Ephemeroptera',
          'Trichoptera',
          'Megaloptera',
          'Gammaridae')
group = as.data.frame(group)

year = c(rep('2018', 10), rep('2019', 10), rep('2020', 10))
year = as.data.frame(year)

b.mean = c(rep(0, 30))
b.mean = as.data.frame(b.mean)

sd.mean = c(rep(NA, 30))
sd.mean = as.data.frame(sd.mean)

order = c(1,2,3,4,5,6,7,8,9,10,
          1,2,3,4,5,6,7,8,9,10,
          1,2,3,4,5,6,7,8,9,10)
order = as.data.frame(order)

fill.taxa = cbind(group, year, b.mean, sd.mean, order)
fill.taxa

# Storm # 
storm.18.sum = storm18 %>%
  group_by(group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'storm') %>%
  mutate(order = as.numeric(order)) %>%
  arrange(order)
storm.18.sum

storm.18.fill = fill.taxa %>%
  filter(year == '2018', 
         order == 6 | order == 7 | order == 8| order == 9)  
storm.18.fill$lake = 'storm'
storm.18.fill

storm.18 = rbind(storm.18.sum, storm.18.fill) %>% 
  rename(b.2018 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>% 
  arrange(order)
storm.18

storm19

storm19.sum = storm19 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'storm') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
storm19.sum

storm.19.fill = fill.taxa %>%
  filter(year == '2019', order == 6 | order == 7 | order == 10)  
storm.19.fill$lake = 'storm'
storm.19.fill

storm.19 = rbind(storm19.sum, storm.19.fill) %>% 
  rename(b.2019 = b.mean) %>% 
  arrange(order)
storm.19

storm20

storm20.sum = storm20 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'storm') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
storm20.sum

storm.20.fill = fill.taxa %>%
  filter(year == '2020', 
         order == 2 | order == 4 | order == 7)  
storm.20.fill$lake = 'storm'
storm.20.fill

storm.20 = rbind(storm20.sum, storm.20.fill) %>% 
  rename(b.2020 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
storm.20

storm.bar = cbind(storm.18, storm.19, storm.20) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020)
storm.bar


storm.perc = storm.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
storm.perc

# south.twin # 
south.twin.18.sum = south.twin18 %>%
  group_by(group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'south.twin') %>%
  mutate(order = as.numeric(order)) %>%
  arrange(order)
south.twin.18.sum

south.twin.18.fill = fill.taxa %>%
  filter(year == '2018', 
         order == 2 | order == 4 | order == 6| order == 7 |
           order == 8 | order == 9 | order == 10)  
south.twin.18.fill$lake = 'south.twin'
south.twin.18.fill

south.twin.18 = rbind(south.twin.18.sum, south.twin.18.fill) %>% 
  rename(b.2018 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>% 
  arrange(order)
south.twin.18

south.twin19

south.twin19.sum = south.twin19 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'south.twin') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
south.twin19.sum

south.twin.19.fill = fill.taxa %>%
  filter(year == '2019', order == 2 | order == 6 |
           order == 7 | order == 8 | order == 9 | order == 10)  
south.twin.19.fill$lake = 'south.twin'
south.twin.19.fill

south.twin.19 = rbind(south.twin19.sum, south.twin.19.fill) %>% 
  rename(b.2019 = b.mean) %>% 
  arrange(order)
south.twin.19

south.twin20

south.twin20.sum = south.twin20 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'south.twin') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
south.twin20.sum

south.twin.20.fill = fill.taxa %>%
  filter(year == '2020', 
         order == 2 | order == 4 | order == 5 | 
           order == 6 | order == 7 | order == 8 |
         order == 9 | order == 10)  
south.twin.20.fill$lake = 'south.twin'
south.twin.20.fill

south.twin.20 = rbind(south.twin20.sum, south.twin.20.fill) %>% 
  rename(b.2020 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
south.twin.20

south.twin.bar = cbind(south.twin.18, south.twin.19, south.twin.20) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020)
south.twin.bar


south.twin.perc = south.twin.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
south.twin.perc

# center # 
center.18.sum = center18 %>%
  group_by(group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'center') %>%
  mutate(order = as.numeric(order)) %>%
  arrange(order)
center.18.sum

center.18.fill = fill.taxa %>%
  filter(year == '2018', 
          order == 4 | order == 5| order == 6 | order == 7 |
           order == 8 | order == 9 | order == 10)  
center.18.fill$lake = 'center'
center.18.fill

center.18 = rbind(center.18.sum, center.18.fill) %>% 
  rename(b.2018 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>% 
  arrange(order)
center.18

center19

center19.sum = center19 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'center') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
center19.sum

center.19.fill = fill.taxa %>%
  filter(year == '2019', order == 4 | order == 5 | order == 6 |
           order == 7 | order == 8 | order == 9 | order == 10)  
center.19.fill$lake = 'center'
center.19.fill

center.19 = rbind(center19.sum, center.19.fill) %>% 
  rename(b.2019 = b.mean) %>% 
  arrange(order)
center.19

center20

center20.sum = center20 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'center') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
center20.sum

center.20.fill = fill.taxa %>%
  filter(year == '2020', 
          order == 4 | order == 5 | 
           order == 6 | order == 7 | order == 8 |
           order == 9 | order == 10)  
center.20.fill$lake = 'center'
center.20.fill

center.20 = rbind(center20.sum, center.20.fill) %>% 
  rename(b.2020 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
center.20

center.bar = cbind(center.18, center.19, center.20) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020)
center.bar


center.perc = center.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
center.perc

# five.island # 
five.island.18.sum = five.island18 %>%
  group_by(group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'five.island') %>%
  mutate(order = as.numeric(order)) %>%
  arrange(order)
five.island.18.sum

five.island.18.fill = fill.taxa %>%
  filter(year == '2018', 
         order == 4 | order == 5| order == 6 | order == 9 |
           order == 10)  
five.island.18.fill$lake = 'five.island'
five.island.18.fill

five.island.18 = rbind(five.island.18.sum, five.island.18.fill) %>% 
  rename(b.2018 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>% 
  arrange(order)
five.island.18

five.island19

five.island19.sum = five.island19 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'five.island') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
five.island19.sum

five.island.19.fill = fill.taxa %>%
  filter(year == '2019', order == 2 | order == 4 | order == 6 |
           order == 7 | order == 8 | order == 9 | order == 10)  
five.island.19.fill$lake = 'five.island'
five.island.19.fill

five.island.19 = rbind(five.island19.sum, five.island.19.fill) %>% 
  rename(b.2019 = b.mean) %>% 
  arrange(order)
five.island.19

five.island20

five.island20.sum = five.island20 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'five.island') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
five.island20.sum

five.island.20.fill = fill.taxa %>%
  filter(year == '2020', 
         order == 2 | order == 4 | 
           order == 6 | order == 7 | order == 10)  
five.island.20.fill$lake = 'five.island'
five.island.20.fill

five.island.20 = rbind(five.island20.sum, five.island.20.fill) %>% 
  rename(b.2020 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
five.island.20

five.island.bar = cbind(five.island.18, five.island.19, five.island.20) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020)
five.island.bar


five.island.perc = five.island.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
five.island.perc

# north.twin # 
north.twin.18.sum = north.twin18 %>%
  group_by(group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'north.twin') %>%
  mutate(order = as.numeric(order)) %>%
  arrange(order)
north.twin.18.sum

north.twin.18.fill = fill.taxa %>%
  filter(year == '2018', 
         order == 4 | order == 5| order == 6 | order == 9 |
           order == 10)  
north.twin.18.fill$lake = 'north.twin'
north.twin.18.fill

north.twin.18 = rbind(north.twin.18.sum, north.twin.18.fill) %>% 
  rename(b.2018 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>% 
  arrange(order)
north.twin.18

north.twin19

north.twin19.sum = north.twin19 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'north.twin') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
north.twin19.sum

north.twin.19.fill = fill.taxa %>%
  filter(year == '2019', order == 5 | order == 6 | order == 7 |
           order == 8 | order == 9 | order == 10)  
north.twin.19.fill$lake = 'north.twin'
north.twin.19.fill

north.twin.19 = rbind(north.twin19.sum, north.twin.19.fill) %>% 
  rename(b.2019 = b.mean) %>% 
  arrange(order)
north.twin.19

north.twin20

north.twin20.sum = north.twin20 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'north.twin') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
north.twin20.sum

north.twin.20.fill = fill.taxa %>%
  filter(year == '2020', 
         order == 2 | order == 4 | 
           order == 6 | order == 7 | order == 8 | order == 9 | order == 10)  
north.twin.20.fill$lake = 'north.twin'
north.twin.20.fill

north.twin.20 = rbind(north.twin20.sum, north.twin.20.fill) %>% 
  rename(b.2020 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
north.twin.20

north.twin.bar = cbind(north.twin.18, north.twin.19, north.twin.20) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020)
north.twin.bar


north.twin.perc = north.twin.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
north.twin.perc

# silver # 
silver.18.sum = silver18 %>%
  group_by(group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'silver') %>%
  mutate(order = as.numeric(order)) %>%
  arrange(order)
silver.18.sum

silver.18.fill = fill.taxa %>%
  filter(year == '2018', 
         order == 6 | 
           order == 10)  
silver.18.fill$lake = 'silver'
silver.18.fill

silver.18 = rbind(silver.18.sum, silver.18.fill) %>% 
  rename(b.2018 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>% 
  arrange(order)
silver.18

silver19

silver19.sum = silver19 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'silver') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
silver19.sum

silver.19.fill = fill.taxa %>%
  filter(year == '2019', order == 6 | order == 8 | order == 10)  
silver.19.fill$lake = 'silver'
silver.19.fill

silver.19 = rbind(silver19.sum, silver.19.fill) %>% 
  rename(b.2019 = b.mean) %>% 
  arrange(order)
silver.19

silver20

silver20.sum = silver20 %>%
  group_by(lake, group, year) %>% 
  summarize(sum_biom = sum(biomass)) %>% 
  ungroup() %>% 
  group_by(group, year) %>%
  summarize(b.mean = mean(sum_biom), 
            sd.mean = sd(sum_biom)) %>% 
  ungroup() %>% 
  mutate(order = case_when(.$group %in% c('Chironomidae') ~ "1",
                           .$group %in% c("Misc.Diptera") ~ '2',
                           .$group %in% c('Worms') ~ '3', 
                           .$group %in% c('Gastropoda') ~ '4', 
                           .$group %in% c('Bivalvia') ~ '5', 
                           .$group %in% c('Dreissenidae') ~ '6', 
                           .$group %in% c('Ephemeroptera') ~ '7', 
                           .$group %in% c('Trichoptera') ~ '8', 
                           .$group %in% c('Megaloptera') ~ '9', 
                           .$group %in% c('Gammaridae') ~ '10')) %>% 
  mutate(lake = 'silver') %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
silver20.sum

silver.20.fill = fill.taxa %>%
  filter(year == '2020', 
         order == 6 | order == 7 | 
           order == 8)  
silver.20.fill$lake = 'silver'
silver.20.fill

silver.20 = rbind(silver20.sum, silver.20.fill) %>% 
  rename(b.2020 = b.mean) %>% 
  mutate(order = as.numeric(order)) %>%
  arrange(order)
silver.20

silver.bar = cbind(silver.18, silver.19, silver.20) %>% 
  subset(select=which(!duplicated(names(.)))) %>%
  select(group, b.2018, b.2019, b.2020)
silver.bar


silver.perc = silver.bar %>%
  mutate(p.2018 = (b.2018/sum(b.2018)*100), 
         p.2019 = (b.2019/sum(b.2019)*100), 
         p.2020 = (b.2020/sum(b.2020)*100)) %>%
  select(group, p.2018, p.2019, p.2020)
silver.perc

# MIV Stacked percent #====================

windows(height = 6, width = 8) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)
library(paletteer)
library(unikn)
ramp = c('#009AD1FF', '#59B6DCFF', '#A0D3E6FF', '#C8E5EFFF', '#D4F1F4', '#E5E5E5FF', '#CCCCCCFF',
         '#999999FF', '#666666FF', '#000000FF')
ramp 


# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

xax = barplot(as.matrix(south.twin.perc[1:8,2:4]), col=ramp, col.axis = transparent, 
              names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 

barplot(as.matrix(storm.perc[1:10,2:4]), col=ramp,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
mtext(side =2, line=3, 'MIV Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Reference', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'Storm')

barplot(as.matrix(south.twin.perc[1:10,2:4]), col=ramp, col.axis = transparent, 
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n')
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T )
box()
mtext(side = 3, 'South Twin')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Dreissenidae', 'Ephemeroptera', 'Trichoptera', 'Megaloptera', 'Gammaridae'))
ramp3 = ramp[6:10]
legend(0,4.4, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp3), title = 'Macroinvertebrate Taxa')


# RRN # 
barplot(as.matrix(center.perc[1:10,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side =2, line=3, 'MIV Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1) 
mtext(side = 3, 'Center')

barplot(as.matrix(five.island.perc[1:10,2:4]), col=ramp, col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Chironomidae', 'Misc.Diptera', 'Worms', 'Gastropoda', 'Bivalvia'))
ramp2 = ramp[1:5]
legend(0.3,5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2))


# NRR
barplot(as.matrix(north.twin.perc[1:10,2:4]), col=ramp,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side =2, line=3, 'MIV Biomass (%)', cex=1)
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side = 3, 'North Twin')
mtext(side = 1, 'Year', line = 2)

barplot(as.matrix(silver.perc[1:10,2:4]), col=ramp, col.axis = transparent,
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = c('2018', '2019', '2020'), tick = T)
mtext(side = 1, line = 2, 'Year')
mtext(side = 3, 'Silver')

# MIV Stacked Biomass #====================

windows(height = 6, width = 8) 

library(paletteer)
library(unikn)
ramp = c('#009AD1FF', '#59B6DCFF', '#A0D3E6FF', '#C8E5EFFF', '#D4F1F4', '#E5E5E5FF', '#CCCCCCFF',
         '#999999FF', '#666666FF', '#000000FF')
ramp 

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

# xax = barplot(as.matrix(south.twin.bar[1:8,2:4]), col=ramp, col.axis = transparent, 
#               names.arg = c('2018', '2019', '2020'), border=T)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Reference # 

barplot(as.matrix(storm.bar[1:10,2:4]), col=ramp,ylim = c(0, 25000), col.axis = 'maroon',
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
mtext(side=2, line=3, cex = 1, expression('MIV Biomass'~"(mg"~'m'^-2*")"))
mtext(side =2, line=1.8, 'Reference', cex =1)
mtext(side = 3, 'Storm')
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()

barplot(as.matrix(south.twin.bar[1:10,2:4]), col=ramp, ylim = c(0,5000),
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n')
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
box()
mtext(side = 3, 'South Twin')

#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Dreissenidae', 'Ephemeroptera', 'Trichoptera', 'Megaloptera', 'Gammaridae'))
ramp3 = ramp[6:10]
legend(0,4.4, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp3), title = 'Macroinvertebrate Taxa')

# RRN # 
barplot(as.matrix(center.bar[1:10,2:4]), col=ramp, ylim = c(0,5000),
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side=2, line=3, cex = 1, expression('MIV Biomass'~"(mg"~'m'^-2*")"))
mtext(side =2, line=1.8, 'Removal 2018 - 2019', cex =1)
mtext(side = 3, 'Center')


barplot(as.matrix(five.island.bar[1:10,2:4]), col=ramp, ylim = c(0,5000),
        names.arg = c('2018', '2019', '2020'), border=T, xaxt = 'n');
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Five Island')


#empty plot 
plot(1, type = "n", xlab = "", col.axis = transparent, xaxt='n', yaxt = 'n',
     ylab = "", xlim = c(0, 5), 
     ylim = c(0, 5), col.axis = transparent, bty = 'n')
taxa2 = rev(c('Chironomidae', 'Misc.Diptera', 'Worms', 'Gastropoda', 'Bivalvia'))
ramp2 = ramp[1:5]
legend(0.3,5, legend = taxa2, pch=c(15), pt.cex=3, cex=1.5, bty='n',
       col = rev(ramp2))


# NRR
barplot(as.matrix(north.twin.bar[1:10,2:4]), col = ramp,ylim = c(0,5000), 
        names.arg = c('2018', '2019', '2020'), border=T);
box()
mtext(side=2, line=3, cex = 1, expression('MIV Biomass'~"(mg"~'m'^-2*")"))
mtext(side =2, line=1.8, 'Removal 2019 - 2020', cex =1)
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'North Twin')
mtext(side = 1, line =2, 'Year')

barplot(as.matrix(silver.bar[1:10,2:4]), col=ramp, 
        names.arg = c('2018', '2019', '2020'), border=T);
box()
axis(side =1, at = xax[c(1,2,3)], labels = F, tick = T)
mtext(side = 3, 'Silver')
mtext(side = 1, line =2, 'Year')




