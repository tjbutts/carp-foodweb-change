## Number of individuals x Dry Weight Biomass ## 

# Run Size Spectra Analysis - Combined Taxa; First # 
zoop.dat = left_join(zp.biom, zp.dens, by = c('sampleID', 
                                              'year', 'season',
                                              'doy','taxon')) %>%
  select(sampleID, lake.y, year, season, doy, taxon, group, biomass_g, count, density.areal) %>%
  rename(lake = lake.y) 
zoop.dat

levels(zoop.dat$lake) = c(levels(zoop.dat$lake), 'Blue', 'Storm', 'South.Twin',
                        'Center', 
                        'Five.Island', 'North.Twin', 
                        'Silver')

zoop.dat$lake[zoop.dat$lake == '12'] <- 'Blue'
zoop.dat$lake[zoop.dat$lake == '19'] <- 'Center' 
zoop.dat$lake[zoop.dat$lake == '36'] <- 'Five.Island' 
zoop.dat$lake[zoop.dat$lake == '90'] <- 'North.Twin'
zoop.dat$lake[zoop.dat$lake == '105'] <- 'Silver'
zoop.dat$lake[zoop.dat$lake == '113'] <- 'Storm'
zoop.dat$lake[zoop.dat$lake == '406'] <- 'South.Twin'
zoop.dat

zoop.dat$lake = factor(zoop.dat$lake, levels = c('Blue', 'Storm', 'South.Twin', 
                                                 'Center', 'Five.Island', 'North.Twin',
                                                 'Silver'))
levels(zoop.dat$lake)

zoop.dat$group = factor(zoop.dat$group, levels = c('Lg.Cladocera', 'Daphnia','Ceriodaphnia' ,'Bosmina', 'Chydorid',
                                                  'Calanoid', 'Cyclopoid', 'Nauplii', 'Rotifer'))
levels(zoop.dat$group)
zoop.dat

# Prepare the data 
library(ggplot2)

spring = zoop.dat %>% 
  filter(season == 'spring')
spring[rep(1:nrow(spring), spring$count), ] # biomass reflected by the number of actual individuals measured 

spring.mu = spring %>% 
  group_by(lake, year) %>% 
  summarize(
    grp.mean = mean(log2(biomass_g))
  ) %>% 
  ungroup()
spring.mu  

# Basic histogram - Spring # 
windows(width = 9, height = 4)
ggplot(spring, aes(x = log2(biomass_g), color = group, fill = group)) + geom_histogram(bins = 3, alpha=0.4, position = 'dodge') +
  theme_bw() + 
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        strip.text.x = element_text(size = 10, face = 'bold.italic' ),
        strip.text.y = element_text(size = 10, face = 'bold.italic' )) + 
  facet_grid(year~lake) + 
  scale_fill_brewer(palette = 'Greens') +
  scale_color_manual(values = c('black', 'black','black', 'black',
                                       'black', 'black',
                                       'black', 'black',
                                       'black')) + 
  labs( x = expression ( paste ( 'Log'[2], 'Zooplankton Dry Weight Biomass (g)') ), 
        y = expression ( paste ( 'Frequency' ) ) )  + 
  geom_vline(data = spring.mu, aes(xintercept=grp.mean), linetype = 'dashed')

summer = zoop.dat %>% 
  filter(season == 'summer')
summer[rep(1:nrow(summer), summer$count), ] # biomass reflected by the number of actual individuals measured 

summer.mu = summer %>% 
  group_by(lake, year) %>% 
  summarize(
    grp.mean = mean(log2(biomass_g))
  ) %>% 
  ungroup()
summer.mu  

# Basic histogram - Summer # 
windows(width = 9, height = 4)
ggplot(summer, aes(x = log2(biomass_g), color = group, fill = group)) + geom_histogram(bins = 3, alpha=0.4, position = 'dodge') +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), 
    strip.text.x = element_text(size = 10, face = 'bold.italic' ),
    strip.text.y = element_text(size = 10, face = 'bold.italic' )) + 
  facet_grid(year~lake) + 
  scale_fill_brewer(palette = 'Greens') +
  scale_color_manual(values = c('black', 'black','black', 'black',
                                       'black', 'black',
                                       'black', 'black',
                                       'black')) + 
                                         labs( x = expression ( paste ( 'Log'[2], 'Zooplankton Dry Weight Biomass (g)') ), 
                                               y = expression ( paste ( 'Frequency' ) ) ) + 
  geom_vline(data = summer.mu, aes(xintercept=grp.mean), linetype = 'dashed')

# Density Plot # 
windows(width = 9, height = 4)
ggplot(zoop.dat, aes(x = log2(biomass_g), fill = season)) + geom_density(alpha=0.5, lwd = 1, col = 'black') +
  theme_bw() + 
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        strip.text.x = element_text(size = 10, face = 'bold.italic' ),
        strip.text.y = element_text(size = 10, face = 'bold.italic' )) + 
  facet_grid(year~lake) + 
  scale_fill_manual(values = c('forestgreen', 'steelblue')) + 
  labs( x = expression ( paste ( 'Log'[2], 'Zooplankton Dry Weight Biomass (g)') ), 
        y = expression ( paste ( 'Density' ) ) ) 
  
## Macroinvertebrates ##========================
miv.dat = miv.join2 %>% 
  select(lake, year, season, doy, taxon, group, biomass_g, density.areal, type)
miv.dat

levels(miv.dat$lake) = c(levels(miv.dat$lake), 'Blue', 'Storm', 'South.Twin',
                          'Center', 
                          'Five.Island', 'North.Twin', 
                          'Silver')

miv.dat$lake[miv.dat$lake == '12'] <- 'Blue'
miv.dat$lake[miv.dat$lake == '19'] <- 'Center' 
miv.dat$lake[miv.dat$lake == '36'] <- 'Five.Island' 
miv.dat$lake[miv.dat$lake == '90'] <- 'North.Twin'
miv.dat$lake[miv.dat$lake == '105'] <- 'Silver'
miv.dat$lake[miv.dat$lake == '113'] <- 'Storm'
miv.dat$lake[miv.dat$lake == '406'] <- 'South.Twin'
miv.dat

miv.dat$lake = factor(miv.dat$lake, levels = c('Blue', 'Storm', 'South.Twin', 
                                                 'Center', 'Five.Island', 'North.Twin',
                                                 'Silver'))
levels(miv.dat$lake)

miv.dat.mu = miv.dat %>% 
  group_by(lake, year) %>% 
  summarize(
    grp.mean = mean(log2(biomass_g))
  ) %>% 
  ungroup()
miv.dat.mu  

windows(width = 9, height = 4)
ggplot(miv.dat, aes(x = log2(biomass_g), color = group, fill = group)) + geom_histogram(bins = 3, alpha=0.4, position = 'dodge') +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), 
    strip.text.x = element_text(size = 10, face = 'bold.italic' ),
    strip.text.y = element_text(size = 10, face = 'bold.italic' )) + 
  facet_grid(year~lake) + 
  scale_fill_brewer(palette = 'RdBu') +
  scale_color_manual(values = c('black', 'black','black', 'black',
                                       'black', 'black',
                                       'black', 'black',
                                       'black', 'black', 'black')) + 
                                         labs( x = expression ( paste ( 'Log'[2], '(Macroinvertebrate Dry Weight Biomass (g))') ), 
                                               y = expression ( paste ( 'Frequency' ) ) )  + 
  geom_vline(data = miv.dat.mu, aes(xintercept=grp.mean), linetype = 'dashed')

# Density Plot # 
windows(width = 9, height = 4)
ggplot(miv.dat, aes(x = log2(biomass_g), fill = lake)) + geom_density(alpha=0.5, lwd = 1, col = 'black') +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), 
    strip.text.x = element_text(size = 10, face = 'bold.italic' ),
    strip.text.y = element_text(size = 10, face = 'bold.italic' )) + 
  facet_grid(year~lake) + 
  scale_fill_manual(values = c(ref_col_18, ref_col_18, ref_col_18, rrn_col_18, rrn_col_18, 
                               nrr_col_18, nrr_col_18)) + 
  labs( x = expression ( paste ( 'Log'[2], '(Macroinvertebrate Dry Weight Biomass (g))') ), 
        y = expression ( paste ( 'Density' ) ) ) 
