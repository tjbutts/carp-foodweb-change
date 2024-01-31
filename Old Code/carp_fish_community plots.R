## Fish community x lake x year ## 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)


## Electrosishing CPUE ## 
electro = read_csv('Electrofishing_CPUE.csv')
electro

# df : lake, year, fish, CPUE 

electro_df = electro %>% 
  pivot_longer(cols = !c(Lake, Year), 
               names_to = 'taxa', 
               values_to = 'CPUE') %>% 
  filter(Year >2017) %>%
  filter(CPUE > 0)
electro_df

test = electro_df %>%
  filter(Lake == 'Blue')

# Polar CPUE # 
electro_df.wide = electro_df %>% 
  pivot_wider(names_from = 'taxa', 
              values_from = 'CPUE')
electro_df.wide[is.na(electro_df.wide)] <- 0
electro_df.wide

electro_df.long = electro_df.wide %>%
  pivot_longer(cols = !c(Lake, Year), 
               names_to = 'taxa', 
               values_to = 'CPUE') %>% 
  mutate(yval = match(taxa, colnames(electro_df.wide[,3:27])))
electro_df.long 

blue.fish = electro_df.long %>% 
  filter(Lake == 'Blue') %>% 
  filter(CPUE > 0)
blue.fish
unique(blue.fish$taxa)

blue.fish18 = electro_df.long %>%
  filter(taxa == 'Bluegill' | taxa == 'Channel Catfish' | taxa == 'Common Carp' |
           taxa == 'Largemouth Bass' | taxa == 'Black Crappie' | taxa == 'Green Sunfish' | 
           taxa == 'Black Bullhead' | taxa == 'Northern Pike') %>% 
  filter(Lake == 'Blue') %>% 
  filter(Year == 2018)
blue.fish18

# Carp Lakes data #=======================
library(ggplot2)


# Blue Lake # 
electro_df.wide = electro_df %>% 
  pivot_wider(names_from = 'taxa', 
              values_from = 'CPUE')
electro_df.wide[is.na(electro_df.wide)] <- 0
electro_df.wide

electro_df.long = electro_df.wide %>%
  pivot_longer(cols = !c(Lake, Year), 
               names_to = 'taxa', 
               values_to = 'CPUE') %>% 
  mutate(yval = match(taxa, colnames(electro_df.wide[,3:27])))
electro_df.long 

taxa = 'CPUE'
CPUE = 0
label.frame = data.frame(taxa, CPUE)
label.frame

blue.fish = electro_df.long %>% 
  filter(Lake == 'Blue') %>% 
  filter(CPUE > 1 | CPUE == 1)
blue.fish
unique(blue.fish$taxa)
max(blue.fish$CPUE)
min(blue.fish$CPUE)

blue.fish18 = electro_df.long %>%
  filter(taxa == 'Bluegill' | taxa == 'Channel Catfish' | taxa == 'Common Carp' |
           taxa == 'Largemouth Bass') %>% 
  filter(Lake == 'Blue') %>% 
  filter(Year == 2018)
dat.blue.18 = blue.fish18[,3:4]
dat.blue.18 = rbind(dat.blue.18, label.frame)
dat.blue.18$taxa = factor(dat.blue.18$taxa, levels = c('CPUE', 'Bluegill', 'Channel Catfish', 'Common Carp', 
                                                      'Largemouth Bass'))

# Plot # 
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

# Blue 2018 


library(ggplot2)

blue.18 = ggplot(data = dat.blue.18) +
  geom_col(aes(x = taxa, y = CPUE, fill = 'Black'),
           width = 1) + 
  geom_hline(yintercept = seq(0, 20, by = 5),
             color = "black", lty = 2, 
             size = .5) +
  geom_vline(xintercept = seq(.5, 8.5, by = 1),
             color = "black", 
             size = .5) +
  scale_fill_manual(values = ref_col_20) +
  theme_minimal() +
  labs(x = NULL, y = NULL) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_blank(),
         axis.text.y = element_blank(), panel.grid = element_blank(),
        legend.position = 'none', plot.margin = unit(c(-0.5,0.2,-0.5,1), 'lines')) +
  annotate('text', x = 1, y = c(5, 10, 15, 20), label = c('5', '10', '15', '20'), size = 4) + 
  coord_polar()
blue.18

# Blue 2019 
blue.fish19 = electro_df.long %>%
  filter(taxa == 'Bluegill' | taxa == 'Channel Catfish' | taxa == 'Common Carp' |
           taxa == 'Largemouth Bass') %>% 
  filter(Lake == 'Blue') %>% 
  filter(Year == 2019)
dat.blue.19 = blue.fish19[,3:4]
dat.blue.19 = rbind(dat.blue.19, label.frame)
dat.blue.19$taxa = factor(dat.blue.19$taxa, levels = c('CPUE', 'Bluegill', 'Channel Catfish', 'Common Carp', 
                                                       'Largemouth Bass'))

dat.blue.19

blue.19 = ggplot(data = dat.blue.19) +
  geom_col(aes(x = taxa, y = CPUE, fill = 'Black'),
           width = 1) + 
  geom_hline(yintercept = seq(0, 20, by = 5),
             color = "black", lty = 2, 
             size = .5) +
  geom_vline(xintercept = seq(.5, 8.5, by = 1),
             color = "black", 
             size = .5) +
  scale_fill_manual(values = ref_col_20) +
  theme_minimal() +
  labs(x = NULL, y = NULL) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),  
        axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank(),
        legend.position = 'none', plot.margin = unit(c(-0.5,0.2,-0.5,1), 'lines')) + 
  annotate('text', x = 1, y = c(5, 10, 15, 20), label = c('5', '10', '15', '20'), size = 4) + 
  coord_polar()
blue.19

# Blue 2020
blue.fish20 = electro_df.long %>%
  filter(taxa == 'Bluegill' | taxa == 'Channel Catfish' | taxa == 'Common Carp' |
           taxa == 'Largemouth Bass') %>% 
  filter(Lake == 'Blue') %>% 
  filter(Year == 2020)
dat.blue.20 = blue.fish20[,3:4]
dat.blue.20 = rbind(dat.blue.20, label.frame)
dat.blue.20$taxa = factor(dat.blue.20$taxa, levels = c('CPUE', 'Bluegill', 'Channel Catfish', 'Common Carp', 
                                                       'Largemouth Bass'))
dat.blue.20

blue.20 = ggplot(data = dat.blue.20) +
  geom_col(aes(x = taxa, y = CPUE, fill = 'Black'),
           width = 1) + 
  geom_hline(yintercept = seq(0, 20, by = 5),
             color = "black", lty = 2, 
             size = .5) +
  geom_vline(xintercept = seq(.5, 8.5, by = 1),
             color = "black", 
             size = .5) +
  scale_fill_manual(values = ref_col_20) +
  theme_minimal() +
  labs(x = NULL, y = NULL) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank(), 
        legend.position = 'none', plot.margin = unit(c(-0.5,0.2,-0.5,1), 'lines')) + 
  annotate('text', x = 1, y = c(5, 10, 15, 20), label = c('5', '10', '15', '20'), size = 4) + 
  coord_polar()
blue.20

library(ggpubr)
windows(height = 9, width = 4)
ggarrange(blue.18, blue.19, blue.20, nrow = 3)


## Take 2 ##=======================
# Libraries
library(tidyverse) # help you to prepare the data
library(ggplot2) # help you to prepare the plots

# prepare dataset
data = data.frame(
  # add a parameter with a range list 1-100
  index = seq(1,100),
  # create labelled parameter
  label = paste( data ="Data ",
                 seq(1,100),
                 sep="= "),
  # random values in the range 1 - 100
  values = sample( seq(10,100), 100, replace = T)
)

# top five values of the dataframe
head(data)

p <- ggplot(data, aes(x = as.factor(index), # x-axis factor label
                      
                      # y-axis numerical parameter
                      y = values)) +     
  
  # the bar height will represent
  # the actual value of the data
  geom_bar(stat = "identity",
           fill=alpha(ref_col_18, 0.5)) + # define bar color
  
  # define size of inner circle
  # and the size of the bar
  ylim(-100,120) + 
  labs(xlab = NULL, ylab = NULL) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  
  # define the polar coordinate
   coord_polar(start = 0)

# plot
p

# Adding labels to the plot

data_with_labels = data

# number of labels required
number_of_label <- nrow(data_with_labels)
# find the angle of rotation of the label
angle <-  90 - 360 * (data_with_labels$index - 0.5) /number_of_label    

# check the label alignment - right or left
data_with_labels$hjust<-ifelse( angle < -90, 1, 0)
# check the label angle
data_with_labels$angle<-ifelse(angle < -90,
                               angle + 180, angle)

data_with_labels

# Make the plot
# x-axis factor label
p <- ggplot(data, aes(x = as.factor(index),
                      
                      # y-axis numerical parameter
                      y = values)) +     
  
  # the bar height will represent
  # the actual value of the data
  geom_bar(stat = "identity",
           
           # define bar color
           fill=alpha("green", 0.5)) +
  
  # define size of inner circle
  # and the size of the bar
  ylim(-100,120) +
  
  # define the polar coordinate
  coord_polar(start = 0) +
  
  # add labels
  geom_text(data = data_with_labels,
            aes(x = index, y = values+10,
                
                # label alignment
                label = label, hjust=hjust),
            color = "black", fontface="bold",
            alpha = 0.6, size = 2.5,
            angle = data_with_labels$angle,
            inherit.aes = FALSE )

p

electro_df.wide = electro_df %>% 
  filter(CPUE == 1 | CPUE > 1) %>%
  pivot_wider(names_from = 'taxa', 
              values_from = 'CPUE')
electro_df.wide[is.na(electro_df.wide)] <- 0
electro_df.wide

electro_df.long = electro_df.wide %>%
  pivot_longer(cols = !c(Lake, Year), 
               names_to = 'taxa', 
               values_to = 'CPUE') %>% 
  mutate(yval = match(taxa, colnames(electro_df.wide[,3:19])))
electro_df.long 

# Get Labels in order # 
labeled.fish = electro_df.long 
labeled.fish
unique(labeled.fish$taxa)

# find the angle of rotation of the label (17 fish taxa)
angle <-  90 - 360 * (labeled.fish$yval- 0.5) /17   

# check the label alignment - right or left
labeled.fish$hjust<-ifelse( angle < -90, 1, 0)

# check the label angle
labeled.fish$angle<-ifelse(angle < -90,
                               angle + 180, angle)

# Make the plot
# x-axis factor label
p <- ggplot(labeled.fish, aes(x = as.factor(yval),
                      
                      # y-axis numerical parameter
                      y = CPUE)) +   
  facet_grid(Year~Lake) +
  
  # the bar height will represent
  # the actual value of the data
  geom_bar(stat = "identity",
           
           # define bar color
           fill=alpha("green", 0.5)) +
  
  # define size of inner circle
  # and the size of the bar
  ylim(-10,65) +
  
  # define the polar coordinate
  coord_polar(start = 0) +
  
  # add labels
  geom_text(data = labeled.fish,
            aes(x = yval, y = CPUE+10,
                
                # label alignment
                label = taxa, hjust=hjust),
            color = "black", fontface="bold",
            alpha = 0.6, size = 2.5,
            angle = labeled.fish$angle,
            inherit.aes = FALSE ) + 
  labs(xlab = NULL, ylab = NULL) +
  theme(axis.text.x = element_blank()) 
  
  

p

windows(height = 9, width = 15)
p

## Take 3 - Select for fish only within that lake (gridarrange approach) 

# Global dataset # 
electro_df.wide = electro_df %>% 
  filter(CPUE == 1 | CPUE > 1) %>%
  pivot_wider(names_from = 'taxa', 
              values_from = 'CPUE')
electro_df.wide[is.na(electro_df.wide)] <- 0
electro_df.wide

electro_df.long = electro_df.wide %>%
  pivot_longer(cols = !c(Lake, Year), 
               names_to = 'taxa', 
               values_to = 'CPUE') %>% 
  mutate(yval = match(taxa, colnames(electro_df.wide[,3:19])))
electro_df.long 

# Blue Lake # 
blue.taxa = electro_df.long %>% 
  filter(Lake == 'Blue') %>%
  filter(CPUE == 1 | CPUE > 1)
unique(blue.taxa$taxa)

blue.fish = electro_df.long %>%
  filter(Lake == 'Blue') %>% 
  filter(taxa == 'Bluegill' | taxa == 'Channel Catfish' | 
           taxa == 'Common Carp' | taxa == 'Largemouth Bass') %>% 
  group_by(Lake, Year) %>% 
  mutate(yval = 1:n()) %>% 
  ungroup()

blue.fish

# Get Labels in order # 
blue.n = max(blue.fish$yval)
labeled.fish = blue.fish
labeled.fish
unique(labeled.fish$taxa)

# find the angle of rotation of the label
angle <-  90 - 360 * (labeled.fish$yval- 0.5) /blue.n 

# check the label alignment - right or left
labeled.fish$hjust<-ifelse( angle < -90, 1, 0)

# check the label angle
labeled.fish$angle<-ifelse(angle < -90,
                           angle + 180, angle)

# Make the plot
# x-axis factor label
blue.p <- ggplot(labeled.fish, aes(x = as.factor(yval),
                              
                              # y-axis numerical parameter
                              y = CPUE)) +   
  facet_wrap(~Year, ncol = 1) +
  
  # the bar height will represent
  # the actual value of the data
  geom_bar(stat = "identity",
           
           # define bar color
           fill=alpha(ref_col_20, 0.5), 
           width = 0.5) +
  
  # define size of inner circle
  # and the size of the bar
  ylim(-10,65) +
  
  # define the polar coordinate
  coord_polar(start = 0) +
  
  # add labels
  geom_text(data = labeled.fish,
            aes(x = yval, y = CPUE+5,
                
                # label alignment
                label = taxa, hjust=hjust),
            color = "black", fontface="bold",
            alpha = 0.6, size = 3,
            angle = labeled.fish$angle,
            inherit.aes = FALSE ) + 
  labs(xlab = NULL, ylab = 'CPUE') + 
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        title = element_blank(), strip.text.x = element_blank()) 

# Storm #
storm.taxa = electro_df.long %>% 
  filter(Lake == 'Storm') %>%
  filter(CPUE == 1 | CPUE > 1)
unique(storm.taxa$taxa)

storm.fish = electro_df.long %>%
  filter(Lake == 'Storm') %>% 
  filter(taxa == 'Common Carp' | taxa == 'Walleye' | taxa == 'Yellow Bullhead' | 
           taxa == 'White Bass' | taxa == 'Gizzard Shad' | taxa == 'Bluegill' | 
           taxa == 'Yellow Perch' | taxa == 'Emerald Shiner' | taxa == 'Spottail Shiner') %>% 
  group_by(Lake, Year) %>% 
  mutate(yval = 1:n()) %>% 
  ungroup()

storm.fish

# Get Labels in order # 
storm.n = max(storm.fish$yval)
labeled.fish = storm.fish
labeled.fish
unique(labeled.fish$taxa)

# find the angle of rotation of the label
angle <-  90 - 360 * (labeled.fish$yval- 0.5) /storm.n 

# check the label alignment - right or left
labeled.fish$hjust<-ifelse( angle < -90, 1, 0)

# check the label angle
labeled.fish$angle<-ifelse(angle < -90,
                           angle + 180, angle)

# Make the plot
# x-axis factor label
storm.p <- ggplot(labeled.fish, aes(x = as.factor(yval),
                                   
                                   # y-axis numerical parameter
                                   y = CPUE)) +   
  facet_wrap(~Year, ncol = 1) +
  
  # the bar height will represent
  # the actual value of the data
  geom_bar(stat = "identity",
           
           # define bar color
           fill=alpha(ref_col_20, 0.5), 
           width = 0.5) +
  
  # define size of inner circle
  # and the size of the bar
  ylim(-10,65) +
  
  # define the polar coordinate
  coord_polar(start = 0) +
  
  # add labels
  geom_text(data = labeled.fish,
            aes(x = yval, y = CPUE+5,
                
                # label alignment
                label = taxa, hjust=hjust),
            color = "black", fontface="bold",
            alpha = 0.6, size = 3,
            angle = labeled.fish$angle,
            inherit.aes = FALSE ) + 
  labs(xlab = NULL, ylab = 'CPUE') + 
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        title = element_blank(), strip.text.x = element_blank()) 
storm.p

## Take 4 ##===========================

# Change in CPUE each year by fish and lake 
#create data
df <- data.frame(study=c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7'),
                 index=1:7,
                 effect=c(-.4, -.25, -.1, .1, .15, .2, .3),
                 lower=c(-.43, -.29, -.17, -.02, .04, .17, .27),
                 upper=c(-.37, -.21, -.03, .22, .24, .23, .33))

#view data
head(df)

#load ggplot2
library(ggplot2)

#create forest plot (make this for change in CPUE for fish 2020 - 2018)
ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(df), labels=df$study)

#or 
# Load package 
library(forestplot)
#> Loading required package: grid
#> Loading required package: magrittr
#> Loading required package: checkmate
# Generate dummy dataset
Genes <- c("Gene1","Gene2", "Gene3", "Gene4", "Gene5")
HR1 <- c(1.0, 1.2, 1.3, 1.4, 1.5)
HR2 <- c(1.2, 1.4, 1.5, 1.6, 1.7)
lower1 <- c(0.9, 1.0, 1.1, 1.2, 1.3)
lower2 <- c(1.0, 1.1, 1.2, 1.3, 1.4)
upper1 <- c(1.4, 1.6, 1.8, 2.0, 2.2)
upper2 <- c(1.7, 1.9, 2.1, 2.3, 2.5)
p.value1 <- c(2e-03, 2e-02, 3e-01, 5e-04, 1e-03)
p.value2 <- c(4e-01, 6e-04, 2e-03, 5e-01, 7e-04)
dummydata <- data.frame(Genes, HR1, HR2, lower1, lower2, upper1, upper2, p.value1, 
                        p.value2)

# Text on plot
tabletextdummy <- cbind(c("Genes",dummydata$Genes))

# Plot
forestplot(tabletextdummy, mean = cbind(c(NA, dummydata$HR1), c(NA,dummydata$HR2)),
           lower = cbind (c(NA,dummydata$lower1), c(NA,dummydata$lower2)), 
           upper = cbind(c(NA,dummydata$upper1), c(NA, dummydata$upper2)),
           new_page = TRUE,
           clip = c(0.1,5), 
           lineheight = unit(10,"mm"),
           line.margin = .1,
           xlog = TRUE, xlab = "HR with 95% CI", 
           col = fpColors(box = c("red4", "skyblue3"), 
                          lines = c("red3", "skyblue2")),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawDiamondCI),
           is.summary = c(TRUE,rep(FALSE,5)), 
           boxsize = 0.4, 
           xticks = c(0.75, 1, 1.5, 2, 3),
           legend = c("Group1", "Group2"), 
           vertices = TRUE)

# Forest Plot # =======================
# Global dataset # 
electro = read_csv('Electrofishing_CPUE.csv')
electro

# df : lake, year, fish, CPUE 
library(tidyverse)

electro_df = electro %>% 
  pivot_longer(cols = !c(Lake, Year), 
               names_to = 'taxa', 
               values_to = 'CPUE') %>% 
  filter(Year >2017) %>%
  filter(CPUE > 0)
electro_df


electro_df.wide = electro_df %>% 
  pivot_wider(names_from = 'taxa', 
              values_from = 'CPUE')
electro_df.wide[is.na(electro_df.wide)] <- 0
electro_df.wide

electro_df.long = electro_df.wide %>%
  pivot_longer(cols = !c(Lake, Year), 
               names_to = 'taxa', 
               values_to = 'CPUE') %>% 
  mutate(yval = match(taxa, colnames(electro_df.wide[,3:27])))
electro_df.long 

electro_change_final = electro_df.long %>% 
  pivot_wider(names_from = 'Year', 
              values_from = 'CPUE') %>% 
  mutate(change1 = `2019` - `2018`, 
         change2 = `2020` - `2019`) %>% 
  pivot_longer(cols = c(change1, change2), 
               names_to = 'Change', 
               values_to = 'CPUE') %>%
  select(Lake, taxa, Change, CPUE)
electro_change_final

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

# Ellen Figure # 
electro_change_reference = electro_change_final %>%
  arrange(taxa) %>%
  mutate(taxa = factor(taxa, levels=c('Bigmouth Buffalo', 'Common Carp', 'Black Bullhead', 'Black Crappie',
                                         'Bluegill','Brown Bullhead','Channel Catfish', 'Emerald Shiner', 
                                         'Freshwater Drum', 'Gizzard Shad', 'Green Sunfish', 'Hybrid Sunfish', 
                                         'Largemouth Bass', 'Northern Pike', 'Orangespotted Sunfish', 'Smallmouth Bass',
                                         'Spotfin Shiner', 'Spottail Shiner', 'Walleye', 'White Bass', 'White Crappie',
                                         'White Sucker', 'Yellow Bass', 'Yellow Bullhead', 'Yellow Perch'))) %>% 
  mutate(Lake = factor(Lake, levels = c('Blue', 'Storm', 'South Twin', 
                                        'Center', 'Five Island', 
                                        'North Twin', 'Silver')))
windows(height = 9, width =6)
ggplot(electro_change_final, aes(x=taxa,y=CPUE,color=Lake, shape = Change)) + 
  ###################################################
  geom_point(size=3, color = transparent) +
  scale_shape_manual(labels = c('Change after 2018', 'Change after 2019'), 
                       values = c(19, 18)) + 
  scale_color_manual(values = c(ref_col_20, ref_col_19, ref_col_18, 
                     rrn_col_20, rrn_col_18, 
                     nrr_col_20, nrr_col_18)) + 
  #geom_errorbar(aes(x=taxa, ymin=avg.change-sd, ymax=avg.change+sd),width=0,size=1,position=position_dodge(0.05))+
  # Max = 52, Min = -65 # 
  coord_flip()+ylim(-65,44)+
  #geom_rect(aes(ymin=0.497, ymax=1.54, xmin=-Inf, xmax=Inf), color=NA, fill="grey80")+
  #geom_hline(yintercept=1.0177,linetype="solid")+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(size=4)+
  #geom_errorbar(aes(x=lake_id, ymin=g-error, ymax=g+error),width=0,size=1,position=position_dodge(0.05))+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Taxa")+ylab("Change in CPUE")
  #annotate("text", x = 8, y = -1, hjust=1, fontface=3, label = "greater flux\nrates under oxic  \nconditions     ")+
  #annotate("text", x = 31, y = 4.2, hjust=0, fontface=3, label = "greater flux\n   rates under\n      anoxic conditions")




