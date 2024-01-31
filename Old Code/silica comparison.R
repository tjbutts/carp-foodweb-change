# Mendota - Monona - Trout Silica Comparison # 
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(here)
remotes::install_github("bmcafee/EDIutilsAddons")
library(EDIutilsAddons)

# Silica Data #============================
# Package ID: knb-lter-ntl.1.60 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
silica = get_data('knb-lter-ntl.1.60') %>% 
	select(lakeid, year4, daynum, sampledate, depth, rep, sta, event, 
				 drsif, drsif_sloh, flagdrsif, flagdrsif_sloh) %>%
	# drsif = dissolved reactive silica - filtered (ug/L)
	# drsif_sloh = dissolved reactive silice - state lab (mg/L)
	# flagdrsif = data flag for drsif 
	# flagdrsif_sloh = data flag for drsif_sloh 
	mutate(drsif_mgL = drsif/1000, 
				 drsif_sloh_mgL = drsif_sloh) %>% 
	select(lakeid, year4, daynum, sampledate, depth,
				 drsif_mgL, flagdrsif, 
				 drsif_sloh_mgL, flagdrsif_sloh) %>% 
	mutate(drsif_comb = ifelse(is.na(drsif_sloh_mgL), drsif_mgL, drsif_sloh_mgL)) %>% 
	filter(depth == 0)
silica

silica_TR = silica %>% 
	filter(lakeid == 'TR')
silica_TR_1 = silica_TR %>%
	filter(year4 > 2000 & year4 < 2008)
silica_TR_2 = silica_TR %>%
	filter(year4 > 2007 & year4 < 2015)
silica_TR_3 = silica_TR %>%
	filter(year4 > 2014)


silica_ME = silica %>% 
	filter(lakeid == 'ME')
silica_MO = silica %>%
	filter(lakeid == 'MO')

# plot dynamics # 
plot(drsif_comb ~ year4, data = silica_TR_1, col = 'black', xlim = c(2000, 2022), 
		 bg = '#045a8d', pch = 21, ylim = c(0,10),  xaxp = c(2000, 2022, 22))
points(drsif_comb ~ year4, data = silica_TR_2, col = 'black', bg = '#41b6c4', pch = 21, ylim = c(0,10))
points(drsif_comb ~ year4, data = silica_TR_3, pch = 21, col = 'black', bg = '#78c679', ylim = c(0,10))
#abline(h = 0)
legend('topleft', legend = c('2001-2007', '2008-2014', '2015-2022'), pch = c(21,21,21), pt.bg = c('#045a8d', '#41b6c4', '#78c679'))

# Zoom into 2008 - 2014 
plot(x = NULL, y = NULL, type = 'n', xlab = 'Day of Year',
		 ylab = 'Silica Concentration', xlim = c(0,365), ylim = c(0,10))
library(viridis)
line_colors = c('#8c510a','#d8b365','#E3BC9A','black','#c7eae5','#5ab4ac','#01665e')
years = unique(silica_TR_2$year4)
# loop through each year and add a line to the plot # 
for (year4 in years) {
	subset_data <- silica_TR_2[silica_TR_2$year4 == year4, ]
	points(subset_data$daynum, subset_data$drsif_comb, pch = 19, 
				 col = line_colors[year4 == years], lwd = 2, type = 'o')
}
legend('topright', legend = as.character(years), ncol = 4,
			 col = line_colors, lty = 1, lwd = 2, title = 'Year')
text(300, 1, 'Trout Lake', font = 2)

# Now take a look at lake Mendota # 
silica_ME_1 = silica_ME %>%
	filter(year4 < 2009)
silica_ME_2 = silica_ME %>%
	filter(year4 > 2008 & year4 < 2016)
silica_ME_3 = silica_ME %>%
	filter(year4 > 2015)

plot(drsif_comb ~ year4, data = silica_ME_1, col = 'black', xlim = c(1995, 2022), 
		 bg = '#045a8d', pch = 21, ylim = c(0,6),  xaxp = c(1995, 2022, 27))
points(drsif_comb ~ year4, data = silica_ME_2, col = 'black', bg = '#41b6c4', pch = 21, ylim = c(0,10))
points(drsif_comb ~ year4, data = silica_ME_3, pch = 21, col = 'black', bg = '#78c679', ylim = c(0,10))
#abline(h = 0)
legend('topleft', legend = c('2001-2007', '2008-2014', '2015-2022'), pch = c(21,21,21), pt.bg = c('#045a8d', '#41b6c4', '#78c679'))

# Zoom into 2004 - 2013
silica_ME_zoom = silica_ME %>%
	filter(year4 > 2003 & year4 < 2014)

plot(x = NULL, y = NULL, type = 'n', xlab = 'Day of Year',
		 ylab = 'Silica Concentration', xlim = c(0,365), ylim = c(0,10))
library(viridis)
line_colors = c('#8c510a','#bf812d','#dfc27d','#E3BC9A','gray30','#c7eae5','#80cdc1','#35978f','#01665e','black')
years = unique(silica_ME_zoom$year4)
# loop through each year and add a line to the plot # 
for (year4 in years) {
	subset_data <- silica_ME_zoom[silica_ME_zoom$year4 == year4, ]
	points(subset_data$daynum, subset_data$drsif_comb, pch = 19, 
				 col = line_colors[year4 == years], lwd = 2, type = 'o')
}
legend('topright', legend = as.character(years), ncol = 4,
			 col = line_colors, lty = 1, lwd = 2, title = 'Year')
text(300, 6, 'Lake Mendota', font = 2)

