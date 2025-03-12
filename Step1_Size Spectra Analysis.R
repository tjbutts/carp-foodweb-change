## Step 1_Size Spectra Analysis ##==============================

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
remotes::install_github("bmcafee/EDIutilsAddons") # Install EDIutilsAddons package for easy EDI data download
library(EDIutilsAddons)

# Data for scripts is available through the environmental data initiative, package id = edi.1926.1 (as of March 11, 2025)
  # Updates to the metadata or data in the package will be maintained by the author (Tyler Butts, tyler.james.butts@gmail.com)
  # DOI for the data archived in EDI: https://doi.org/10.6073/pasta/e6307fcbf343d2f948ad552101862704 

# Load in SSA dataset #===========================
output = get_data("edi.1926.1", filenum = 2) # Size spectra data 
output

# If want to rerun with only summer data excluding spring (as shown in supplemental) 
# Uncomment the following line to filter out spring data  
  # output = output %>% filter(season == "summer")

# Replace lake number with lake name #
levels(output$lake) = c(levels(output$lake), 'Center', 
                         'Five Island', 'North Twin', 
                         'Silver', 'Storm', 'South Twin')

output$lake[output$lake == '19'] <- 'Center' 
output$lake[output$lake == '36'] <- 'Five Island' 
output$lake[output$lake == '90'] <- 'North Twin'
output$lake[output$lake == '105'] <- 'Silver'
output$lake[output$lake == '113'] <- 'Storm'
output$lake[output$lake == '406'] <- 'South Twin'
output


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
log2bins.list = log2bins_butts(output$biomass_g)
output$BINMID <- as.factor(log2bins.list$indiv$binMid)
output$BINMIN <- log2bins.list$indiv$binMin
output$BINMAX <- log2bins.list$indiv$binMax
output

# Calculate average density per bin # 
output.combined = output %>% 
  group_by(year, lake, season, BINMID, BINMIN, BINMAX) %>%
  summarize(avg.density.areal = mean(density.areal, na.rm = T)) %>%
  ungroup()

output.combined

output.combined$BINMID = as.numeric(as.character(output.combined$BINMID))
#BINMIN and BINMAX are included here but will not do anything, 
## but by including they will be carried through to the summary table 
## and will be usuable for relative density

output.combined = as_tibble(output.combined) # Save in a tidy format
output.combined

# log 2 binning # 

output.binned = output.combined %>% # Remove season from grouping 
  group_by(year, lake, BINMID, BINMIN, BINMAX) %>%
  mutate(BINMID_LOG = log2(BINMID), BINMIN_LOG = log2(BINMIN), BINMAX_LOG = log2(BINMAX), DENS_LOG = log2(avg.density.areal)) %>%
  ungroup()
output.binned


# Fits data seasons combined # =======================
output.combined = output.binned %>% 
  select(year, lake, BINMID, BINMIN, BINMAX, avg.density.areal, BINMID_LOG, BINMIN_LOG, BINMAX_LOG, DENS_LOG)
output.combined

# Get Min and Max of weight bins # For Table 2 # 
minmax_bins = output.combined %>%
  group_by(lake, year) %>% 
  summarize(
    minimum = min(BINMIN_LOG), 
    maximum = max(BINMAX_LOG)) %>%
  ungroup()
minmax_bins


## Linear fits of pelagic size spectrum by lake by year ## 
fits.yr.lm = data.frame ( matrix ( NA, ncol = 15, nrow = length( unique (output.combined$lake)) * (length( unique (output$year) )) ) ) 
colnames ( fits.yr.lm ) = c('year', 'lake', 'outlierpres', 'n', 'fitmin', 'fitmax', 'slope', 'slp_se', 'slp_l95ci' ,'slp_u95ci', 'intcpt', 'int_se', 'int_l95ci', 'int_u95CI', 'height')
fits.yr.lm$year = rep( sort( unique( output.combined$year)), each = length(unique(output.combined$lake)))
fits.yr.lm$lake = rep( as.factor ( unique (output.combined$lake) ), each = 1)
fits.yr.lm

fits.yr.lm$outlierpres = factor ( fits.yr.lm$outlierpres, levels = c('Y', 'N'))
fits.yr.lm

## Set up a short dataframe for easier export ## 
short.output = setNames ( data.frame ( matrix (NA, ncol = 6, nrow = 0) ), c( 'year', 'lake',  'DENS_LOG', 'BINMID_LOG', 'DATAUSEDINFIT') )

short.output$lake = factor ( short.output$lake, levels = c('Storm', 'South Twin', 'Center', 'Five Island', 'North Twin','Silver'))

# Remove lake years with no data 
#fits.yr.lm = fits.yr.lm %>% filter(!row_number() %in% c(2,16,30)) 
fits.yr.lm

## Loop for filling with breaks ## 
for ( i in 1:nrow(fits.yr.lm) ) { 
    tempdata = output.combined %>% 
      filter ( year == fits.yr.lm$year[i] ) %>%
      filter ( lake == fits.yr.lm$lake[i] ) 
    
    templin = lm (DENS_LOG ~ BINMID_LOG, data = tempdata) 
    
    cookcutoff = 10/length(templin$residuals) # Set Cook's value, the general rule is 4/n for detection of influence, 1 is a rough cutoff for points with too much leverage
    tempcooks = ifelse ( cooks.distance(templin) > cookcutoff, 'dropped', 'used') 
    tempjoin = data.frame ( 'year' = tempdata$year, 'lake' = tempdata$lake, 
                            'DENS_LOG' = tempdata$DENS_LOG, 'BINMID_LOG' = tempdata$BINMID_LOG, 'DATAUSEDINFIT' = tempcooks)
    short.output = rbind ( short.output, tempjoin)
    
    if (any ( cooks.distance(templin) > cookcutoff ) ) { # remove points with high leverage or just save the lm as it is 
      fits.yr.lm$outlierpres[i] = 'Y' 
      templin = lm ( DENS_LOG [-which ( cooks.distance(templin) > cookcutoff)] ~ BINMID_LOG[-which (cooks.distance(templin) > cookcutoff)], data = tempdata)
    } else {fits.yr.lm$outlierpres[i] = 'N' } # Report that no high leverage points were found 
  
  fits.yr.lm$n[i] = length(templin$residuals) 
  fits.yr.lm$fitmin[i] = min(templin$model$BINMID_LOG, na.rm = T)
  fits.yr.lm$fitmax[i] = max(templin$model$BINMID_LOG, na.rm = T) 
  fits.yr.lm$slope[i] = coef(templin)[[2]] 
  fits.yr.lm$slp_se[i] = coef(summary(templin))[2,2] 
  fits.yr.lm$slp_l95ci[i] = confint(templin, level = 0.95)[2,1]
  fits.yr.lm$slp_u95ci[i] = confint(templin, level=0.95)[2,2]
  fits.yr.lm$intcpt[i] = coef(templin)[[1]]
  fits.yr.lm$int_se[i] = coef(summary(templin))[1,2] 
  fits.yr.lm$int_l95ci[i] = confint(templin, level=0.95)[1,1]
  fits.yr.lm$int_u95CI[i] = confint(templin, level = 0.95)[1,2]
  
  temp.short <- tempjoin %>%
    filter ( DATAUSEDINFIT == 'used' | DATAUSEDINFIT == 'dropped') 
  fits.yr.lm$height.bin[i] = ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2
  fits.yr.lm$height[i] = fits.yr.lm$slope[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lm$intcpt[i]
  fits.yr.lm$height.l95ci[i] = fits.yr.lm$slp_l95ci[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lm$int_l95ci[i] 
  fits.yr.lm$height.u95ci[i] = fits.yr.lm$slp_u95ci[i] * ( min(temp.short$BINMID_LOG) + max(temp.short$BINMID_LOG) ) / 2 + fits.yr.lm$int_u95CI[i]
}

fits.yr.lm # Represents combined spring and summer data 

height.bin.summary = fits.yr.lm %>% 
  select(year, lake, height.bin) %>% 
  summarize(mean = mean(height.bin), 
            sd = sd(height.bin))
height.bin.summary

short.output

### Fits of height from overall ### 
fits.yr.lm$height.overall = fits.yr.lm$slope * ( floor(min(fits.yr.lm$fitmin)) + ceiling(max(fits.yr.lm$fitmax)) ) / 2 + fits.yr.lm$intcpt


## Size Spectra Data ## 
short.output$lake = factor(short.output$lake, levels = c('Storm', 'South Twin', 'Center', 'Five Island', 'North Twin','Silver'))
plot.data = short.output 
plot.data
color_custom <- c("limegreen", "royalblue2", "gray50")
color_custom2 <- c( "purple", "royalblue2")

output_for_plot = left_join(short.output, fits.yr.lm, by = c('year', 'lake')) # Join makes it easier for ggplot to take the data, of course it creates many rep values 
output_for_plot


#### Size spectrum analysis with different spectra plotted ####
library(ggplot2)

plot_ssa <-  
  ggplot(output_for_plot,
         aes(BINMID_LOG, DENS_LOG) ) +
  geom_point ( alpha = 1/2, size=3 ) +
  ylim (1 , 22) +
  xlim (-35, 5) +
  facet_grid ( year ~ lake ) +
  scale_color_manual(values = color_custom2, name = 'Season', labels = c('summer', 'spring')) +
  
  labs ( x = expression ( paste ( "Log"[2], " Wet Weight Biomass (g)" ) ), 
         y = expression ( paste ( "Log"[2], " Abundance (Individuals/m"^2,")" ) ) ) +
  
  theme_bw() + 
  theme ( panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          strip.text.x = element_text(size = 12 ),
          strip.text.y = element_text(size = 12 ) )

plot_ssa # take a first look at plot 

#### Size Spectrum analysis of all lakes with slopes ####
# Output graphs with linear fits # 
library(ggplot2)

# Output graphs with linear fits # 
# Reference - no removal # South Twin, Storm
ref_col_18 = rgb(91, 83, 147, max = 255, alpha = 100) 
ref_col_19 = rgb(91, 83, 147, max = 255, alpha = 180)
ref_col_20 = rgb(91, 83, 147, max = 255, alpha = 255)

# removal, removal, null # Center, Five Island 
rrn_col_18 = rgb(37, 111, 92, max = 255, alpha = 100) 
rrn_col_19 = rgb(37, 111, 92, max = 255, alpha = 180)
rrn_col_20 = rgb(37, 111, 92, max = 255, alpha = 255)

# null, removal, removal # North Twin, Silver
nrr_col_18 = rgb(77, 77, 77, max = 255, alpha = 100) 
nrr_col_19 = rgb(77, 77, 77, max = 255, alpha = 180)
nrr_col_20 = rgb(77, 77, 77, max = 255, alpha = 255)

# transparent 
transparent = rgb(255,255,255, max=255, alpha = 0)

options(repr.plot.width = 4, repr.plot.height = 2)
windows(width = 9, height = 4)
plot_ssa2 = 
  ggplot(output_for_plot, 
         aes(BINMID_LOG, DENS_LOG, shape = DATAUSEDINFIT, color = lake)) + 
  geom_point(size = 1.5) + 
  scale_shape_manual(values = c(1,19)) +
  ylim (2, 22) +
  xlim (-35, 5) +
  facet_grid(year ~ lake) + 
  labs( x = expression ( paste ( 'Log'[2], 'Dry Weight Biomass (g)') ), 
        y = expression ( paste ( 'Log'[2], 'Abundance (Individuals/m'^2,')' ) ) ) +
  scale_color_manual(values = c(ref_col_20, ref_col_18,
                                rrn_col_20, rrn_col_18,
                                nrr_col_20, nrr_col_18)) +
  theme_bw() + 
  theme ( panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          strip.text.x = element_text(size = 10, face = 'bold.italic' ),
          strip.text.y = element_text(size = 10, face = 'bold.italic' ) ) +
  geom_smooth( data = output_for_plot %>% filter(DATAUSEDINFIT == 'used') %>% subset ( fitmax & fitmin), 
               method = lm, se = FALSE)
#+ 
# geom_label(data = output_for_plot, aes(x=-7, y=PLACEMENT, label = format ( round( slope, 2 ), 2) ), show.legend = FALSE)
plot_ssa2 # Supplemental figure of fits 

# Pluck out one plot #============================== (Figure 2)
windows(height = 4, width = 5)
examp = output_for_plot %>% 
  filter(lake == 'Storm' & year == 2019)
examp

examp_mod = lm(DENS_LOG~BINMID_LOG, data = examp)
examp_mod
plot(DENS_LOG~BINMID_LOG, data = examp, pch = 21, ylab = '', 
     xlab = '', 
     bg = ref_col_20, col = 'black', cex = 1.1, ylim = c(2, 22), xlim = c(-32, -5))
mtext(expression ( paste ('Log'[2]~'Abundance (Individuals m'^-2~')' ) ), side =2, line = 2)
mtext(expression ( paste ('Log'[2], 'Dry Weight Biomass (g)')), side = 1, line = 2)
abline(examp_mod,lwd = 2)
text(-8.2, 21, 'Slope = -0.45')
points(-18.41504, 12.16517, pch = 23, bg = 'white', col = 'black', cex = 2, lwd = 4)
text(-9.55, 19, 'Height = ')
points(-5.9, 19, pch = 23, bg = 'white', col = 'black', cex = 2, lwd = 4)


# slope x height.overall data - unique # 
fit.data = output_for_plot %>% 
  select(year, lake, n, slope, slp_se, slp_l95ci, slp_u95ci, 
         intcpt, int_se, int_l95ci, int_u95CI, height, height.l95ci, height.u95ci, height.overall) %>% 
  distinct() %>% 
  arrange(lake, year)
fit.data

# Create datasets to use in Step 3 ---------------------------------
fits_dat = fits.yr.lm
sizespec = short.output
