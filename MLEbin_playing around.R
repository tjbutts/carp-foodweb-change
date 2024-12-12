# MLEbin - Playing around with it # 

library(sizeSpectra)

set.seed(42)
x = rPLB(n = 1000, b = -2, xmin = 1, xmax = 1000)
x

# Bin this data as I do and then enter the breaks as a vector 

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
log2bins.list = log2bins_butts(x)
log2bins.list

binbreaks.max = log2bins.list$binVals$binMax
binbreaks.max 

binbreaks.min = log2bins.list$binVals$binMin
binbreaks.min

x.binned = binData(x, binBreaks = c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
head(x.binned$indiv) # Individual x values and which bin they are assigned to
x.binned$binVals

num.bins = nrow(x.binned$binVals)

# bin breaks are the minima plus the max of the final bin # 
binBreaks = c(dplyr::pull(x.binned$binVals, binMin), 
              dplyr::pull(x.binned$binVals, binMax)[num.bins])
binBreaks

binCounts = dplyr::pull(x.binned$binVals, binCount)
binCounts

MLEbin.res = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = -1.5,
                      w = binBreaks, 
                      d = binCounts,
                      J = length(binCounts), # = num.bins
                      vecDiff = 1) # increase this if hit a bound 

#> Warning in nlm(f = negLL.fn, p = p, ...): NA/Inf replaced by maximum positive
#> value

# fig.width is 0.67 * fig.height (which is 8)
ISD_bin_plot_nonoverlapping(binBreaks = binBreaks,
                            binCounts = binCounts,
                            b.MLE = MLEbin.res$MLE,
                            b.confMin = MLEbin.res$conf[1],
                            b.confMax = MLEbin.res$conf[2])


# Normalized Biomass Spectrum # =======================
library(sizeSpectra)
library(tidyverse)

set.seed(42)
x <- rPLB(n = 1000,
          b = -2,
          xmin = 1,
          xmax = 1000)

x.binned <- binData(x,
                    binWidth = "2k")  # 1, 2, 5 or "2k"
head(x.binned$indiv)     # Individual x values and which bin they are assigned to
x.binned$binVals         # Bins and the counts in each bin (with extra columns

num.bins <- nrow(x.binned$binVals)

# bin breaks are the minima plus the max of the final bin:
binBreaks <- c(dplyr::pull(x.binned$binVals, binMin),
               dplyr::pull(x.binned$binVals, binMax)[num.bins])

binCounts <- dplyr::pull(x.binned$binVals, binCount)

MLEbin.res <-  calcLike(negLL.fn = negLL.PLB.binned,
                        p = -1.5,
                        w = binBreaks,
                        d = binCounts,
                        J = length(binCounts),   # = num.bins
                        vecDiff = 1)             # increase this if hit a bound


# Normalized Biomass Bin # 
LBN_bin_plot(binValsTibble = x.binned$binVals, 
             b.MLE = MLEbin.res$MLE,
             b.confMin = MLEbin.res$conf[1],
             b.confMax = MLEbin.res$conf[2],
             log.xy = '',
             plot.binned.fitted = FALSE)
# Hard to see - log the x axis # 
LBN_bin_plot(binValsTibble = x.binned$binVals, 
             b.MLE = MLEbin.res$MLE,
             b.confMin = MLEbin.res$conf[1],
             b.confMax = MLEbin.res$conf[2], 
             leg.text = '(b)', 
             log.xy = 'x',
             plot.binned.fitted = FALSE)
# Not perfect - log both axes # 
LBN_bin_plot(binValsTibble = x.binned$binVals, 
             b.MLE = MLEbin.res$MLE,
             b.confMin = MLEbin.res$conf[1],
             b.confMax = MLEbin.res$conf[2], 
             leg.text = '(c)', 
             log.xy = 'xy',
             plot.binned.fitted = FALSE)
# Since data are binned - straight red lines not most intuitive thing to plot. Estimate the 
  # expected normailzed biomass in each bin based on the MLE for b and the conf for b
LBN_bin_plot(binValsTibble = x.binned$binVals, 
             b.MLE = MLEbin.res$MLE,
             b.confMin = MLEbin.res$conf[1],
             b.confMax = MLEbin.res$conf[2], 
             leg.text = '(c)', 
             log.xy = 'xy',
             plot.binned.fitted = TRUE)

# nOW run this with the carp data #============================

library(sizeSpectra)
library(tidyverse)

# Create the 'output' object to be used for the MLE estimation 

# Load in SSA dataset #===========================
output = read_csv('SSA start_zoop.miv-biomass.csv')
output

# Replace lake number with lake name #
levels(output$lake) = c(levels(output$lake), 'Blue', 'Center', 
                         'Five Island', 'North Twin', 
                         'Silver', 'Storm', 'South Twin')

output$lake[output$lake == '12'] <- 'Blue'
output$lake[output$lake == '19'] <- 'Center' 
output$lake[output$lake == '36'] <- 'Five Island' 
output$lake[output$lake == '90'] <- 'North Twin'
output$lake[output$lake == '105'] <- 'Silver'
output$lake[output$lake == '113'] <- 'Storm'
output$lake[output$lake == '406'] <- 'South Twin'
output

# Remove Blue Lake - geologically distinct, almost no BMB # 
output = filter(output, lake != 'Blue')
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

# Calculate for each lake and each year ================================

### Storm Lake ##====================

#### 2018 ##=======================
# Test with Carp Lakes Data # 
storm18.test = output %>% # output from Step 2, Line 99
  filter(lake == 'Storm' & year == 2018)
storm18.test # now extract the 'binned' data 

bincharac.test = storm18.test %>% 
  group_by(BINMID) %>%
  reframe(binCount = mean(density.areal, na.rm = T)) 
bincharac.test

storm18.trim = storm18.test %>%
  select(BINMID, BINMIN, BINMAX) %>%
  mutate(BINWIDTH = BINMAX - BINMIN)

join1 = left_join(bincharac.test, storm18.trim, by = 'BINMID')
join1

storm18.prepped = join1[!duplicated(join1), ] %>% 
  rename(binMid = BINMID, 
         binMin = BINMIN, 
         binMax = BINMAX, 
         binWidth = BINWIDTH) %>%
  select(binMid, binMin, binMax, binWidth, binCount)
storm18.prepped


# now run the MLE and plotting for this data (see if it works) 
num.bins = nrow(storm18.prepped)
binBreaks = c(dplyr::pull(storm18.prepped, binMin), 
              dplyr::pull(storm18.prepped, binMax)[num.bins])
binCounts = dplyr::pull(storm18.prepped, binCount)

lower_bound = -10
upper_bound = 10

MLEbin.storm18 = calcLike(negLL.fn = negLL.PLB.binned, 
                      p = 1, 
                      w = binBreaks, 
                      d = binCounts, 
                      J = length(binCounts), # = numbins 
                      vecDiff = 0.1,  # increase this if hit a bound 
                      vecInc = 0.01, 
                      lower = lower_bound, 
                      upper = upper_bound)

binBreaks_scaled = binBreaks / max(binBreaks) 
binCounts_scaled = binCounts / max(binCounts)

MLEbin.storm18 = calcLike(negLL.fn = negLL.PLB.binned, 
                          p = -1, 
                          w = binBreaks_scaled,
                          d = binCounts_scaled, 
                          J = length(binCounts_scaled), 
                          vecDiff = 1, 
                          vecInc = 0.01)


MLEbin.storm18$MLE
MLEbin.storm18$conf[1]
MLEbin.storm18$conf[2]

ISD_bin_plot_nonoverlapping(yBig.inc =1000000, # Will have to adjust this for each lake 
                            binBreaks = binBreaks, 
                            binCounts = binCounts, 
                            b.MLE = MLEbin.storm18$MLE,
                            b.confMin = MLEbin.storm18$conf[1],
                            b.confMax = MLEbin.storm18$conf[2])

# FIGURED OUT CONF INTERVALS FUCK YA # 

# Reasoning: 
  # The values of binBreaks or binCounts varied across orders of magnitude which was causing 
  # issues with the optimization process. I thus rescaled the binBreaks and binCounts by their maximum values 
  # This gives the optimizer more room to find valid results 

